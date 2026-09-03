library(fmesher)
library(sf)
library(ggplot2)
library(patchwork)

# Step 1: Create a regular prediction grid over Greece (0.05 degree resolution)
grid_sf <- st_make_grid(greece_boundary, cellsize = 0.05, what = "centers")
grid_sf <- st_intersection(grid_sf, greece_boundary) # Keep only points inside Greece
grid_coords <- st_coordinates(grid_sf)

# Step 2: Modern fmesher Projection using fm_evaluate()
# Evaluate Spatial Random Field Mean and Standard Deviation on the Grid
spatial_mean <- fm_evaluate(
  mesh = mesh, 
  loc = grid_coords, 
  field = model_fit$summary.random$spatial.field$mean
)

spatial_sd <- fm_evaluate(
  mesh = mesh, 
  loc = grid_coords, 
  field = model_fit$summary.random$spatial.field$sd
)

# Step 3: Combine Intercept + Spatial Random Field for Total PM2.5 Estimates
intercept_val <- model_fit$summary.fixed["intercept", "mean"]

pred_df <- data.frame(
  longitude = grid_coords[, 1],
  latitude  = grid_coords[, 2],
  pm25_pred = intercept_val + spatial_mean, # Total Predicted Mean PM2.5
  pm25_sd   = spatial_sd                     # Posterior Uncertainty (SD)
)

# Step 4: Map 1 - Posterior Predicted Mean PM2.5 Concentration
p1 <- ggplot(pred_df) +
  geom_tile(aes(x = longitude, y = latitude, fill = pm25_pred)) +
  geom_sf(data = greece_map, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = stations_sf, color = "red", size = 2) +
  scale_fill_viridis_c(option = "magma", name = expression(PM[2.5]~(mu*g/m^3))) +
  theme_minimal() +
  labs(
    title = "(A) Bayesian Spatial Interpolation",
    subtitle = "Posterior Mean PM2.5 Concentration",
    x = "Longitude", y = "Latitude"
  )

# Step 5: Map 2 - Posterior Standard Deviation (Uncertainty Map)
p2 <- ggplot(pred_df) +
  geom_tile(aes(x = longitude, y = latitude, fill = pm25_sd)) +
  geom_sf(data = greece_map, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf(data = stations_sf, color = "red", size = 2) +
  scale_fill_viridis_c(option = "mako", name = "Uncertainty (SD)") +
  theme_minimal() +
  labs(
    title = "(B) Predictive Uncertainty Quantification",
    subtitle = "Posterior Standard Deviation (Uncertainty)",
    x = "Longitude", y = "Latitude"
  )

# Combine both maps side-by-side
p1 + p2
