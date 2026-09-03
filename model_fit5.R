library(INLA)

# Step 1: Define Penalized Complexity (PC) Priors for the Spatial Field
# Prior: Probability that spatial range < 1.0 degree is 1%
# Prior: Probability that marginal variance > 20 ug/m3 is 1%
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(1.0, 0.01),  
  prior.sigma = c(20, 0.01)
)

# Step 2: Create Spatial Index
spatial_index <- inla.spde.make.index("spatial.field", n.spde = spde$n.spde)

# Step 3: Projection Matrix (A matrix) mapping mesh nodes to station locations
coords_matrix <- st_coordinates(stations_sf)
A_obs <- inla.spde.make.A(mesh = mesh, loc = coords_matrix)

# Step 4: Build INLA Stack for Observation Data
# We model average PM2.5 per station as a function of Intercept + Spatial Random Field
stack_obs <- inla.stack(
  data = list(y = station_summary$mean_pm25),
  A = list(A_obs, 1),
  effects = list(
    spatial.field = spatial_index$spatial.field,
    intercept = rep(1, nrow(station_summary))
  ),
  tag = "obs"
)

# Step 5: Fit the Bayesian Spatial Model
formula <- y ~ -1 + intercept + f(spatial.field, model = spde)

model_fit <- inla(
  formula,
  data = inla.stack.data(stack_obs),
  control.predictor = list(A = inla.stack.A(stack_obs), compute = TRUE),
  control.family = list(hyper = list(prec = list(prior = "pc.prec", param = c(10, 0.01)))),
  verbose = TRUE
)

cat("\n--- Model Fit Complete! ---\n")
summary(model_fit)