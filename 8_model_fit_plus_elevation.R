library(INLA)

# Step 1: Define SPDE object WITH constr = TRUE specified inside inla.spde2.pcmatern()
spde <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(1.0, 0.01),  
  prior.sigma = c(20, 0.01),
  constr = TRUE  # <--- Correct place to enable sum-to-zero constraint for SPDE!
)

# Step 2: Re-create Spatial Index and Projection Matrix
spatial_index <- inla.spde.make.index("spatial.field", n.spde = spde$n.spde)
coords_matrix <- st_coordinates(stations_sf)
A_obs <- inla.spde.make.A(mesh = mesh, loc = coords_matrix)

# Step 3: Re-build INLA Stack with Elevation
stack_obs_elev <- inla.stack(
  data = list(y = station_summary$mean_pm25),
  A = list(A_obs, 1),
  effects = list(
    spatial.field = spatial_index$spatial.field,
    list(
      intercept = rep(1, nrow(station_summary)),
      elevation_km = stations_elev$elev_km
    )
  ),
  tag = "obs"
)

# Step 4: Formula WITHOUT constr=TRUE inside f()
formula_elev <- y ~ -1 + intercept + elevation_km + f(spatial.field, model = spde)

# Step 5: Fit the Model via INLA
model_fit_elev <- inla(
  formula_elev,
  data = inla.stack.data(stack_obs_elev),
  control.predictor = list(A = inla.stack.A(stack_obs_elev), compute = TRUE),
  control.fixed = list(
    mean = list(intercept = 15, elevation_km = 0),
    prec = list(intercept = 0.04, elevation_km = 0.01) # Prior: intercept ~ N(15, sd=5)
  ),
  control.family = list(hyper = list(prec = list(prior = "pc.prec", param = c(10, 0.01)))),
  verbose = FALSE
)

# Step 6: Print Summary
summary(model_fit_elev)
