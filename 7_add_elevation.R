# Install elevatr if not already installed
if (!requireNamespace("elevatr", quietly = TRUE)) {
  install.packages("elevatr")
}

library(elevatr)
library(sf)
library(dplyr)
library(INLA)

# Extract elevation for station locations (AWS terrain raster tile server)
stations_elev <- get_elev_point(stations_sf, src = "aws")

# Scale elevation to kilometers (km) for numerical stability in Bayesian regression
# (1 km increase in elevation is easier to interpret than 1 meter)
stations_elev$elev_km <- stations_elev$elevation / 1000

cat("Extracted station elevations (in meters):\n")
summary(stations_elev$elevation)
