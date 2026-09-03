library(fmesher)
library(sf)
library(ggplot2)
library(rnaturalearth)

# Step 1: Ensure greece_map has explicit EPSG:4326 (WGS84) CRS
greece_map <- ne_countries(scale = "medium", country = "Greece", returnclass = "sf")
st_crs(greece_map) <- 4326

# Step 2: Unify boundary and re-assign CRS explicitly
greece_boundary <- st_union(greece_map)
st_crs(greece_boundary) <- 4326

# Step 3: Convert station points to an sf object with CRS assigned
stations_sf <- st_as_sf(
  station_summary, 
  coords = c("longitude", "latitude"), 
  crs = 4326
)

# Step 4: Construct the Mesh with explicit CRS passed to fmesher
mesh <- fm_mesh_2d_inla(
  loc = stations_sf,
  boundary = greece_boundary,
  max.edge = c(0.3, 1.0), # Edge lengths in degrees
  cutoff = 0.08,          # Minimum distance between nodes
  crs = fm_crs(4326)      # Assign WGS84 explicitly to the mesh
)

cat("Spatial Mesh created successfully with", mesh$n, "nodes.\n")

# Step 5: Plot using geom_sf and geom_fm
ggplot() +
  geom_sf(data = greece_map, fill = "gray95", color = "gray60") +
  geom_fm(data = mesh, color = "steelblue", alpha = 0.35) +
  geom_sf(data = stations_sf, color = "red", size = 2.5) +
  theme_minimal() +
  labs(
    title = "Bayesian SPDE Spatial Mesh over Greece",
    subtitle = "Triangulated Domain (fmesher) with EPSG:4326 CRS",
    x = "Longitude", y = "Latitude"
  )
