library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)


# Get Greece map outline
greece_map <- ne_countries(scale = "medium", country = "Greece", returnclass = "sf")

# Convert station summary to Spatial sf object
station_summary <- daily_pm25 %>%
  group_by(site, site_name, latitude, longitude, site_type) %>%
  summarise(
    mean_pm25 = mean(pm25_daily, na.rm = TRUE),
    total_days = n(),
    .groups = "drop"
  )

stations_sf <- st_as_sf(station_summary, coords = c("longitude", "latitude"), crs = 4326)

# Plot Spatial Station Distribution
ggplot() +
  geom_sf(data = greece_map, fill = "gray95", color = "black") +
  geom_sf(data = stations_sf, aes(color = mean_pm25, size = total_days), alpha = 0.8) +
  scale_color_viridis_c(option = "magma", name = expression(Mean ~ PM[2.5] ~ (mu*g/m^3))) +
  theme_minimal() +
  labs(
    title = "Air Quality Station Network in Greece (2023)",
    subtitle = "Ground-truth PM2.5 Monitoring Stations",
    x = "Longitude", y = "Latitude"
  )