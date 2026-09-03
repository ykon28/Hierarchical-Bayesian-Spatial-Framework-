library(saqgetr)
library(dplyr)

# Step 1: Load site metadata
sites <- get_saq_sites()

# Step 2: Filter stations for Greece using country_iso_code ("GR" or "EL")
sites_gr <- sites %>% 
  filter(country_iso_code %in% c("GR", "EL"))

cat("Found", nrow(sites_gr), "Greek stations in saqgetr database.\n")
head(sites_gr[, c("site", "site_name", "country_iso_code", "latitude", "longitude")])

# Step 3: Fetch PM2.5 observations for 2023 (using start and end)
data_gr <- get_saq_observations(
  site = sites_gr$site,
  variable = "pm2.5",
  start = 2023,
  end = 2023,
  verbose = TRUE
)

# Step 4: Preview observations
head(data_gr)
