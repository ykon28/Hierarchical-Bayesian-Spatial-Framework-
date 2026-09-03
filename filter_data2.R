library(dplyr)
library(ggplot2)

# Step 1: Filter valid data and remove negative/missing concentrations
clean_obs <- data_gr %>%
  filter(validity == 1) %>%               # Keep valid observations
  filter(!is.na(value) & value >= 0)      # Remove NAs and invalid negative readings

# Step 2: Merge observations with site coordinates (lat/lon)
full_obs <- clean_obs %>%
  left_join(
    sites_gr %>% select(site, site_name, latitude, longitude, site_type),
    by = "site"
  )

# Step 3: Aggregate hourly measurements to DAILY averages per station
daily_pm25 <- full_obs %>%
  mutate(date_only = as.Date(date)) %>%
  group_by(site, site_name, latitude, longitude, site_type, date_only) %>%
  summarise(
    pm25_daily = mean(value, na.rm = TRUE),
    n_hours = n(),                       # Count valid hours per day
    .groups = "drop"
  ) %>%
  filter(n_hours >= 18)                  # Require at least 18 valid hours/day for daily mean

cat("Daily aggregation complete. Total daily observations:", nrow(daily_pm25), "\n")
head(daily_pm25)