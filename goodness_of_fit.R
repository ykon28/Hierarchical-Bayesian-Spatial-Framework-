library(INLA)
library(ggplot2)

# Extract fitted values for the observation indices
index_obs <- inla.stack.index(stack_obs_elev, tag = "obs")$data
fitted_means <- model_fit_elev$summary.fitted.values$mean[index_obs]
observed_y   <- station_summary$mean_pm25
residuals    <- observed_y - fitted_means

diag_df <- data.frame(
  observed = observed_y,
  fitted   = fitted_means,
  residual = residuals,
  longitude = station_summary$longitude,
  latitude  = station_summary$latitude
)

# Compute RMSE and R-squared
rmse_val <- sqrt(mean(residuals^2))
r2_val   <- cor(observed_y, fitted_means)^2

# Plot 1: Observed vs Fitted
p_fit <- ggplot(diag_df, aes(x = fitted, y = observed)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  theme_minimal() +
  labs(
    title = "(A) Model Goodness-of-Fit",
    subtitle = paste0("RMSE = ", round(rmse_val, 2), " µg/m³ | R² = ", round(r2_val, 2)),
    x = "Posterior Fitted Mean (µg/m³)",
    y = "Observed Station PM2.5 (µg/m³)"
  )

p_fit