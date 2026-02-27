# -------------------------------------------------------------------------
# TGV EXPRESS REVENUE MANAGEMENT: UNCONSTRAINING BOOKING DATA
#
# Data Source: TGV_Express_Forecast-Assign2.xlsx
# Purpose: Unconstrain booking observations affected by inventory closure
# -------------------------------------------------------------------------


# SECTION 0: PREREQUISITES (SETUP & DATA LOADING) ------------------------
# -------------------------------------------------------------------------
# This section loads required libraries and imports the TGV booking data
# from the Excel file.

# 0.1: Load Libraries
library(tidyverse)
library(readxl)

# 0.2: Load and Prepare Data
raw_sales_data <- read_excel("TGV_Express_Forecast-Assign2.xls", 
                             sheet = "Base Data", 
                             skip = 2)

# Clean column names for easier reference
colnames(raw_sales_data)[colnames(raw_sales_data) == "Day of departure"] <- "day_departure"
colnames(raw_sales_data)[colnames(raw_sales_data) == "Actual travel Nos"] <- "actual_pax"
colnames(raw_sales_data)[colnames(raw_sales_data) == "Denied Boarded at London"] <- "denied_boarding"

# Remove completely empty columns
tgv_data <- raw_sales_data %>%
  select(where(~!all(is.na(.))))

# Convert date column
tgv_data$DEPARTURE_DATE <- as.Date(tgv_data$DEPARTURE_DATE)

# Define booking period columns (as numbers, not with X prefix)
booking_periods <- c("180", "150", "120", "90", "75", "60", 
                     "45", "30", "15", "10", "7", "6", "3", "2", "1")

# Convert booking columns to numeric
for(col in booking_periods) {
  if(col %in% names(tgv_data)) {
    tgv_data[[col]] <- as.numeric(tgv_data[[col]])
  }
}

# Convert capacity to numeric
tgv_data$Capacity <- as.numeric(tgv_data$Capacity)


# -------------------------------------------------------------------------
# SECTION 1: IDENTIFY CONSTRAINED OBSERVATIONS
# -------------------------------------------------------------------------

# Constraints occur when:
# 1. Bookings >= Capacity (inventory closed - pink cells in Excel)
# 2. Same value as next booking period (frozen bookings)

# 1.1: Flag observations at or above capacity
for(col in booking_periods) {
  if(col %in% names(tgv_data)) {
    constraint_flag <- paste0("is_constrained_", col)
    
    tgv_data[[constraint_flag]] <- if_else(
      !is.na(tgv_data[[col]]) & tgv_data[[col]] >= tgv_data$Capacity,
      TRUE,
      FALSE
    )
  }
}

# 1.2: Additional check for frozen bookings (consecutive periods with same value)
for(i in 1:(length(booking_periods) - 1)) {
  current_period <- booking_periods[i]
  next_period <- booking_periods[i + 1]
  
  if(current_period %in% names(tgv_data) & next_period %in% names(tgv_data)) {
    constraint_flag <- paste0("is_constrained_", current_period)
    
    # Flag if current period equals next period
    same_as_next <- !is.na(tgv_data[[current_period]]) & 
      !is.na(tgv_data[[next_period]]) &
      tgv_data[[current_period]] == tgv_data[[next_period]]
    
    tgv_data[[constraint_flag]] <- tgv_data[[constraint_flag]] | same_as_next
  }
}

# 1.3: Display constraint summary
cat("CONSTRAINT IDENTIFICATION COMPLETE\n")
cat("===================================\n")
for(col in booking_periods) {
  if(col %in% names(tgv_data)) {
    n_constrained <- sum(tgv_data[[paste0("is_constrained_", col)]], na.rm = TRUE)
    if(n_constrained > 0) {
      cat(sprintf("  %s days prior: %d constrained observations\n", 
                  col, n_constrained))
    }
  }
}


# -------------------------------------------------------------------------
# SECTION 2: DEFINE GROWTH RATES
# -------------------------------------------------------------------------

# Expected pickup (growth) rates by days prior to departure
growth_rates <- tibble(
  days_prior = c(180, 150, 120, 90, 75, 60, 45, 30, 15, 10, 7, 6, 3, 2, 1),
  growth_rate = c(28.31, 34.59, 42.93, 18.78, 18.05, 20.38, 15.44, 
                  3.59, 4.98, 13.86, 2.89, 1.56, 3.00, 1.00, 1.00)
)


# -------------------------------------------------------------------------
# SECTION 3: UNCONSTRAIN BOOKING DATA
# -------------------------------------------------------------------------

# Formula: Unconstrained_Value = Next_Period_Value * (1 + Growth_Rate/100)
# Work backwards in time from 180 days to 1 day before departure

# 3.1: Initialize unconstrained columns with original values
for(col in booking_periods) {
  if(col %in% names(tgv_data)) {
    unconstrained_col <- paste0("unc_", col)
    tgv_data[[unconstrained_col]] <- tgv_data[[col]]
  }
}

# 3.2: Apply unconstraining algorithm
cat("\nAPPLYING UNCONSTRAINING ALGORITHM\n")
cat("=================================\n")

n_unconstrained <- 0

for(row_idx in 1:nrow(tgv_data)) {
  
  # Work backwards through booking periods
  for(period_idx in 1:(length(booking_periods) - 1)) {
    
    current_period <- booking_periods[period_idx]
    next_period <- booking_periods[period_idx + 1]
    
    if(!(current_period %in% names(tgv_data)) | !(next_period %in% names(tgv_data))) next
    
    # Check if current period is constrained
    constraint_flag <- paste0("is_constrained_", current_period)
    is_constrained <- tgv_data[[constraint_flag]][row_idx]
    
    if(!is.na(is_constrained) && is_constrained) {
      
      # Get next period unconstrained value
      next_unc <- paste0("unc_", next_period)
      next_value <- tgv_data[[next_unc]][row_idx]
      
      # Get growth rate for current period
      current_days <- as.numeric(current_period)
      growth_rate <- growth_rates %>%
        filter(days_prior == current_days) %>%
        pull(growth_rate)
      
      # Apply unconstraining formula
      if(!is.na(next_value) && length(growth_rate) > 0) {
        current_unc <- paste0("unc_", current_period)
        tgv_data[[current_unc]][row_idx] <- next_value * (1 + growth_rate / 100)
        
        n_unconstrained <- n_unconstrained + 1
        
        # Display first few examples
        if(n_unconstrained <= 5) {
          cat(sprintf("Row %d, %s days: %.0f -> %.2f (next period: %.0f, rate: %.2f%%)\n",
                      row_idx, 
                      current_period,
                      tgv_data[[current_period]][row_idx],
                      tgv_data[[current_unc]][row_idx],
                      next_value,
                      growth_rate))
        }
      }
    }
  }
}

cat(sprintf("\nTotal values unconstrained: %d\n", n_unconstrained))


# -------------------------------------------------------------------------
# SECTION 4: CREATE FINAL UNCONSTRAINED DATASET
# -------------------------------------------------------------------------

# 4.1: Select and rename unconstrained columns
unconstrained_data <- tgv_data %>%
  select(Train, ORGN, DSTN, DEPARTURE_DATE, Capacity,
         starts_with("unc_"), actual_pax, denied_boarding)

# Remove "unc_" prefix from column names
names(unconstrained_data) <- gsub("^unc_", "", names(unconstrained_data))

# 4.2: Display sample of unconstrained data
cat("\nSAMPLE UNCONSTRAINED DATA\n")
cat("=========================\n")
unconstrained_data %>%
  select(DEPARTURE_DATE, Capacity, `180`, `150`, `120`, `90`, `60`, `30`) %>%
  head(10) %>%
  print()

# 4.3: Save unconstrained data
write.csv(unconstrained_data, "unconstrained_bookings.csv", row.names = FALSE)
save(unconstrained_data, file = "unconstrained_bookings.RData")

cat("\nDATA SAVED\n")

cat("\n=== UNCONSTRAINING COMPLETE ===\n")


# -------------------------------------------------------------------------
# SECTION 5: PERIOD-TO-PERIOD GROWTH RATE FORECASTING
# -------------------------------------------------------------------------

# Purpose: Forecast future booking values by calculating historical growth rates
# between consecutive booking periods (180→150, 150→120, etc.) 
# and applying them sequentially to fill future values

cat("\n\nPERIOD-TO-PERIOD GROWTH RATE FORECASTING\n")
cat("==========================================\n")

# 5.1: Define consecutive booking period pairs
booking_period_pairs <- tibble(
  from_period = c("180", "150", "120", "90", "75", "60", "45", "30", "15", "10", "7", "6", "3", "2"),
  to_period = c("150", "120", "90", "75", "60", "45", "30", "15", "10", "7", "6", "3", "2", "1")
)

cat("\nBooking Period Transitions:\n")
print(booking_period_pairs)


# -------------------------------------------------------------------------
# SECTION 6: COMPUTE HISTORICAL GROWTH RATES
# -------------------------------------------------------------------------

# Calculate growth rate = (to_value - from_value) / from_value * 100

cat("\n\nCOMPUTING GROWTH RATES\n")
cat("=======================\n")

growth_rate_stats <- tibble(
  transition = character(),
  from_period = character(),
  to_period = character(),
  mean_growth = numeric(),
  median_growth = numeric(),
  n_observations = integer()
)

for(i in 1:nrow(booking_period_pairs)) {
  
  from_col <- booking_period_pairs$from_period[i]
  to_col <- booking_period_pairs$to_period[i]
  transition_name <- paste0(from_col, "→", to_col)
  
  # Calculate growth rates from unconstrained data
  growth_rates <- unconstrained_data %>%
    filter(!is.na(.data[[from_col]]) & !is.na(.data[[to_col]]) & .data[[from_col]] > 0) %>%
    mutate(
      growth_rate = (.data[[to_col]] - .data[[from_col]]) / .data[[from_col]] * 100
    ) %>%
    pull(growth_rate)
  
  if(length(growth_rates) > 0) {
    growth_rate_stats <- growth_rate_stats %>%
      bind_rows(tibble(
        transition = transition_name,
        from_period = from_col,
        to_period = to_col,
        mean_growth = mean(growth_rates, na.rm = TRUE),
        median_growth = median(growth_rates, na.rm = TRUE),
        n_observations = length(growth_rates)
      ))
    
    cat(sprintf("  %s: Mean=%.2f%%, Median=%.2f%%, N=%d\n", 
                transition_name, 
                mean(growth_rates, na.rm = TRUE),
                median(growth_rates, na.rm = TRUE),
                length(growth_rates)))
  }
}


# -------------------------------------------------------------------------
# SECTION 7: GENERATE FORECASTS SEQUENTIALLY
# -------------------------------------------------------------------------

# Start with baseline at 180 days, then apply growth rates sequentially

cat("\n\nGENERATING FORECASTS\n")
cat("====================\n")

# Determine forecast dates
last_historical_date <- max(unconstrained_data$DEPARTURE_DATE, na.rm = TRUE)
forecast_horizon_days <- 60

future_dates <- seq(last_historical_date + 1, 
                    last_historical_date + forecast_horizon_days, 
                    by = "day")

cat(sprintf("Forecasting %d days from %s to %s\n", 
            forecast_horizon_days,
            as.character(future_dates[1]),
            as.character(future_dates[length(future_dates)])))

# Initialize forecast dataframe
forecast_data <- tibble(
  DEPARTURE_DATE = future_dates,
  forecast_180 = NA_real_,
  forecast_150 = NA_real_,
  forecast_120 = NA_real_,
  forecast_90 = NA_real_,
  forecast_75 = NA_real_,
  forecast_60 = NA_real_,
  forecast_45 = NA_real_,
  forecast_30 = NA_real_,
  forecast_15 = NA_real_,
  forecast_10 = NA_real_,
  forecast_7 = NA_real_,
  forecast_6 = NA_real_,
  forecast_3 = NA_real_,
  forecast_2 = NA_real_,
  forecast_1 = NA_real_
)

# Calculate baseline from last 30 days
baseline_180 <- unconstrained_data %>%
  arrange(desc(DEPARTURE_DATE)) %>%
  head(30) %>%
  summarise(avg = mean(`180`, na.rm = TRUE)) %>%
  pull(avg)

cat(sprintf("Baseline (180 days): %.2f bookings\n", baseline_180))

# Apply growth rates sequentially for each forecast row
for(row_idx in 1:nrow(forecast_data)) {
  
  # Start with baseline
  forecast_data$forecast_180[row_idx] <- baseline_180
  
  # Apply each transition using median growth rate
  for(i in 1:nrow(booking_period_pairs)) {
    from_col <- paste0("forecast_", booking_period_pairs$from_period[i])
    to_col <- paste0("forecast_", booking_period_pairs$to_period[i])
    
    # Get median growth rate for this transition
    growth_rate <- growth_rate_stats %>%
      filter(from_period == booking_period_pairs$from_period[i], 
             to_period == booking_period_pairs$to_period[i]) %>%
      pull(median_growth)
    
    if(length(growth_rate) > 0 && !is.na(forecast_data[[from_col]][row_idx])) {
      # Apply formula: next = current * (1 + growth/100)
      forecast_data[[to_col]][row_idx] <- forecast_data[[from_col]][row_idx] * (1 + growth_rate/100)
    }
  }
}

cat("\nSample forecasts (first 10 days):\n")
forecast_data %>%
  select(DEPARTURE_DATE, forecast_180, forecast_120, forecast_60, forecast_30, forecast_7, forecast_1) %>%
  head(10) %>%
  print()


# -------------------------------------------------------------------------
# SECTION 8: COMBINE HISTORICAL AND FORECAST DATA
# -------------------------------------------------------------------------

# Prepare for plotting
historical_for_plot <- unconstrained_data %>%
  select(DEPARTURE_DATE, `180`, `150`, `120`, `90`, `75`, `60`, `45`, `30`, `15`, `10`, `7`, `6`, `3`, `2`, `1`) %>%
  rename_with(~paste0("days_", .), -DEPARTURE_DATE) %>%
  mutate(data_type = "Historical (Unconstrained)")

forecast_for_plot <- forecast_data %>%
  rename_with(~gsub("forecast_", "days_", .), -DEPARTURE_DATE) %>%
  mutate(data_type = "Forecasted (Growth Rate)")

combined_data <- bind_rows(historical_for_plot, forecast_for_plot)


# -------------------------------------------------------------------------
# SECTION 9: VISUALIZATION
# -------------------------------------------------------------------------

cat("\n\nCREATING PLOTS\n")
cat("==============\n")

# Plot 180 days
plot_180 <- combined_data %>%
  ggplot(aes(x = DEPARTURE_DATE, y = days_180, color = data_type)) +
  geom_line(size = 0.8) +
  geom_point(alpha = 0.4, size = 1.5) +
  scale_color_manual(values = c("Historical (Unconstrained)" = "#1f77b4", 
                                "Forecasted (Growth Rate)" = "#ff7f0e")) +
  labs(
    title = "Booking Forecast: 180 Days Prior",
    x = "Departure Date",
    y = "Bookings",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_180)
ggsave("forecast_180_days.png", plot_180, width = 12, height = 6, dpi = 300)

# Plot 120 days
plot_120 <- combined_data %>%
  ggplot(aes(x = DEPARTURE_DATE, y = days_120, color = data_type)) +
  geom_line(size = 0.8) +
  geom_point(alpha = 0.4, size = 1.5) +
  scale_color_manual(values = c("Historical (Unconstrained)" = "#1f77b4", 
                                "Forecasted (Growth Rate)" = "#ff7f0e")) +
  labs(
    title = "Booking Forecast: 120 Days Prior",
    x = "Departure Date",
    y = "Bookings",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_120)
ggsave("forecast_120_days.png", plot_120, width = 12, height = 6, dpi = 300)

# Plot 60 days
plot_60 <- combined_data %>%
  ggplot(aes(x = DEPARTURE_DATE, y = days_60, color = data_type)) +
  geom_line(size = 0.8) +
  geom_point(alpha = 0.4, size = 1.5) +
  scale_color_manual(values = c("Historical (Unconstrained)" = "#1f77b4", 
                                "Forecasted (Growth Rate)" = "#ff7f0e")) +
  labs(
    title = "Booking Forecast: 60 Days Prior",
    x = "Departure Date",
    y = "Bookings",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_60)
ggsave("forecast_60_days.png", plot_60, width = 12, height = 6, dpi = 300)

# Combined plot
combined_data_long <- combined_data %>%
  select(DEPARTURE_DATE, data_type, days_180, days_120, days_60, days_30, days_7) %>%
  pivot_longer(
    cols = starts_with("days_"),
    names_to = "booking_window",
    values_to = "bookings"
  ) %>%
  mutate(
    booking_window = factor(
      booking_window,
      levels = c("days_180", "days_120", "days_60", "days_30", "days_7"),
      labels = c("180 Days", "120 Days", "60 Days", "30 Days", "7 Days")
    )
  )

plot_combined <- combined_data_long %>%
  ggplot(aes(x = DEPARTURE_DATE, y = bookings, color = data_type)) +
  geom_line(size = 0.7) +
  geom_point(alpha = 0.3, size = 1) +
  facet_wrap(~ booking_window, ncol = 2, scales = "free_y") +
  scale_color_manual(values = c("Historical (Unconstrained)" = "#1f77b4", 
                                "Forecasted (Growth Rate)" = "#ff7f0e")) +
  labs(
    title = "Period-to-Period Growth Rate Forecasting",
    subtitle = "Historical vs Forecasted Bookings Across Booking Windows",
    x = "Departure Date",
    y = "Bookings",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(plot_combined)
ggsave("forecast_combined.png", plot_combined, width = 14, height = 10, dpi = 300)

cat("\nPlots saved:\n")
cat("  - forecast_180_days.png\n")
cat("  - forecast_120_days.png\n")
cat("  - forecast_60_days.png\n")
cat("  - forecast_combined.png\n")


# -------------------------------------------------------------------------
# SECTION 10: SAVE RESULTS
# -------------------------------------------------------------------------

write.csv(forecast_data, "growth_rate_forecast.csv", row.names = FALSE)
write.csv(growth_rate_stats, "growth_rate_statistics.csv", row.names = FALSE)

save(forecast_data, growth_rate_stats, combined_data,
     file = "forecast_results.RData")

cat("\n\nRESULTS SAVED\n")
cat("=============\n")
cat("  - growth_rate_forecast.csv\n")
cat("  - growth_rate_statistics.csv\n")
cat("  - forecast_results.RData\n")

cat("\n=== FORECASTING COMPLETE ===\n")