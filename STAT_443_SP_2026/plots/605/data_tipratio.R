library(arrow)
library(dplyr)
library(ggplot2)
library(lubridate)
  # run once
library(ggcorrplot)              

data_path <- "/Users/sorenbasnet/Desktop/data/2021/par/fhvhv_tripdata_2021-01.parquet"
ds <- open_dataset(data_path)

ds %>% head() %>% collect()

# 1. Identify the Top 10 Routes
top_routes <- sampled %>%
  # Create a readable route name
  mutate(route = paste0(PULocationID, " -> ", DOLocationID)) %>%
  group_by(route) %>%
  summarize(
    trip_count = n(),
    # Optional: see if these are mostly shared rides
    shared_match_rate = mean(shared_match_flag == "Y", na.rm = TRUE)
  ) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 10)

# 2. Visualize
ggplot(top_routes, aes(x = reorder(route, trip_count), y = trip_count, fill = shared_match_rate)) +
  geom_col() +
  coord_flip() +  # Flipping makes long route names easier to read
  scale_fill_viridis_c(labels = scales::percent) +
  labs(
    title = "Routes Travelled in 2021",
    subtitle = "Color intensity shows the rate of successful ride-sharing matches",
    x = "Route (Pickup - Dropoff)",
    y = "Number of Trips",
  ) +
  theme_minimal()





colnames(ds)
# Pull data first
sampled <- ds %>% 
  head(100000) %>% 
  collect()

# Then transform
sampled_clean <- sampled %>%
  mutate(
    # Only use ymd_hms if the data is currently a character string
    hour = hour(pickup_datetime),
    weekday = wday(pickup_datetime, label = TRUE)
  )

# Plot
ggplot(sampled_clean, aes(x = weekday)) +
  geom_bar() # geom_bar counts the rows automatically!

ggplot(sampled, aes(trip_miles, trip_duration_min)) +
  geom_point(alpha = 0.1) +
  geom_smooth()




ds %>%
  group_by(weekday) %>%
  summarize(n = n()) %>%
  ggplot(aes(weekday, n)) +
  geom_col()


sampled <- ds %>% sample_n(100000) %>% collect()

sampled <- ds %>%
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    dropoff_datetime = ymd_hms(dropoff_datetime),
    
    hour = hour(pickup_datetime),
    weekday = wday(pickup_datetime, label = TRUE),
    month = month(pickup_datetime, label = TRUE),
    
    trip_duration_min = as.numeric(difftime(dropoff_datetime, pickup_datetime, units = "mins")),
    
    tip_ratio = tips / base_passenger_fare
  )


# trips by week days 

sampled %>%
  group_by(weekday) %>%
  summarize(n = n()) %>%
  ggplot(aes(weekday, n)) +
  geom_col()



ds <- open_dataset("/Users/sorenbasnet/Desktop/data/2021/par/fhvhv_tripdata_2021-01.parquet")
colnames(ds)
ds |>
  select(DOLocationID,request_datetime) |>
  collect() |>
  mutate(row_id = row_number()) |>
  write.csv("missing_column.csv", row.names = FALSE)

# ~/Desktop/STAT_443_SP_2026/plots/605/data_tipratio.R


# Process a large sample to keep it performant
sampled_data <- ds %>%
  filter(base_passenger_fare > 0) %>%
  select(
    pickup_datetime, request_datetime, trip_miles, trip_time, 
    base_passenger_fare, tips, shared_request_flag, congestion_surcharge
  ) %>%
  collect() %>%
  slice_sample(n = 1000000) %>%
  mutate(
    # 1. The Target Variable
    tip_ratio = tips / base_passenger_fare,
    
    # 2. Time-based features
    hour = hour(pickup_datetime),
    wait_time_min = as.numeric(difftime(pickup_datetime, request_datetime, units = "mins")),
    is_rush_hour = ifelse(hour %in% c(7:9, 16:19), 1, 0),
    
    # 3. Trip-specific features
    speed_mph = trip_miles / (trip_time / 3600),
    is_shared = ifelse(shared_request_flag == "Y", 1, 0)
  ) %>%
  # Clean outliers (e.g., negative wait times or impossible speeds)
  filter(tip_ratio >= 0 & tip_ratio <= 1, wait_time_min >= 0, speed_mph < 100)

tip_impact_model <- lm(
  tip_ratio ~ trip_miles + trip_time + is_shared + wait_time_min + factor(hour), 
  data = sampled_data
)

summary(tip_impact_model)
colnames(sampled_data)


library(ggcorrplot)

cor_matrix <- sampled_data %>%
  select(tip_ratio, trip_miles, trip_time, base_passenger_fare, wait_time_min, speed_mph) %>%
  cor(use = "complete.obs")

ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation of Trip Factors with Tip Ratio")


# Linear model to quantify influence
tip_impact_model <- lm(
  tip_ratio ~ trip_miles + trip_time + is_shared + wait_time_min + factor(hour), 
  data = sampled_data
)

summary(tip_impact_model)

library(ggplot2)
library(dplyr)

hour_coefs <- data.frame(
  hour = 0:23,
  estimate = c(0, -0.000439, 0.002206, 0.003910, 0.006110, 0.008294,
               0.007716, 0.007202, 0.005522, 0.003660, 0.003079, 0.003183,
               0.003411, 0.004427, 0.003536, 0.002668, 0.001732, 0.000075,
               -0.002330, -0.002767, -0.002763, -0.005704, -0.006096, -0.001880),
  significant = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                  TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
                  TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
)

ggplot(hour_coefs, aes(x = hour, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = 0, ymax = estimate), fill = "#378ADD", alpha = 0.08) +
  geom_line(color = "#378ADD", linewidth = 1) +
  geom_point(aes(color = significant), size = 3) +
  scale_color_manual(
    values = c("TRUE" = "#378ADD", "FALSE" = "#B4B2A9"),
    labels = c("TRUE" = "Significant (p < 0.05)", "FALSE" = "Not significant"),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 23),
    labels = c("12am", "6am", "12pm", "6pm", "11pm")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 2), "%")
  ) +
  labs(
    title = "Tip ratio by hour of day",
    subtitle = "Coefficient relative to midnight (hour 0)",
    x = "Hour of day",
    y = "Coefficient"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50", size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )


# Visualizing how shared status and distance interact to affect tips
ggplot(sampled_data, aes(x = trip_miles, y = tip_ratio, color = shared_request_flag)) +
  geom_smooth(method = "gam") + # Generalized Additive Model for non-linear trends
  coord_cartesian(ylim = c(0, 0.25), xlim = c(0, 20)) +
  labs(
    title = "Influence of Distance and Shared Status on Tip Ratio",
    x = "Trip Distance (Miles)",
    y = "Average Tip Ratio",
    color = "Shared Request"
  ) +
  theme_minimal()


sampled_data %>%
  mutate(has_surcharge = ifelse(congestion_surcharge > 0, "Surcharge", "No Surcharge")) %>%
  group_by(has_surcharge) %>%
  summarise(mean_tip_ratio = mean(tip_ratio)) %>%
  ggplot(aes(x = has_surcharge, y = mean_tip_ratio, fill = has_surcharge)) +
  geom_col() +
  labs(title = "Does the NYC Congestion Surcharge Cannibalize Tips?",
       y = "Mean Tip Ratio", x = "") +
  theme_minimal()


t.test(tip_ratio ~ has_surcharge, data = sampled_data %>%
         mutate(has_surcharge = ifelse(congestion_surcharge > 0, "Surcharge", "No Surcharge")))

surcharge_summary <- sampled_data %>%
  mutate(has_surcharge = ifelse(congestion_surcharge > 0, "Surcharge", "No Surcharge")) %>%
  group_by(has_surcharge) %>%
  summarise(mean_tip_ratio = mean(tip_ratio))

ggplot(surcharge_summary, aes(x = has_surcharge, y = mean_tip_ratio, fill = has_surcharge)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(round(mean_tip_ratio * 100, 2), "%")),
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("No Surcharge" = "#B4B2A9", "Surcharge" = "#378ADD")) +
  labs(
    title = "Does the NYC Congestion Surcharge Cannibalize Tips?",
    subtitle = "t = -102.31, p < 0.001 — surcharge trips tip significantly more",
    y = "Mean tip ratio", x = "",
    caption = "No Surcharge: 2.81%  |  Surcharge: 4.88%  |  Difference: +2.08pp"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.06)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50", size = 11),
    plot.caption = element_text(color = "gray50", size = 10),
    panel.grid.major.x = element_blank()
  )



# Add fare amount as a predictor
#Tip ratio might be heavily influenced by the base fare size — cheap trips may get tipped differently than expensive ones.


sampled_data <- sampled_data %>%
  mutate(fare_bucket = cut(base_passenger_fare, 
                           breaks = c(0, 5, 10, 20, 40, Inf),
                           labels = c("$0-5", "$5-10", "$10-20", "$20-40", "$40+")))

sampled_data %>%
  group_by(fare_bucket) %>%
  summarise(mean_tip_ratio = mean(tip_ratio)) %>%
  ggplot(aes(x = fare_bucket, y = mean_tip_ratio)) +
  geom_col(fill = "#378ADD") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Tip ratio by fare size", x = "Base fare", y = "Mean tip ratio") +
  theme_minimal()



# Zero tip 
sampled_data <- sampled_data %>%
  mutate(tipped = ifelse(tip_ratio > 0, 1, 0))

tip_binary_model <- glm(
  tipped ~ trip_miles + trip_time + wait_time_min + 
    speed_mph + is_shared + fare_bucket + 
    factor(hour) + congestion_surcharge,
  data = sampled_data,
  family = binomial
)

summary(tip_binary_model)

# Get actual predicted probabilities for an average trip at each fare level
newdata <- data.frame(
  trip_miles = 3, trip_time = 900, wait_time_min = 3,
  speed_mph = 12, is_shared = 0, congestion_surcharge = 0,
  hour = factor(12, levels = 0:23),
  fare_bucket = factor(c("$0-5","$5-10","$10-20","$20-40","$40+"),
                       levels = c("$0-5","$5-10","$10-20","$20-40","$40+"))
)

newdata$prob_tip <- predict(tip_binary_model, newdata, type = "response")
print(newdata[, c("fare_bucket", "prob_tip")])





