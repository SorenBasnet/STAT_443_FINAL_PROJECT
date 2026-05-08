#####
#
#
#
#####

# Import required libraries 
library(arrow)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)     # For spatial mapping
library(scales)

# data file path 
#data_file_path <- "../data/"

# Read datafile 
#data <- read_parquet("/Users/sorenbasnet/Desktop/data/2021/fhvhv_tripdata_2021-01.parquet")

data_path <- "/Users/sorenbasnet/Desktop/data_2/"
ds <- open_dataset(data_path)

df <- ds %>% collect()

colnames(ds)
summary(df)


# 1. Monthly Demand Trend (2021-2025)
monthly_demand <- ds %>%
  mutate(
    year = year(pickup_datetime),
    month = month(pickup_datetime)
  ) %>%
  group_by(year, month) %>%
  summarize(total_trips = n()) %>%
  collect() %>%
  mutate(date = make_date(year, month, 1)) %>%
  arrange(date)

# Visualization
ggplot(monthly_demand, aes(x = date, y = total_trips)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_point() +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Monthly Ride Demand Trend (Months of 2021)",
       subtitle = "Total HVFHV Trips in NYC",
       x = "Time", y = "Number of Trips") +
  theme_minimal()

# ----------- #


# 2. Peak Hours vs. Day of Week
hourly_patterns <- ds %>%
  mutate(
    hour = hour(pickup_datetime),
    wday = wday(pickup_datetime, label = TRUE)
  ) %>%
  group_by(wday, hour) %>%
  summarize(avg_trips = n() / (5 * 52)) %>% # Rough normalization for 5 years
  collect()

# Heatmap Visualization
ggplot(hourly_patterns, aes(x = hour, y = wday, fill = avg_trips)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", labels = label_comma()) +
  labs(title = "Weekly Demand Heatmap",
       subtitle = "Average trips by Hour and Day of Week",
       x = "Hour of Day (24h)", y = "Day of Week", fill = "Avg Trips") +
  theme_minimal()

# ----------- #

# 3. Spatial Distribution (Pickups)
spatial_demand <- ds %>%
  group_by(PULocationID) %>%
  summarize(total_trips = n()) %>%
  collect()

# Load NYC Taxi Zone Shapefile (assuming you have it)
zones <- st_read("taxi_zones.shp")
map_data <- zones %>% left_join(spatial_demand, by = c("LocationID" = "PULocationID"))

library(ggplot2)
# Map Visualization
ggplot(map_data) +
  geom_sf(aes(fill = total_trips)) +
  scale_fill_viridis_c(trans = "log10") + # Log scale helps see variation
  labs(title = "Spatial Distribution of Ride Demand",
       fill = "Log(Total Trips)") +
   theme_void()

# ----------- #



library(arrow)
library(tools)

input_dir <- "/Users/sorenbasnet/Desktop/data/2021"
output_dir <- "/Users/sorenbasnet/Desktop/data/2021"

# make sure output directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# list all parquet files
parquet_files <- list.files(input_dir, pattern = "\\.parquet$", full.names = TRUE)

for (file in parquet_files) {
  
  message("Processing: ", file)
  
  # 1. Read the file into memory (or a stream)
  # For simple conversion, read_parquet is more direct than open_dataset
  df <- read_parquet(file)
  
  # 2. Define the exact file path
  out_file <- file.path(
    output_dir,
    paste0(file_path_sans_ext(basename(file)), ".csv")
  )
  
  # 3. Use write_csv_arrow to create a single file
  write_csv_arrow(df, sink = out_file)
}





for (file in parquet_files) {
  
  message("Processing: ", file)
  
  # read dataset
  ds <- open_dataset(file)
  
  # convert filename
  out_file <- file.path(
    output_dir,
    paste0(file_path_sans_ext(basename(file)), ".csv")
  )
  
  # write csv
  write_dataset(
        ds,
    path = out_file,
    format = "csv"
  )
}









hourly_patterns <- ds %>%
  mutate(
    hour = hour(pickup_datetime),
    wday = wday(pickup_datetime, label = TRUE)
  ) %>%
  group_by(wday, hour) %>%
  summarize(avg_trips = n() / (5 * 52)) %>% # Rough normalization for 5 years
  collect()

# Heatmap Visualization
ggplot(hourly_patterns, aes(x = hour, y = wday, fill = avg_trips)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", labels = label_comma()) +
  labs(title = "Weekly Demand Heatmap",
       subtitle = "Average trips by Hour and Day of Week",
       x = "Hour of Day (24h)", y = "Day of Week", fill = "Avg Trips") +
  theme_minimal()


data_path <- "/Users/sorenbasnet/Desktop/data/2021/par/fhvhv_tripdata_2021-01.parquet"
ds <- open_dataset(data_path

                   