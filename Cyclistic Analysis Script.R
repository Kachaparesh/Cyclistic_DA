# Data cleaning library
library(dplyr)
library("plyr")

# Package to calculate geo distance 
library(radiant)

# easy date time operations
library(lubridate)

library(plyr)
library(tidyverse)
library(ggplot2)
# Get all csv files from the directory
current_path <- "/Users/pareshkacha/Documents/Capestone proj1/Cyclistic project data/"
setwd(current_path)

files <- list.files(pattern = "*.csv", full.names=TRUE, recursive=FALSE)
for(i in 1:length(files)){ # 
  file <- files[i]
  fileNameSplitName <- unlist(strsplit(file, "\\."))
  finalFileName <- gsub("/", "", fileNameSplitName[2])
  print(finalFileName)
  fileExtension <- "csv"
  fileName <- paste(finalFileName, fileExtension, sep = ".")
  
  #  Read CSV files and store data in data frame
  # trip_data <- ldply(list.files(), read.csv, header=TRUE)
  trip_data <- read.csv(fileName)
  
  # check if records are unique
  isDuplicate <-   trip_data %>%
    group_by(ride_id) %>%
    dplyr::mutate(duplicate_name = n()-1) %>%
    ungroup() %>%
    filter(duplicate_name > 0)
  
  print(isDuplicate)
  
  # 1) total number of record in the data frame
  total_records <- nrow(trip_data)
  
  # 2) Get records without missing value
  records_without_missing_values <- trip_data[complete.cases(trip_data), ]
  
  # 3) filter records further to check validation: positive time duration and time is not more then a day
  ride_duration_in_secs <- as.numeric(difftime(strptime(records_without_missing_values$ended_at, "%Y-%m-%d %H:%M:%S", tz = "GMT"), strptime(records_without_missing_values$started_at, "%Y-%m-%d %H:%M:%S", tz = "GMT"), units = "secs"))
  records_without_missing_values$ride_duration_in_secs <- round(ride_duration_in_secs, 2)
  
  records_with_valid_values <- filter(records_without_missing_values, ride_duration_in_secs > 0)
  
  
  # 4.1) Get week day of start and end times for further analysis
  records_with_valid_values$start_day <- weekdays(as.Date(records_with_valid_values$started_at))
  records_with_valid_values$end_day <- weekdays(as.Date(records_with_valid_values$ended_at))
  
  # 4.2) Get month of start and end times for further analysis
  records_with_valid_values$start_ride_month <- months(as.Date(records_with_valid_values$started_at))
  records_with_valid_values$end_ride_month <- months(as.Date(records_with_valid_values$ended_at))
  
  # 4.3) Get hour and date of start times for further analysis
  records_with_valid_values$start_ride_hour <- hour(records_with_valid_values$started_at)
  records_with_valid_values$start_ride_date <- date(records_with_valid_values$started_at)
  
  # 5) Insert new row with distance in meter
  records_with_valid_values$ride_distance_in_meter <- round(
    as_distance(
      records_with_valid_values$start_lat, records_with_valid_values$start_lng, records_with_valid_values$end_lat, records_with_valid_values$end_lng, unit = "km"
    ) * 1000
    , 2)
  
  # 6) To check avg speed of ride in meter/second, to filter bias and consider riders who has actually ride the bike
  records_with_valid_values$ride_avg_speed_in_meter_per_sec <- round(records_with_valid_values$ride_distance_in_meter/as.numeric(records_with_valid_values$ride_duration), 2)
  
  records_with_valid_values <- records_with_valid_values %>% filter(ride_avg_speed_in_meter_per_sec > 0)
  
  records_with_valid_values <- records_with_valid_values %>% filter(ride_avg_speed_in_meter_per_sec < 20)
  
  # Count total valid records
  count_of_valid_records <- nrow(records_with_valid_values)
  
  # 7) check if 80% of data qualifies for the analysis
  ratio_of_valid_records <- (count_of_valid_records/total_records)*100
  print(ratio_of_valid_records)
  
  # New file name using old one
  newFileName <- paste(paste(finalFileName, "-validated", sep = ""), fileExtension, sep = ".") 
  newPath <- paste(paste(current_path, "validated_data", sep = "/"), newFileName, sep = "/")
  write.csv(records_with_valid_values, newPath)
  
  # str(records_with_valid_values)  
}

current_path <- "/Users/pareshkacha/Documents/Capestone proj1/Cyclistic project data/validated_data/"
setwd(current_path)

complete_trip_data <- ldply(list.files(), read.csv, header=TRUE)

str(complete_trip_data)

ride_data <- complete_trip_data %>% group_by(start_day)

temp_data <- complete_trip_data %>%
  group_by(start_ride_month, start_day, start_ride_date, start_ride_hour, rideable_type, member_casual) %>%
  dplyr::summarise(number_of_rides = n()/1000) 
# start visualization from here%>%
  ggplot(aes(x=start_ride_month, y=number_of_rides, color=member_casual)) +
  geom_point() +
  geom_smooth() +
  # facet_wrap(~ member_casual) +
  ggtitle("Number of rides for type of rideable and types of users in the last 12 months") +
  ylab("Number of rides (1 000)") +
  xlab("Date") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

View(ride_data)
