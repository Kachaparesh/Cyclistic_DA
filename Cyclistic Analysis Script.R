# Data cleaning library
library(dplyr)
library("plyr")

# Package to calculate geo distance 
library(radiant)

# easy date time operations
library(lubridate)

# Data set library
library(tidyverse)

# Data visualization library
library(ggplot2)
library(gridExtra)

# Get all csv files from the directory
current_path <- "C:\\Users\\Paresh\\Documents\\Cyclistic\\Cyclistic_DA\\DataSets\\"

# Set working directory for project
setwd(current_path)

# search for csv files, to automate the same operation for all the files
files <- list.files(pattern = "*.csv", full.names=TRUE, recursive=FALSE)

# Fore loop of csv files
for(i in 1:length(files)){
  
  # Use one file at a time at index "i"
  file <- files[i]
  
  # Split file name from its extension
  fileNameSplitName <- unlist(strsplit(file, "\\."))
  
  # Use file name for later usage, to store validated csv file
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
  records_without_missing_ending_trip_data <- trip_data[!apply(trip_data, 1, function(x) any(x=="")),] 

  # 3) filter records further to check validation: positive time duration where time is not more then a day
  ride_duration_in_secs <- as.numeric(difftime(strptime(records_without_missing_ending_trip_data$ended_at, "%Y-%m-%d %H:%M:%S", tz = "GMT"), strptime(records_without_missing_ending_trip_data$started_at, "%Y-%m-%d %H:%M:%S", tz = "GMT"), units = "secs"))
  records_without_missing_ending_trip_data$ride_duration_in_secs <- round(ride_duration_in_secs, 2)
  
  records_with_valid_ride_duration <- filter(records_without_missing_ending_trip_data, ride_duration_in_secs > 0)
  
  # 4.1) Get week day of start and end times for further analysis
  records_with_valid_ride_duration$start_day <- weekdays(as.Date(records_with_valid_ride_duration$started_at))
  records_with_valid_ride_duration$end_day <- weekdays(as.Date(records_with_valid_ride_duration$ended_at))
  
  # 4.2) Get month of start and end times for further analysis
  records_with_valid_ride_duration$start_ride_month <- months(as.Date(records_with_valid_ride_duration$started_at))
  records_with_valid_ride_duration$end_ride_month <- months(as.Date(records_with_valid_ride_duration$ended_at))
  
  # 4.3) Get hour and date of start times for further analysis
  records_with_valid_ride_duration$start_ride_hour <- hour(records_with_valid_ride_duration$started_at)
  records_with_valid_ride_duration$start_ride_date <- date(records_with_valid_ride_duration$started_at)
  
  # 5) Insert new row with distance in meter
  records_with_valid_ride_duration$ride_distance_in_meter <- round(
    as_distance(
      records_with_valid_ride_duration$start_lat, records_with_valid_ride_duration$start_lng, records_with_valid_ride_duration$end_lat, records_with_valid_ride_duration$end_lng, unit = "km"
    ) * 1000
    , 2)
  
  # 5.1) filter rows that shows no ride distance
  records_with_valid_ride_duration <- records_with_valid_ride_duration %>% filter(ride_distance_in_meter > 0)
  
  records_with_valid_distance <- nrow(records_with_valid_ride_duration)
  
  # 6) To check avg speed of ride in meter/second, to filter bias and consider riders who has actually ride the bike
  records_with_valid_ride_duration$ride_avg_speed_in_meter_per_sec <- round(records_with_valid_ride_duration$ride_distance_in_meter/as.numeric(records_with_valid_ride_duration$ride_duration), 2)
  
  records_with_valid_ride_duration <- records_with_valid_ride_duration %>% filter(ride_avg_speed_in_meter_per_sec < 20)
  
  # Count total valid records
  count_of_valid_records <- nrow(records_with_valid_ride_duration)
  
  # 7) check if 80% of data qualifies for the analysis
  ratio_of_valid_records <- (count_of_valid_records/total_records)*100
  print(ratio_of_valid_records)
  
  # data analysis for quality
 data_quality_metrics <- c(total_records, nrow(records_without_missing_ending_trip_data), records_with_valid_distance, count_of_valid_records)
 print(data_quality_metrics)
 
 # Usage analysis per week day
 plotPrep <- records_with_valid_ride_duration %>% 
   group_by(start_day, member_casual, start_ride_month) %>% 
   dplyr :: summarise(count = n(), usage = sum(ride_distance_in_meter), duration = sum(ride_duration_in_secs))

p1 <- ggplot(data = plotPrep) +
 geom_col(position = "dodge", mapping = aes(x=start_day, y=duration, fill = member_casual)) +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=7)) +
  labs( x = "Day of the week", y = "Total time spent in seconds ",
        title ="Time spent by user every week day") +
   facet_wrap(~start_ride_month)
 
p2 <- ggplot(data = plotPrep) +
   geom_col(position = "dodge", mapping = aes(x=start_day, y=usage, fill = member_casual)) +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=7)) +
  labs( x = "Day of the week", y = "Total distnace in meter ",
        title ="Distance of rides every week day") + 
  facet_wrap(~start_ride_month)

grid.arrange(p1, p2, nrow = 1)
 
 # New file name using old one
 newFileName <- paste(paste(finalFileName, "-validated", sep = ""), fileExtension, sep = ".") 
 newPath <- paste(paste(current_path, "validated_data", sep = ""), newFileName, sep = "\\")
 write.csv(records_with_valid_ride_duration, newPath)
}

# current_path <- "C:\\Users\\Paresh\\Documents\\Cyclistic\\Cyclistic_DA\\DataSets\\validated_data\\"
# setwd(current_path)
# 
# complete_trip_data <- ldply(list.files(), read.csv, header=TRUE)
# 
# complete_trip_data <- complete_trip_data %>% filter(nchar(complete_trip_data$start_station_name) == 0)
# 
# View(complete_trip_data)
