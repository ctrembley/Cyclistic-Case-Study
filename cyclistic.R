# cyclistic capstone project (google data analytics certificate)
# author: connor trembley


## LOAD NECESSARY PACKAGES AND SET WORKING DIRECTORY
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
setwd('/Users/Konnor/Documents/Code/course_google_data/captstone_case_study/raw_data')


## INSPECT DATA FORMAT
test_df <- read_csv("202207-divvy-tripdata.csv")
head(test_df)


## READ AND MERGE CSV's

# source: https://stackoverflow.com/questions/23190280/whats-wrong-with-my-function-to-load-multiple-csv-files-into-single-dataframe
master_df <- list.files(pattern="*.csv") %>%  # creates vector of file names in char format for all files in working directory
  map_df(~read_csv(.))  #map_df applies a function to each element of a list, in this case fxn is read_csv
glimpse(master_df)

# read each csv and count rows and add row count as line to df
files <- list.files(pattern="*.csv") # turns csvs into list
row_counts <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(row_counts) <- c('file_name', 'row_count')
for(file in files){
  dataset <- read_csv(file) # read the csv at index i
  new_row <- c(file, nrow(dataset))
  row_counts[nrow(row_counts) + 1,] <- new_row
}
row_counts$row_count <- as.integer(row_counts$row_count)
row_counts %>% 
  summarise(total_rows = sum(row_count))


## CLEAN & MANIPULATE DATA

#make new df and rename column
clean_df <- master_df %>% 
  rename(user_type = member_casual)

# verify dates being viewed with max and min
clean_df %>% 
  summarise(oldest_date = min(started_at), most_recent_date = max(started_at))

# check for missing values and other errors in user_type column
clean_df %>% 
  group_by(user_type) %>% 
  summarise(users = n())

# check for errors in rideable_type column
clean_df %>% 
  group_by(rideable_type) %>% 
  summarise(rideable_types = n())

# look for duplicates in ride_id
sum(duplicated(clean_df$ride_id))
  
# add new columns
clean_df <- clean_df %>% 
  mutate(ride_duration = difftime(ended_at, started_at, unit="secs")) %>% 
  mutate(travel_day = weekdays(started_at)) %>% 
  mutate(travel_month = month(started_at))

# look for ride duration less than or equal to zero and remove, also drop na
clean_df <- clean_df %>% 
  filter(ride_duration > 0) %>% 
  drop_na()

# view duration to look for outliers
duration_df <- clean_df %>% 
  arrange(-ride_duration)

# remove unused columns and drop na values
clean_df <- clean_df %>% 
  select(-c(start_lat, end_lat, start_lng, end_lng, start_station_name, end_station_name, start_station_id, end_station_id)) %>% 
  drop_na()

## ANALYSIS & VIZ

days_in_week <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
                      "Thursday", "Friday", "Saturday")
months_in_year <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# average ride duration per month split by user type
one_df <- clean_df %>% 
  group_by(user_type, travel_month) %>% 
  summarise(mean_ride_duration = mean(ride_duration))
one_df

ggplot(data = one_df) +
  geom_bar(mapping = aes(x = factor(month.abb[travel_month], months_in_year), y = mean_ride_duration, fill=user_type), stat = "identity", show.legend=FALSE) +
  facet_wrap(~user_type) +
  labs(title="Members vs Casual Riders Ride Duration",
       subtitle='Split by Month',
       x = "Travel Month",
       y = "Avg. Ride Duration (s)")
ggsave('plot_one.png', width = 10, height = 5)
  
# average ride duration per weekday split by user type
two_df <- clean_df %>% 
  group_by(user_type, travel_day) %>% 
  summarise(number_of_rides = n())
two_df

ggplot(data = two_df) +
  geom_bar(mapping = aes(x = factor(travel_day, days_in_week), y = number_of_rides, fill=user_type), stat = "identity", show.legend=FALSE) +
  facet_wrap(~user_type) +
  labs(title="Members vs Casual Riders Ride Count",
     subtitle='Split by Weekday',
     x = "Travel Day",
     y = "Number of Rides")
ggsave('plot_two.png', width = 10, height = 5)

# average ride length of user_types
three_df <- clean_df %>% 
  group_by(user_type) %>% 
  summarise(mean_ride_duration = mean(ride_duration))
three_df

# ride duration distribution
ggplot(data = clean_df, aes(x = ride_duration, fill = user_type)) +
  geom_histogram(bins = 100) +
  scale_y_continuous(n.breaks=10) +
  xlim(0, 7500) +
  labs(title="Members vs Casual Riders Ride Duration",
       subtitle='Bins of 100',
       caption = "Duration limited to 7500 secs",
       x = "Ride Duration (secs)",
       y = "Count")
ggsave('plot_three.png', width = 10, height = 5)
