install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("geosphere")

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(geosphere)

#read month-wise csv files
#august22 <- read.csv("./202208-divvy-tripdata.csv")
#september22 <- read.csv("./202209-divvy-tripdata.csv")
october22 <- read.csv("./202210-divvy-tripdata.csv")
november22 <- read.csv("./202211-divvy-tripdata.csv")
december22 <- read.csv("./202212-divvy-tripdata.csv")
january23 <- read.csv("./202301-divvy-tripdata.csv")
february23 <- read.csv("./202302-divvy-tripdata.csv")
march23 <- read.csv("./202303-divvy-tripdata.csv")
april23 <- read.csv("./202304-divvy-tripdata.csv")
may23 <- read.csv("./202305-divvy-tripdata.csv")
#june23 <- read.csv("./202306-divvy-tripdata.csv")
#july23 <- read.csv("./202307-divvy-tripdata.csv")

#merge month-wise data frame into a single frame
trip_data <- bind_rows(october22, november22, december22, january23, february23, march23, april23, may23)

#see the first 6 rows of the data frame
head(trip_data)
#how many rows are in the data frame
nrow(trip_data)
#list of column names
colnames(trip_data)
#dimensions of the data frame
dim(trip_data)
#statistical summary of data, mainly for numerics
summary(trip_data)
#see list of columns and data types
str(trip_data)
#see the last 6 rows of the data frame
tail(trip_data)

#adding columns for date, month, day, year to the data frame
trip_data$date <- as.Date(trip_data$started_at)
trip_data$month <- format(as.Date(trip_data$date), "%m")
trip_data$day <- format(as.Date(trip_data$date), "%d")
trip_data$year <- format(as.Date(trip_data$date), "%Y")
trip_data$day_of_week <- format(as.Date(trip_data$date), "%A")
#to get the names of all the columns
colnames(trip_data)

#ride length calculation
trip_data$ride_length <- difftime(trip_data$ended_at, trip_data$started_at)
#check structure of the columns
str(trip_data)

#convert ride length data to Numeric
trip_data$ride_length <- as.numeric(as.character(trip_data$ride_length))
#to confirm it is now numeric
is.numeric(trip_data$ride_length)

#ride distance calculation
trip_data$ride_distance <- distGeo(matrix(c(trip_data$start_lng, trip_data$start_lat), ncol=2), matrix (c(trip_data$end_lng, trip_data$end_lat), ncol=2))
#to calculate distance in km
trip_data$ride_distance <- trip_data$ride_distance/1000

#removing unclean data where length of the ride is either negative or 0
trip_data_clean <- trip_data[!(trip_data$ride_length <= 0),]
#gives column names and data in the column
glimpse(trip_data_clean)

#first lets check the structure of the data frame
str(trip_data_clean)
#to check the summarized details of the clean data frame
summary(trip_data_clean)


trip_data_clean %>% 
  group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length), median_length = median(ride_length), 
            max_ride_length = max(ride_length), min_ride_length = min(ride_length))

trip_data_clean %>% 
  group_by(member_casual) %>% 
  summarise(ride_count = length(ride_id))

# lets order the days of the week
trip_data_clean$day_of_week <- ordered(trip_data_clean$day_of_week, 
                                       levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
trip_data_clean %>% 
  group_by(member_casual, day_of_week) %>%  
  #groups by member_casual
  summarise(number_of_rides = n() 
            #calculates the number of rides and average duration 
            ,average_ride_length = mean(ride_length),.groups="drop") %>% 
  # calculates the average duration
  arrange(member_casual, day_of_week) #sort

#chart for visualize average ride by day of the week
trip_data_clean %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time of Members and Casual riders Vs. Day of the week")

#total rides taken by members and casuals by month
trip_data_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders by Month") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#comparison of members and casual riders depending on ride distance
trip_data_clean %>% 
  group_by(member_casual) %>% drop_na() %>%
  summarise(average_ride_distance = mean(ride_distance)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean distance traveled by Members and Casual riders")

#bike rides by membership
trip_data_clean %>% 
  ggplot(aes(fill=rideable_type, x= member_casual)) +
  geom_bar() +
  labs(title="Bike Rides by Membership", x="User Type", y="Rides", fill="Bike Type")

#bike type by membership
trip_data_clean %>%
  ggplot(aes(fill=rideable_type, x= member_casual, y=1)) +
  geom_bar(stat="identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title="Bike Type by Membership", x="User Type", y="Bike Type")
