---
title: "Cyclistic bike-share analysis case study"
author: "Ezra"
output:
  html_document: default
  pdf_document: default
---
# Business Task
How do annual members and casual riders use Cyclistic bikes differently?

# DATA PROCESSING
## Load libraries
```{r}
library(tidyverse)
library(lubridate)
library(ggplot2)
```

## load 12 .csv files from Febuary 2023 to January 2024 
```{r}
 Feb2023_df <- read.csv("Dataset2/202302-divvy-tripdata.csv")
 Mar2023_df <- read.csv("Dataset2/202303-divvy-tripdata.csv")
 Apr2023_df <- read.csv("Dataset2/202304-divvy-tripdata.csv")
 May2023_df <- read.csv("Dataset2/202305-divvy-tripdata.csv")
 Jun2023_df <- read.csv("Dataset2/202306-divvy-tripdata.csv")
 Jul2023_df <- read.csv("Dataset2/202307-divvy-tripdata.csv")
 Aug2023_df <- read.csv("Dataset2/202308-divvy-tripdata.csv")
 Sep2023_df <- read.csv("Dataset2/202309-divvy-tripdata.csv")
 Oct2023_df <- read.csv("Dataset2/202310-divvy-tripdata.csv")
 Nov2023_df <- read.csv("Dataset2/202311-divvy-tripdata.csv")
 Dec2023_df <- read.csv("Dataset2/202312-divvy-tripdata.csv")
 Jan2024_df <- read.csv("Dataset2/202401-divvy-tripdata.csv")
```

## Merge all  files into a new data frame 
```{r}
merged_df <- rbind(Feb2023_df, Mar2023_df, Apr2023_df, May2023_df, Jun2023_df, Jul2023_df, Aug2023_df, Sep2023_df, Oct2023_df, Nov2023_df, Dec2023_df, Jan2024_df)
```

## Remove 12 uploaded csv.files
```{r}
rm(Feb2023_df, Mar2023_df, Apr2023_df, May2023_df, Jun2023_df, Jul2023_df, Aug2023_df, Sep2023_df, Oct2023_df, Nov2023_df, Dec2023_df, Jan2024_df)
```

## Save the data frame as `merged_data` and get an overview of it 
```{r}
merged_data <- merged_df
glimpse(merged_data)
```

There are *13 variables* and *5,674,449 observations* in total. 

# DATA CLEANING AND MANIPULATION
## Data Cleaning 
### Remove duplicate rows 
```{r}
merged_data <- distinct(merged_data)
```

### Remove rows with NA values 
```{r}
merged_data <- na.omit(merged_data) 
```

### Remove rows with white space or empty string in any columns
```{r}
merged_data <- merged_data[rowSums(sapply(merged_data, function(x) x == "")) == 0, ]
```

### Remove unnecessary columns
```{r}
merged_data <- subset(merged_data, select = -c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))
```

### Save the data frame as `cleaned_data` and get an overview of it 
```{r}
cleaned_data <- merged_data
View(cleaned_data)
```

## Data Manipulation 
### Create new columns `Ride_length` that calculates the length of each ride by subtracting started_at column from ended_at column and converts it into minutes.
```{r}
# create the column
cleaned_data$ride_length <- difftime(cleaned_data$ended_at, cleaned_data$started_at, units = 'min')
cleaned_data$ride_length <- as.numeric(cleaned_data$ride_length) 

# check the first 5 rows
head(cleaned_data, 5)
```

### Create new column `Day_of_week` that extracts the day of the week from started_at column
```{r}
# create the column
cleaned_data$day_of_week <- wday(cleaned_data$started_at, label = TRUE, abbr = FALSE)

# check the first 5 rows
head(cleaned_data, 5)
```

### Create new column `Month` that extracts the month from started_at column
```{r}
# create the column
cleaned_data$month <- format(as.Date(cleaned_data$started_at), "%m")

# check the first 5 rows
head(cleaned_data, 5)
```

### Create new column `Day` that extracts the day from started_at column.
```{r}
# create the column
cleaned_data$day <- format(as.Date(cleaned_data$started_at), "%d") 

# check the first 5 rows
head(cleaned_data, 5)
```

### Create new column `Hour` that extracts the hour from started_at column
```{r}
# create the column
cleaned_data$hour <- format(as.POSIXct(cleaned_data$started_at), format = "%H")

# check the first 5 rows
head(cleaned_data, 5)
```

### Round_trip: identify whether the route is one-way or round-trip
```{r}
# create the column
cleaned_data$round_trip <- ifelse(cleaned_data$start_station_name == cleaned_data$end_station_name, 'Yes', 'No')

# check the first 5 rows
head(cleaned_data, 5)
```

## View the data frame 
```{r}
View(cleaned_data)
```

## Remove any rows which ride_length is negative
```{r}
cleaned_data <- cleaned_data %>%
  filter(ride_length >= 0)  
```

## Save the new data frame as `final_data`
```{r}
final_data <- cleaned_data
```

# DATA ANALYZATION
## Total Rides & Average Ride Length
```{r}
final_data %>%
 summarize(total_rides = nrow(final_data),average_ride_length = round(mean(ride_length), digits = 2))
```

## Total Rides and Average Ride Length by User Type 
```{r}
final_data %>%
  group_by(member_casual) %>%
  summarise(total_rides = length(member_casual),
            percentage = round((length(member_casual) / nrow(final_data)) * 100, digits = 2),
            average_ride_length = round(mean(ride_length), digits = 2))

```

## Total Rides and Average Ride Length by Rideable Type and User Type. 
```{r}
final_data %>%
  group_by(member_casual, rideable_type) %>%
  summarise(total_rides = length(member_casual),
            percentage = round((length(rideable_type) / nrow(final_data)) * 100, digits = 2),
            average_ride_length = round(mean(ride_length), digits = 2))
```
Findings after a few calculations:

- Members accounted for a larger share of the total rides, at 64.6%.

- The average ride length for members was 12.2 minutes which was significantly shorter than that of casual riders (23.0 minutes) and the overall average ride length (16.02 minutes).

- The most popular bike among all users was classic.

- Member users were the predominant users of both electric and classic bikes, while docked bikes were not significantly used by either type of user.

# DATA VISUALIZATION
## Total Rides by Month and User Type 
```{r}
final_data %>%
  group_by(member_casual, month) %>% 
  summarize(number_of_ride = n()) %>% 

ggplot(aes(month, number_of_ride, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.7) +
  facet_wrap(~ member_casual) + 
  geom_text(aes(label = format(number_of_ride, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 2.0) + 
  
  labs(x = "Month", y = "Number of Rides", 
       title = "Cyclistic - Total Rides (Feb 2023 - Jan 2024)", 
       subtitle =  "By Month and User Type") +
  scale_fill_manual(values = c("#de6e56","#0b3c5d")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, 550000)) +
  theme_classic(base_size = 10) +
  coord_flip()

ggsave("Total_Rides_by_Month_and_User_Type.png", plot = last_plot(), dpi = 600)
```
As can be seen from the graph:

- Member usage consistently exceeded casual usage at every point throughout the year.

- The low season for both user types occurred from December to February of the following year, while the high season was from May to October.

- The smallest gap between casual and member rides was in the summer months, which could mean that pleasant weather conditions attract more casual riders. While the biggest gap was in the colder months, potentially indicating that casual riders were far more affected by seasonal changes than members.

## Average Ride Length by Month and User Type 
```{r}
final_data %>%
  group_by(member_casual, month) %>% 
  summarize(average_ride_length = round(mean(ride_length), digits = 2))%>% 
  
ggplot(aes(month,average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.7) +
  facet_wrap(~ member_casual) + 
  geom_text(aes(label = format(average_ride_length, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 2.0) + 
  
  labs(x = "Month", y = "Average Ride Length (min)", 
       title = "Cyclistic - Average Ride Length (Feb 2023 - Jan 2024)", 
       subtitle =  "By Month and User Type") +
  scale_fill_manual(values = c("#de6e56","#0b3c5d")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0,30)) +
  theme_classic(base_size = 10) + 
  coord_flip()

ggsave("Average_Ride_Length_by_Month_and_User_Type.png", plot = last_plot(), dpi = 600)
```

The graph showed that:

-Casual riders consistently had longer average ride lengths with significant fluctuations throughout the year compared to members.

- The gap between the average ride length of casual and member riders narrowed slightly during summer months. It may indicate that members took longer rides in better weather condition, which was similar to casuals, but to a lesser extent.

## Total Rides by Day of the week and User Type
```{r}
final_data %>%
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_ride = n()) %>% 

ggplot(aes(factor(day_of_week,levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")), number_of_ride, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.7) +
  facet_wrap(~ member_casual) + 
  geom_text(aes(label = format(number_of_ride, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 2.0) + 
  
  labs(x = "Day of week", y = "Number of Rides", 
       title = "Cyclistic - Total Rides (Feb 2023 - Jan 2024)", 
       subtitle =  "By Day of Week and User Type") +
  scale_fill_manual(values = c("#de6e56","#0b3c5d")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, 550000)) +
  theme_classic(base_size = 10) +
  coord_flip()

ggsave("Total_Rides_by_Day_of_the_Week_and_User_Type.png", plot = last_plot(), dpi = 600)
```
Overall, member users consistently outnumbered casual users in total rides, with the narrowest gap on Saturdays. Members tended to be more active on weekdays while casual users used the bikes more frequently on weekends.

## Average Ride Length by Day of the week and User Type
```{r}
final_data %>%
  group_by(member_casual, day_of_week) %>% 
  summarize(average_ride_length = round(mean(ride_length), digits = 2))%>% 
  
ggplot(aes(factor(day_of_week,levels = c("Sunday","Saturday", "Friday","Thursday","Wednesday","Tuesday","Monday")),average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.7) +
  facet_wrap(~ member_casual) + 
  geom_text(aes(label = format(average_ride_length, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 2.0) + 
  
  labs(x = "Day of week", y = "Average Ride Length (min)", 
       title = "Cyclistic - Average Ride Length (Feb 2023 - Jan 2024)", 
       subtitle =  "By Day of Week and User Type") +
  scale_fill_manual(values = c("#de6e56","#0b3c5d")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0,30)) +
  theme_classic(base_size = 10) + 
  coord_flip()

ggsave("Average_Ride_Length_by_Day_of_the_Week_and_User_Type.png", plot = last_plot(), dpi = 600)
```
As shown in the graph:

- The difference between the day with the highest and lowest number of rides was particularly pronounced among casual riders.

-Both types had their longest average ride on the weekends.
  
  - For casual users, Sunday recorded the longest average ride at 26.61 minutes. Although Saturday had more riders than Sunday, the average ride length on Saturday was shorter than on Sunday.

  - For member users, Sunday also had the longest average ride at 13.66 minutes. Even though Saturday had higher total rides than Sunday, the average ride length for both days was quite similar.

## Total Rides by Hour and User Type
```{r}
final_data %>%
  group_by(member_casual, hour) %>% 
  summarize(number_of_ride = n()) %>% 

ggplot(aes(hour, number_of_ride, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.7) +
  facet_wrap(~ member_casual) + 
  geom_text(aes(label = format(number_of_ride, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 2.0) + 
  
  labs(x = "Hour", y = "Number of Rides", 
       title = "Cyclistic - Total Rides (Feb 2023 - Jan 2024)", 
       subtitle =  "By Hour and User Type") +
  scale_fill_manual(values = c("#de6e56","#0b3c5d")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, 350000)) +
  theme_classic(base_size = 10) +
  coord_flip()

ggsave("Total_Rides_by_Hour_and_User_Type.png", plot = last_plot(), dpi = 300)
```
According to the graph:

- The total rides of member riders consistently outnumbered casual riders throughout most hours of the day.

- Both types had similar pattern of peak hours which were between 4 PM and 6 PM. They also experienced a significant drop in total rides during the nighttime hours.

- Casual rides were more evenly distributed across the midday to evening hours. Their number of rides increased steadily from 10 AM till the afternoon, peaking at 5 PM.

- Member riders experienced peak hours in the early morning from 7 AM to 8 AM and from late afternoon to early evening (4 - 6 PM), which likely correspond to typical commuting times.

## Average Ride Length by Hour and User Type
```{r}
final_data %>%
  group_by(member_casual, hour) %>% 
  summarize(average_ride_length = round(mean(ride_length), digits = 2))%>% 
  
ggplot(aes(hour,average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.7) +
  facet_wrap(~ member_casual) + 
  geom_text(aes(label = format(average_ride_length, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 2.0) + 
  
  labs(x = "Hour", y = "Average Ride Length (min)", 
       title = "Cyclistic - Average Ride Length (Feb 2023 - Jan 2024)", 
       subtitle =  "By Hour and User Type") +
  scale_fill_manual(values = c("#de6e56","#0b3c5d")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0,30)) +
  theme_classic(base_size = 10) + 
  coord_flip()

ggsave("Average_Ride_Length_by_Hour_and_User_Type.png", plot = last_plot(), dpi = 600)
```
From the graph, it could be noticed that:

- The average ride length of casual riders was significantly longer than that of member riders, with the difference being particularly pronounced during midday hours.

- For casual users, there was a noticeable increase in ride length during the midday hours, particularly from 10 AM to 3 PM.

- For member users, the average ride length was shorter and more consistent throughout the day, fluctuating between 10.23 minutes and 13.14 minutes.

## Total Rides & Average Ride Length of Round-trip and One-way Route by User Type
```{r}
 final_data %>%
  group_by(member_casual, round_trip) %>% 
  summarise(total_rides = length(round_trip),
            average_ride_length = round(mean(ride_length), digits = 2))
```
Findings from the data:

- The total number of rides on one-way routes was more than round-trip routes for both riders while the average ride length for round-trip routes was longer than for one-way routes.

## Top 10 Most Frequent Routes for Member User
```{r}
final_data <- final_data %>%
  mutate(route = paste(start_station_name, "->", end_station_name))

# group by the new route column, filter for list of top 10 routes 
top_routes_member <- final_data %>%
  filter(member_casual == "casual") %>%
  group_by(route, member_casual, round_trip) %>%
  summarize(number_of_ride = n()) %>%
  ungroup() %>%
  arrange(desc(number_of_ride)) %>% 
  slice_max(order_by = number_of_ride, n = 10)

# plot the top 10 routes
ggplot(top_routes_member, aes(x = reorder(route, number_of_ride), y = number_of_ride, fill = round_trip)) +
geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.5) +
  geom_text(aes(label = format(number_of_ride, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 1.7) + 
  
  labs(x = "Route", y = "Number of Rides", 
       title = "Cyclistic - Top 10 Most Frequent Routes", 
       subtitle =  "For Casual Users") +
  scale_fill_manual(values = c("#c1c1f3","#ffd1dc")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0,10000)) +
  theme_classic(base_size = 10) + 
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip()

ggsave("Top_10_Most_Frequent_Routes_For_Casual_Users.png", plot = last_plot(), dpi = 300)
```
## Top 10 Most Frequent Routes for Casual User
```{r}
final_data <- final_data %>%
  mutate(route = paste(start_station_name, "->", end_station_name))

# group by the new route column, filter for list of top 10 routes 
top_routes_member <- final_data %>%
  filter(member_casual == "casual") %>%
  group_by(route, member_casual, round_trip) %>%
  summarize(number_of_ride = n()) %>%
  ungroup() %>%
  arrange(desc(number_of_ride)) %>% 
  slice_max(order_by = number_of_ride, n = 10)

# plot the top 10 routes
ggplot(top_routes_member, aes(x = reorder(route, number_of_ride), y = number_of_ride, fill = round_trip)) +
geom_bar(stat = "identity", position = position_dodge(preserve = 'single'), width = 0.5) +
  geom_text(aes(label = format(number_of_ride, big.mark = ",")), position = position_dodge(width = 0.7), hjust = -0.1, size = 1.7) + 
  
  labs(x = "Route", y = "Number of Rides", 
       title = "Cyclistic - Top 10 Most Frequent Routes", 
       subtitle =  "For Casual Users") +
  scale_fill_manual(values = c("#c1c1f3","#ffd1dc")) +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0,10000)) +
  theme_classic(base_size = 10) + 
  theme(axis.text.y = element_text(size = 6)) + 
  coord_flip()

ggsave("Top_10_Most_Frequent_Routes_For_Casual_Users.png", plot = last_plot(), dpi = 300)
```
As can be discovered from the graph of top 10 most frequent routes for each user:

- Round-trip routes were predominant among casual riders, accounting for 7 out of 10 routes.

- There was only one common route for both groups, which was from Ellis Ave & 55th St to Ellis Ave & 60 St.

- Number of rides for member users were more evenly distributed compared to casual riders.

  -Member riders ranged from around 2,500 to just over 5,000, with the most frequently used route being a one-way trip.
  
  -Casual riders saw greater variability with the total rides fluctuating from about 2,000 to over 8,000 and the most popular route was a round trip.

# Summary

Below are some insights and trends I have gained from the analysis that can be useful when design marketing strategies aimed at converting casual riders into annual members.

- The most popular bike among all users was classic.
The high season for both types of users was from May to October.

- Members tended to be more active on weekdays, whereas casuals used the bikes more frequently on weekends.
Busiest hours for both member and user riders were between 4 PM and 6 PM.

- Casual users travelled longer distance than members throughout the whole week. The average ride length of casual riders was 10.8 minutes longer than members riders.

- The total rides on one-way routes outnumbered those for round-trip routes for both types of users.