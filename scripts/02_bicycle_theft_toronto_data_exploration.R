#### Preamble ####
# Purpose: Conduct exploratory review on the data and determine what to focus on.
  # This script is to store the code for the exploratory review. Not all the code
  # will be used
# Author: Ken Lee
# Data: 31 January 2021
# Contact: kenllee97@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the "Bicycle Thefts" data and saved it to inputs/data


### Workspace setup ###
# Use R Projects, not setwd().
library(tidyverse)
library(ggplot2)
library(knitr)

# Load/Read in the raw data.
cleaned_data <- readr::read_delim("inputs/data/cleaned_data.csv", delim = ","
                             )
#Chekcing column names
names(cleaned_data)

### A. Frequency ###
   
## A1. Offence Frequency
Offence_f <- cleaned_data %>% #Creating an Offence frequency variable.
  count(Primary_Offence, sort = TRUE, name = "Frequency") %>% # Counting the 
  # frequency of Primary. 
  # Offences and sorted them in a descending order.
  top_n(10) # Filtered the top ten

## A2.  Date Frequency
Date_f <- cleaned_data %>% 
  count(Occurrence_Date, name = "Frequency")

# Plotting the date occurrence frequency into a line graph.
Date_f %>% 
  ggplot(mapping = aes(Occurrence_Date, Frequency)) + 
  geom_line() +
  theme_minimal()

## A3. Time Frequency
Time_f <- cleaned_data %>% 
  count(Occurrence_Time, name = "Frequency")

# Plotting the time occurrence frequency into a line graph.
Time_f %>% 
  ggplot(mapping = aes(Occurrence_Time, Frequency)) + 
  geom_line() +
  theme_minimal()

## A4. Division Frequency
Division_f <- cleaned_data %>% 
  count(Division, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A5. Location Type Frequency
Location_Type_f <- cleaned_data %>% 
  count(Location_Type, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A6. Bike Maker Frequency
Bike_Make_f <- cleaned_data %>% 
  count(Bike_Make, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A7. Bike Model Frequency
Bike_Model_f <- cleaned_data %>% 
  count(Bike_Model, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A8. Bike Type Frequency
Bike_Type_f <- cleaned_data %>% 
  count(Bike_Type, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A9. Bike Speed Frequency
Bike_Speed_f <- cleaned_data %>% 
  count(Bike_Speed, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A10. Bike Colour Frequency
Bike_Colour_f <- cleaned_data %>% 
  filter(!is.na(Bike_Colour)) %>% 
  count(Bike_Colour, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A11. Bike Cost Frequency
Bike_Cost_f <- cleaned_data %>% 
  count(Cost_of_Bike, name = "Frequency", sort = TRUE)

# Plotting the bike cost frequency into a point graph.
Bike_Cost_f %>% 
  ggplot(mapping = aes(Cost_of_Bike, Frequency)) +
  geom_point() +
  theme_minimal()

## A12. Status Frequency
Status_f <- cleaned_data %>% 
  count(Status, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

## A13. Neighborhood Frequency
Neightborhood_f <- cleaned_data %>% 
  count(Neighbourhood, name = "Frequency", sort = TRUE) %>% 
  top_n(10)


### B. Relational Analysis ###


## B1. Date by Primary Offence
Date_by_Offence <- cleaned_data %>%
  filter(cleaned_data$Primary_Offence %in% Offence_f$Primary_Offence) %>% 
  group_by(Primary_Offence) %>% 
  count(Occurrence_Date, name = "Frequency")

# Plotting the date by offence into a line graph.
Date_by_Offence %>% 
  ggplot(mapping = aes(x = Occurrence_Date, y = Frequency, colour = Primary_Offence)) + 
  geom_line() +
  theme_minimal()

## B2. Recovered Thefts Cost
# Plotting the status by bike price into a line graph.
cleaned_data %>% 
  select(Status, Cost_of_Bike) %>% 
  ggplot(mapping = aes(x = Status, y = Cost_of_Bike )) +
  geom_point()
  theme_minimal()
  
## B3. Recovered Bikes by Offence
Recovered_by_Offence <- cleaned_data %>% 
  filter(Status == "RECOVERED") %>% 
  count(Primary_Offence, name = "Recovered", sort = TRUE) %>% 
  top_n(10)

## B4. Recovered by Location Type
Recovered_by_Location_Type <- cleaned_data %>% 
  filter(Status == "RECOVERED") %>% 
  count(Location_Type, name = "Recovered", sort = TRUE) %>% 
  top_n(10)

## B5. Recovered by Bike Type
Recovered_by_Bike_Type <- cleaned_data %>% 
  filter(Status == "RECOVERED") %>% 
  count(Bike_Type, name = "Recovered", sort = TRUE) %>% 
  top_n(10)

## B6. Recovered by Bike Colour
Recovered_by_Bike_Colour <- cleaned_data %>% 
  filter(Status == "RECOVERED") %>% 
  count(Bike_Colour, name = "Recovered", sort = TRUE) %>% 
  top_n(10)

colour_recovered <- merge(Bike_Colour_f, Recovered_by_Bike_Colour, by = "Bike_Colour") %>% 
  mutate(Recovery_Rate = Recovered / Frequency) %>% 
  arrange(desc(Recovery_Rate))

## B7. Cost of Bike Recovery Plot
cleaned_data %>% 
  group_by(Primary_Offence) %>% 
  summarise(mean_bike_cost = mean(Cost_of_Bike)) %>% 
  ungroup() %>% 
  arrange(desc(mean_bike_cost)) %>% 
  #top_n(10) %>% 
  kable(col.names = c("Primary Offence", "Mean Bike Cost"), caption = "Bike Cost by Primary Offence")

## B8. Recovered by Bike Type
type_recovered <- merge(Bike_Type_f, Recovered_by_Bike_Type, by = "Bike_Type") %>% 
  mutate(Recovery_Rate = Recovered / Frequency) %>% 
  arrange(desc(Recovery_Rate))