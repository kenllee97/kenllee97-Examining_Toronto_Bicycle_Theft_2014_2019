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

# Saving processed data
write_delim(Offence_f, "outputs/exploratory_code/a1_offence_frequency.csv", 
            na = "NA", delim = ",") 

## A2.  Date Frequency
Date_f <- cleaned_data %>% 
  count(Occurrence_Date, name = "Frequency")

# Saving processed data
write_delim(Date_f, "outputs/exploratory_code/a2_date_frequency.csv", 
            na = "NA", delim = ",") 

# Plotting the date occurrence frequency into a line graph.
Date_f %>% 
  ggplot(mapping = aes(Occurrence_Date, Frequency)) + 
  geom_line() +
  theme_minimal()

## A3. Time Frequency
Time_f <- cleaned_data %>% 
  count(Occurrence_Time, name = "Frequency")

# Saving processed data
write_delim(Time_f, "outputs/exploratory_code/a3_time_frequency.csv", 
            na = "NA", delim = ",") 

# Plotting the time occurrence frequency into a line graph.
Time_f %>% 
  ggplot(mapping = aes(Occurrence_Time, Frequency)) + 
  geom_line() +
  theme_minimal()

## A4. Division Frequency
Division_f <- cleaned_data %>% 
  count(Division, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Division_f, "outputs/exploratory_code/a4_division_frequency.csv", 
            na = "NA", delim = ",") 

## A5. Location Type Frequency
Location_Type_f <- cleaned_data %>% 
  count(Location_Type, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Location_Type_f, "outputs/exploratory_code/a5_location_type_frequency.csv", 
            na = "NA", delim = ",") 

## A6. Bike Maker Frequency
Bike_Make_f <- cleaned_data %>% 
  count(Bike_Make, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Bike_Make_f, "outputs/exploratory_code/a6_bike_make_frequency.csv", 
            na = "NA", delim = ",") 

## A7. Bike Model Frequency
Bike_Model_f <- cleaned_data %>% 
  count(Bike_Model, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Bike_Model_f, "outputs/exploratory_code/a7_bike_model_frequency.csv", 
            na = "NA", delim = ",") 

## A8. Bike Type Frequency
Bike_Type_f <- cleaned_data %>% 
  count(Bike_Type, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Bike_Type_f, "outputs/exploratory_code/a8_bike_type_frequency.csv", 
            na = "NA", delim = ",") 

## A9. Bike Speed Frequency
Bike_Speed_f <- cleaned_data %>% 
  count(Bike_Speed, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Bike_Speed_f, "outputs/exploratory_code/a9_bike_speed_frequency.csv", 
            na = "NA", delim = ",") 

## A10. Bike Colour Frequency
Bike_Colour_f <- cleaned_data %>% 
  filter(!is.na(Bike_Colour)) %>% 
  count(Bike_Colour, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Bike_Colour_f, "outputs/exploratory_code/a10_bike_colour_frequency.csv", 
            na = "NA", delim = ",") 

## A11. Bike Cost Frequency
Bike_Cost_f <- cleaned_data %>% 
  count(Cost_of_Bike, name = "Frequency", sort = TRUE)

# Saving processed data
write_delim(Bike_Cost_f, "outputs/exploratory_code/a11_bike_cost_frequency.csv", 
            na = "NA", delim = ",") 

# Plotting the bike cost frequency into a point graph.
Bike_Cost_f %>% 
  ggplot(mapping = aes(Cost_of_Bike, Frequency)) +
  geom_point() +
  theme_minimal()

## A12. Status Frequency
Status_f <- cleaned_data %>% 
  count(Status, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Status_f, "outputs/exploratory_code/a12_status_frequency.csv", 
            na = "NA", delim = ",") 

## A13. Neighborhood Frequency
Neightborhood_f <- cleaned_data %>% 
  count(Neighbourhood, name = "Frequency", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Neightborhood_f, "outputs/exploratory_code/a13_neighborhood_frequency.csv", 
            na = "NA", delim = ",") 

### B. Relational Analysis ###

## B1. Date by Primary Offence
Date_by_Offence <- cleaned_data %>%
  filter(cleaned_data$Primary_Offence %in% Offence_f$Primary_Offence) %>% 
  group_by(Primary_Offence) %>% 
  count(Occurrence_Date, name = "Frequency")

# Saving processed data
write_delim(Date_by_Offence, "outputs/exploratory_code/b1_date_by_offence.csv", 
            na = "NA", delim = ",") 

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

# Saving processed data
write_delim(Recovered_by_Offence, "outputs/exploratory_code/b3_recovered_by_offence.csv", 
            na = "NA", delim = ",") 

## B4. Recovered by Location Type
Recovered_by_Location_Type <- cleaned_data %>% 
  filter(Status == "RECOVERED") %>% 
  count(Location_Type, name = "Recovered", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Recovered_by_Location_Type, "outputs/exploratory_code/b4_recovered_by_location_type.csv", 
            na = "NA", delim = ",") 

## B5. Recovered by Bike Type
Recovered_by_Bike_Type <- cleaned_data %>% 
  filter(Status == "RECOVERED") %>% 
  count(Bike_Type, name = "Recovered", sort = TRUE) %>% 
  top_n(10)

# Saving processed data
write_delim(Recovered_by_Bike_Type, "outputs/exploratory_code/b5_recovered_by_bike_type.csv", 
            na = "NA", delim = ",") 

## B6. Recovered by Bike Colour
Recovered_by_Bike_Colour <- cleaned_data %>% 
  filter(Status == "RECOVERED") %>% 
  count(Bike_Colour, name = "Recovered", sort = TRUE) %>% 
  top_n(10)

colour_recovered <- merge(Bike_Colour_f, Recovered_by_Bike_Colour, by = "Bike_Colour") %>% 
  mutate(Recovery_Rate = Recovered / Frequency) %>% 
  arrange(desc(Recovery_Rate))

# Saving processed data
write_delim(colour_recovered, "outputs/exploratory_code/b6_recovered_by_bike_colour.csv", 
            na = "NA", delim = ",") 

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

# Saving processed data
write_delim(Recovered_by_Bike_Colour, "outputs/exploratory_code/b8_recovered_by_bike_type.csv", 
            na = "NA", delim = ",") 
