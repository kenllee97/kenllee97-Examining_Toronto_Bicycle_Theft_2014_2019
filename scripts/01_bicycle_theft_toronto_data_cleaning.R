#### Preamble ####
# Purpose: Conduct exploratory analysis on the data.
# Author: Ken Lee
# Data: 27 January 2021
# Contact: kenllee97@gmail.com
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the "Bicycle Thefts" data and saved it to inputs/data


### Workspace setup ###
# Use R Projects, not setwd().
library(tidyverse)
library(janitor)

# Load/Read in the raw data.
raw_data <- readr::read_delim("inputs/data/raw_data.csv", delim = ","
                              )

# Checking the number of NA values for each column.
NA_values <- raw_data %>% 
  is.na() %>% 
  colSums()

print(NA_values) #Here we can see that the latitude has significantly higher 
# number of NA values.

# Removing Lat column while keeping Bike_Make, Bike_Model, Bike_Colour, and Cost_of_Bike
# since they did not have that many NA values and they are vital features to 
# look into.
# Long and geometry are also removed since we won't be using these for the analysis.
cleaned_data <- subset(raw_data, select = -c(Lat, Long, geometry))

# Checking for duplicates
get_dupes(cleaned_data)
# No duplicates found

### Save Data ### 
write_delim(cleaned_data, "inputs/data/cleaned_data.csv", na = "NA", delim = ",")

write_delim(cleaned_data, "outputs/paper 1/cleaned_data.csv", 
            na = "NA", delim = ",") # Saving cleaned data into the paper folder
