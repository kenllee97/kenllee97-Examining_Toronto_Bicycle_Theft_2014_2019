### Preamble ###
# Purpose: Using opendatatoronto to obtain the bicycle theft in Toronto data
# Author: Ken Lee
# Contact: kenllee97@gmail.com
# Date: 26 January 2021
# Prerequisites None
# TODOs: -

### Workspace set-up ###
install.packages("opendatatoronto")
install.packages("tidyverse")
library(opendatatoronto)
library(tidyverse)

###Get Data###
raw_data <- 
  opendatatoronto::search_packages("Bicycle Thefts") %>% 
  opendatatoronto::list_package_resources() %>% 
  filter(name == "Bicycle Thefts Data") %>% # This is the row we are interested in.
  select(id) %>% 
  opendatatoronto::get_resource()

### Save Data ###  
write.csv(raw_data, "inputs/data/raw_data.csv")
