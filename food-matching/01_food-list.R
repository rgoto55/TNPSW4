
## This script extracts and explores the food list of the TZ HCES dataset


#Library loading
library(tidyverse)

#Data loading

#hh_data <- read.csv(here::here("data", "HH_SEC_J1.csv"), header = TRUE) # Wave 5
hh_data <- readxl::read_excel(here::here("data", "hh_sec_j1_1.xlsx")) # Wave 4
item_code <- read.csv(here::here("data", "food-id.csv"), header = TRUE)

names(hh_data)

# Checking number of HHs reported
# n <- length(unique(hh_data$sdd_hhid)) # W5
N <- length(unique(hh_data$y4_hhid)) # W4

# Checking the unique foods 
hh_data %>% distinct(itemcode)

# HH data sub-setting
subset(hh_data, hh_j01 == "YES") # Only food consumed

# Generating the food list ----

## No. of HH consuming each food (yes == 1)
food_consumed <- subset(hh_data, hh_j01 == "YES" | hh_j01 == "1") %>% 
  count(itemcode) %>% arrange(desc(n)) %>%  
  rename(item_id = "itemcode")

## Adding the food item code
(food_consumed <- food_consumed %>% left_join(., item_code) %>% 
  relocate(n, .after = itemcode) %>% 
    mutate(perc = n/N))


#setdiff(item_code[,2], food_consumed[,2])


# Checking the food list & highly consumed foods
food_consumed %>% filter(perc < 0.01)

# Saving the food items, frequency of consumption & perc Wave 4 
saveRDS(food_consumed, here::here("inter-output", "tz-hces-w4_food-list_v.1.0.0.rds"))


