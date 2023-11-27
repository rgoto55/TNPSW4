
## This script prepares the Tanzania Household Budget Survey (2018) NCT to
# be used for allocating the weights of each food consumed in the 
# Tanzania HCES NCT 


# Library loading
library(tidyverse)

# Data loading
hbs_nct <- read.csv(here::here("data", "HBS18_TZNCT_v.2.0.1.csv")) %>% 
  filter(!is.na(ID_3))

dictionary.df <- readRDS(here::here("metadata", "dictionary.df.rds"))


# Data cleaning (HBS NCT) -----

# Renaming variables

names(hbs_nct) <- c("food_id", "food_desc", "n" , "dict_testsample_code", "FoodName_3",
                    "Confidence", "wt")

# Fixing dict codes (cassava flour & pork lard)
hbs_nct$dict_testsample_code[hbs_nct$dict_testsample_code == "23170.01"] <-  "23170.01.01"
hbs_nct$dict_testsample_code[hbs_nct$dict_testsample_code == "F1243.02"] <-  "21521.01"

# Excluding problematic items (missing in FCTs)

excluded <- c("1214.06", "1529.03", "1699.01", "1214.06", "111.02", 
              "2168.01",  "22290.02", "22290.03" ,"22290.02", "22290.03", 
              "23914.05",  "1319.04", 
              "21170.92.07", "21170.92.08", "1699.08")

subset(dictionary.df, ID_3 %in% excluded)
subset(dictionary.df, grepl("Zam", Description2, ignore.case = TRUE))
hbs_nct <- subset(hbs_nct, !dict_testsample_code %in% excluded, 
                  select = c(1:6))

## Excluding duplicated items
# rice (23161.02.01), soft drinks (24490.02)
hbs_nct[, c("food_id", "food_desc", "dict_testsample_code")] %>% 
  group_by(dict_testsample_code) %>% 
  count() %>% arrange(desc(n))

hbs_nct %>% filter( dict_testsample_code == "24490.02") %>% 
  select(food_id, n)

# Selecting for duplicated items those with the highest no. of HHs reporting.
hbs_short <- hbs_nct[, c("food_id", "food_desc", "dict_testsample_code", "n")] %>% 
  group_by(dict_testsample_code) %>% summarise(n = max(n)) %>% 
  left_join(., hbs_nct)


# Adding dict names (complete)
hbs_short <- hbs_short %>% select(!c(FoodName_3)) %>% 
  left_join(., dictionary.df[, c("ID_3", "FoodName_3")],
            by = c("dict_testsample_code" = "ID_3"))

# Checking ID_3 & Names
hbs_short %>% filter(is.na(dict_testsample_code))
hbs_short %>% filter(is.na(FoodName_3))

# TODO: Removing foods w/o info on the dictionary
#hbs_nct <- hbs_nct %>% filter(!is.na(FoodName_3))

# Fixing codes
# Checking duplicated codes (used same item for different indv. reported foods)
# Rice - reported different varieties (we used the same code)
# Sugary drinks (cola, fanta, etc.) - reported as indiv. but used the same code
# Chicken - 4 different items (frozen, live, etc.)
# Jam - 4 kinds (used the same item)

hbs_short %>% count(dict_testsample_code, FoodName_3) %>% arrange(desc(n))

# Saving the food items, frequency of consumption, & dict id for HBS18 
saveRDS(hbs_short, here::here("inter-output", "hbs18-tznct-weights_v.3.0.0.rds"))

                  