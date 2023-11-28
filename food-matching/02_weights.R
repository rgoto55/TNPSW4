
## This scripts assign the dictionary codes & 
## prepare the weights of the multiple items/matches for the TZ HCES 

# Cleaning the environment before running the scripts. 
rm(list = ls())

# Loading libraries & functions
library(dplyr)
library(stringr)
library(tidyr)
# library(tidyverse)
# library(data.table)
#library(openxlsx) # important link: https://www.rdocumentation.org/packages/openxlsx/versions/4.2.5.1

# Loading data

# HBS18 cleaned
# This finds the most recent version of the HBS18
file <-  sort(grep("hbs18-tznct-weights", list.files(here::here("inter-output")),
                           value = TRUE), decreasing = TRUE)[1]
hbs_nct <- readRDS(here::here("inter-output", file))
names(hbs_nct)

# TZ HCES W4 food list
food_consumed <- readRDS(here::here("inter-output", "tz-hces-w4_food-list_v.1.0.0.rds"))

# Food dictionary
dictionary.df <- readRDS(here::here("metadata", "dictionary.df.rds"))

# Multiple matches & weights ----

## Example of how the weights were obtained & calculated

subset(hbs_nct, str_detect(food_desc, " oil")) %>% # Selecting the food group "oils"
 # separate_rows(dict_testsample_code) %>%  # Getting indiv. dict. codes
  add_count(food_id) %>%                  # Counting indiv. dict. codes per food id.
  filter(str_detect(dict_testsample_code, "^216")) %>%  # Selecting only oils (exc. butter & seeds)
  mutate(N = (n/nn),                            # Getting HH per food id.
         wt = N/sum(N),                        # Getting weights for each oil (contribution to total)    
         item_id = "1001") # %>% View()         # Adding the item_id        


# Generating the matrix to store the information above: 
# item_id ("NPSDSS_code"), 
# key words related to the food items in the itemcode, eg, "oil" ("food"), 
# information on the food group that it belows to in HBS dataset ("group_code")
# dictionary ids. to be removed from the selection ("code")

# primary_df <- data.frame(matrix(ncol = 4, nrow = 0)) # matrix w/ 4 columns (as the variables), and empty rows
# colnames(primary_df) <- c("NPSDSS_code", "food", "group_code", "code") # names of the columns

# List for storing the variables above
NPSDSS_code_list <- list()
food_list <- list()
group_code_list <- list()
code_list <- list()

### cooking oil ----

NPSDSS_code <- "1001"
food <- " oil"
group_code <- "^115"
code <- c("1445.01", "1444.01", "115101")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code 
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### ONIONS, TOMATOES, CARROTS AND GREEN PEPPER, OTHER VIUNGO ----
NPSDSS_code <- "601"
food <- "onion|tomato|carrot|pepper"
group_code <- ""
code <- c("117302", "119102")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code 
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code


### MAIZE (FLOUR) ----
NPSDSS_code <- "105"
food <- "maize"
group_code <- ""
code <- c("111501", "111503", "111513", "111502")
#code <- toString(c("111501", "111503", "111513", "111502"))

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code 
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### SUGAR ----
NPSDSS_code <- "301"
food <- "sugar"
group_code <- ""
code <- c("116808", "118104")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code


### SPINACH, CABBAGE AND OTHER GREEN VEGETABLES ----
NPSDSS_code <- "602"
food <- "spinach|cabbage|lea"
group_code <- ""
code <- c("121201", "117109")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

###  PEAS, BEANS, LENTILS AND OTHER PULSES ----
NPSDSS_code <- "401"
food <- "pea|bean|lentil|pulse"
group_code <- ""
code <- c("117107", "115202", "116401", "121102", "116506", "117323")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

subset(hbs_nct, grepl(food, food_desc, ignore.case = T))

subset(hbs_nct, grepl(food, food_desc, ignore.case = T)) %>% #Selecting food
  separate_rows(dict_testsample_code) %>%  #Getting indiv. dict. codes
  add_count(food_id) %>%                  #Counting indiv. dict. codes per food id.
  filter(!food_id %in% code) %>%  #Removing foods (by food_id)
  mutate(N = (n/nn),                            # Getting HH per food id.
         wt = N/sum(N),              # Getting weights for each food (contribution to total of that category)
         itemcode = NPSDSS_code) #%>% View()              

###  FRESH FISH AND SEAFOOD (INCLUDING DAGAA) ----
NPSDSS_code <- "808"
food <- "fresh|prawn|oct|ngisi|Other sea foods"
group_code <- "113"
code <- c("113305")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

###  DRIED/SALTED/CANNED FISH AND SEAFOOD (INCL. DAGAA) ----
NPSDSS_code <- "809"
food <- "dried|salt|can|tin"
group_code <- "^113"
code <- c("113305")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

###  BUNS, CAKES AND BISCUITS ----
NPSDSS_code <- "110"
food <- "bun|cake|pastry|biscu"
group_code <- "^111"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

###  BEEF INCLUDING MINCED SAUSAGE ----
NPSDSS_code <- "802"
food <- "beef"
group_code <- "^112"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

###  CITRUS FRUITS (ORANGES, LEMON, TANGERINES, ETC.) ----
NPSDSS_code <- "702"
food <- "orange|lemon|tangerin|citrus|lime|grapefruit"
group_code <- "^116"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

###  COCONUTS (MATURE/IMMATURE) ----
NPSDSS_code <- "502"
food <- "coconut"
group_code <- "^116"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### MANGOES, AVOCADOES AND OTHER FRUITS ----
NPSDSS_code <- "703"
food <- ""
group_code <- "^116"
code <- c("116101", "116104", "116103",  "116199", "116102", "116105", "116804",
          "116805", "116201", "116808", "116801", "116802", "116803", "116899", 
          "116999", "116806", "116901", "116807", "116904", "116902")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### SWEET POTATOES ----
NPSDSS_code <- "203"
food <- "sweet potato"
group_code <- "^117"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### FRESH MILK ----
NPSDSS_code <- "901"
food <- "milk|fresh"
group_code <- "^114"
code <- c("114402", "114602", "114601", "114301", "114302", "114303", "114105", "114202")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### BREAD ----
NPSDSS_code <- "109"
food <- "bread|chapa"
group_code <- "^111"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### WHEAT FLOUR ----
NPSDSS_code <- "1081"
food <- "wheat flour"
group_code <- "^111"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### GROUNDNUTS IN SHELL/SHELLED ----
NPSDSS_code <- "501"
food <- "groundnut"
group_code <- "^116"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### BOTTLED/CANNED SOFT DRINKS (SODA, JUICE, WATER) ----
NPSDSS_code <- "1104"
food <- "soft|juice|water"
group_code <- "^122"
code <- c("122101")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### CHICKEN AND OTHER POULTRY ----
NPSDSS_code <- "804"
food <- "chicken|poultr|duck|quail"
group_code <- "^112"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### CASSAVA DRY/FLOUR ----
NPSDSS_code <- "202"
food <- "cassava flour|cassava dr"
group_code <- "^117"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### OTHER SPICES ----
NPSDSS_code <- "1004"
food <- "spice|ginger|curry"
group_code <- "^119"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### MILK PRODUCTS (LIKE CREAM, CHEESE, YOGHURT ETC) ----
NPSDSS_code <- "902"
food <- "cream|cheese|yogu|clott|beverage"
group_code <- "^114"
code <- c("114402")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### MACARONI, SPAGHETTI ----
NPSDSS_code <- "111"
food <- "macar|spag"
group_code <- "^111"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### YAMS/COCOYAMS ----
NPSDSS_code <- "204"
food <- "yam"
group_code <- "^117"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### MAIZE (GRAIN) ----
NPSDSS_code <- "104"
food <- "maize grain"
group_code <- "^111"
code <- c("111513")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### MILLET AND SORGHUM (FLOUR) ----
NPSDSS_code <- "107"
food <- "millet flour|sorghum flour"
group_code <- "^111"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### HONEY, SYRUPS, JAMS, MARMALADE, JELLIES, CANNED FRUITS ----
NPSDSS_code <- "303"
food <- "honey|syrup|jam|marmalade|jell|canned fruit"
group_code <- "^118|^116"
code <- c("118204", "118202", "118203")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### GOAT MEAT ----
NPSDSS_code <- "801"
food <- "goat meat"
group_code <- "^112"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### CANNED, DRIED AND WILD VEGETABLES ----
NPSDSS_code <- "603"
food <- "canned|dried|wild"
group_code <- "^117"
code <- c("117110")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### PORK INCLUDING SAUSAGES AND BACON ----
NPSDSS_code <- "803"
food <- "pork|pig|swine|bacon"
group_code <- "^112"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### BUTTER, MARGARINE, GHEE AND OTHER FAT PRODUCTS ----
NPSDSS_code <- "1002"
food <- "butter|margarine|ghee|fat"
group_code <- "^115"
code <- c("115202")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### OTHER CEREAL PRODUCTS ----
NPSDSS_code <- "112"
food <- "flour"
group_code <- "^111"
code <- c("111504", "111506", "111510", "111508")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### CASHEW, ALMONDS AND OTHER NUTS ----
NPSDSS_code <- "503"
food <- "cashe|almond| nut"
group_code <- "^116"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### COFFEE AND COCOA ----
NPSDSS_code <- "1102"
food <- "coffe|cocoa|choco"
group_code <- "^121"
code <- c("121104")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### CANNED MILK/MILK POWDER ----
NPSDSS_code <- "903"
food <- "cann|tinn|powder|evapo"
group_code <- "^114"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### OTHER STARCHES ----
NPSDSS_code <- "207"
food <- "tuber"
group_code <- "^117"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### RICE (PADDY) ----
NPSDSS_code <- "101"
food <- "paddy"
group_code <- "^111"
code <- c("")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### WHEAT, BARLEY GRAIN AND OTHER CEREALS ----
NPSDSS_code <- "1082"
food <- "wheat grain|barley|Other  grain"
group_code <- "^111"
code <- c("111599")


NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### MILLET AND SORGHUM (GRAIN) ----
NPSDSS_code <- "106"
food <- "millet grain|sorghum grain"
group_code <- "^111"
code <- c("")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### SEEDS AND PRODUCTS FROM NUTS/SEEDS (EXCL. COOKING OIL) ----
NPSDSS_code <- "504"
food <- "seed|peanut"
group_code <- "^115"
code <- c("115402")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code


### OTHER DOMESTIC/WILD MEAT PRODUCTS ----
NPSDSS_code <- "806"
food <- "wild animal|edible meat"
group_code <- "^112"
code <- c("")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### PREPARED TEA, COFFEE ----
NPSDSS_code <- "1105"
food <- "coffee|tea"
group_code <- "^121"
code <- c("121201", "121102", "121101") 

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code


### WILD BIRDS AND INSECTS ----
NPSDSS_code <- "805"
food <- "bird|insect"
group_code <- "^112"
code <- c("")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

### SWEETS ----
NPSDSS_code <- "302"
food <- "choco|sweets|cocoa|confe|chew"
group_code <- "^118"
code <- c("")

NPSDSS_code_list[[length(NPSDSS_code_list)+1]] <- NPSDSS_code
food_list[[length(food_list)+1]] <- food
group_code_list[[length(group_code_list)+1]] <- group_code
code_list[[length(code_list)+1]] <- code

# This was used to check the food descriptors and group codes (above)
#subset(hbs_nct, grepl(food, food_desc, ignore.case = T)) 
#subset(hbs_nct, str_detect(food_id, group_code))

## Generating the dataset with the foods & weights ----

# Checking the final structure & getting the variables that will be used 
# in the final dataset

column_name <- subset(hbs_nct[,c(1:6)], grepl(food, food_desc, ignore.case = T) & #Selecting food by desc.
                        str_detect(food_id, group_code)) %>%               #by group id 
  separate_rows(dict_testsample_code) %>%  #Getting indiv. dict. codes
  add_count(food_id) %>%                  #Counting indiv. dict. codes per food id.
  filter(!food_id %in% code[[1]]) %>%  #Removing foods (by food_id)
  mutate(N = (n/nn),                            # Getting HH per food id.
         wt = N/sum(N),              # Getting weights for each food (contribution to total of that category)
         itemcode = NPSDSS_code) %>%  names()  


# Generating a matrix to store the data ncol = length(column_name)
length(column_name)
output_df <- data.frame(matrix(ncol = 10, nrow = 0))

## Storing all the lists with the information for the 45 multiple matched foods
# in the Wave 4 predefined list & their weights into a list 

primary <- list()
primary[1] <- list(NPSDSS_code_list) 
primary[2] <- list(food_list)
primary[3] <- list(group_code_list)
primary[4] <- list(code_list)

# Loop over all the set of variables to select the foods that will conform the weight per each of the 
# 45 foods in the predefined list (primary[[1]] == list of NPSDSS_code_list) to complete
# the output dataset. 

for (i in 1:length(primary[[1]])){
  
  output_chunk <- subset(hbs_nct[,c(1:6)],
                    grepl(paste0(primary[[2]][[i]]), food_desc, ignore.case = T) &
                           grepl(primary[[3]][[i]], food_id)) %>%
    separate_rows(dict_testsample_code) %>%
    add_count(food_id) %>%
    filter(!food_id %in% primary[[4]][[i]]) %>%
    mutate(N = (n/nn),
           wt = N/sum(N),
           itemcode = primary[[1]][[i]])
  
  output_df <- data.table::rbindlist(list(output_df, output_chunk))
}

#View(output_df)

#Renaming the columns as per above & changing the class of variables for merging
names(output_df) <- column_name
output_df$itemcode <- as.integer(output_df$itemcode)
food_consumed$item_id <- as.integer(food_consumed$item_id)

# Checking foods and their food matches
output_df %>% left_join(., food_consumed, by = c("itemcode" = "item_id")) %>% View()

# Joining (right_join) to add all food consumed (w/ & w/o dict id and weight)
output_df <- output_df %>% 
  right_join(., food_consumed, by = c("itemcode" = "item_id")) %>% 
  #select(c(4, 5, 6, 7, 8, 9, 10, 11)) %>%
  relocate(c("itemcode", "itemcode.y"), .before = "FoodName_3") %>%  # reorganising variables
  rename(item_desc = itemcode.y, N_HH = n.y, ID_3 = dict_testsample_code) # renaming

# Checking foods w/o dict id
output_df %>% 
  filter(is.na(ID_3)) #%>% View()

length(unique(output_df$itemcode))

## Adding the ID_3 codes for those w/o matches (mainly Single matches items) ----

# Checking dict. ids for the matches
dictionary.df %>% filter(grepl("fish", FoodName_2, ignore.case = TRUE))

# Getting the food ids of the foods w/o dict matches
food <- output_df %>% filter(is.na(ID_3)) %>% pull(itemcode)
output_df %>% filter(itemcode %in% food) %>% select(itemcode, item_desc, perc)

# Getting the dict ids (ID_3) for those above.
dict_list <- c("1699.02", "23161.02.01", "1620.01", "1520.01.01", "1313.01", "1510.01", 
               "1312.01", "231.01", "1802.01", "1290.01.01",  "24310.04.01", 
                "24310.01.01", NA, NA, NA)
dictionary.df %>% filter(ID_3 %in% dict_list) %>% select(ID_3, FoodName_3)

# Checking that both vectors have the same length (must for next step)
length(food) == length(dict_list)

# Loop: Adding the dict codes to the foods
for(i in 1:length(food)){
  
  output_df$ID_3[output_df$itemcode %in% food[i]] <- dict_list[i]
}

# Checking foods w/o dict id
output_df %>% filter(is.na(ID_3))

# Adding manually values for the missing & fixing other ----

# Other raw materials (NA), package fish (NA)
# Sweets, Wheat and barley & Wine and spirits
#output_df$ID_3[output_df$itemcode == "302"] <- "23670.01.01, F0666.01"
#output_df$wt[output_df$itemcode == "302"] <- 1/2
#output_df$ID_3[output_df$itemcode == "1082"] <- "111.01, 23140.05.01"
#output_df$wt[output_df$itemcode == "1082"] <- 0.5
output_df$ID_3[output_df$itemcode == "1108"] <- "24212.02.01, 24212.02.02, 2413.01"
output_df$wt[output_df$itemcode == "1108"] <- 1/3

# Fixing codes: Maize
output_df$ID_3[output_df$itemcode == "105"] <- c("23120.03.02, 23120.03.01, 23120.03.03")
output_df$wt[output_df$itemcode == "105"] <- 1/3


output_df <- output_df %>% separate_rows(ID_3) %>% 
  mutate(ID_3 = str_squish(ID_3))

# Fixing some issues
output_df$item_desc[output_df$itemcode == "504" &  output_df$ID_3 == "21631.01.01"]
output_df$item_desc[output_df$itemcode == "1001" & (output_df$ID_3 == "1445.01"|output_df$ID_3 == "1444.01")]
output_df <- output_df %>% filter(!(itemcode == "504" &  (output_df$ID_3 == "21631.01.01" |output_df$ID_3 =="21691.07.01")) &
                                    !(itemcode == "1001" & (output_df$ID_3 == "1445.01"|output_df$ID_3 == "1444.01")))

# Documentation: Adding dict food description (FoodName_3) to all food items
output_df <- output_df %>% select(-FoodName_3) %>% 
  left_join(., dictionary.df %>% select(ID_3, FoodName_3) %>%
              filter(grepl("\\b", ID_3))) %>% 
  relocate(., FoodName_3, .after = ID_3) %>% 
  relocate(., c(ID_3, FoodName_3), .after = item_desc)

## Quality checks ----

dictionary.df %>% filter(grepl("biscu", FoodName_3, ignore.case = TRUE))
dictionary.df %>% filter(grepl("F1243", ID_3, ignore.case = TRUE))
dictionary.df %>% filter(grepl("2618", ID_1, ignore.case = TRUE))

# Checking foods without codes in the dictionary (this should be solved by the TODO in 00_hbs18.R)
# Checking if the matches (ID_3) are up to date
output_df[, c("item_desc", "ID_3")] %>% 
  left_join(., dictionary.df %>%
              filter(stringr::str_detect(FoodName_3, "\\b")), by = "ID_3") %>%
  filter(is.na(FoodName_3)) %>% View()


# Checking duplicated items (ID_3)
# 1290.9.05
output_df[, c("itemcode", "item_desc", "ID_3")] %>% 
  group_by(itemcode, item_desc, ID_3) %>% 
  count() %>% arrange(desc(n))


## Checking the weights ----

# Check 3 dict names and check matches to TZ FCT w/ dictionary. 
# Check F0022.08 - mandazi (missing)
# 23170.01, 21491.01, 1699.08

# Writing food matches between the two surveys
# write.csv(output_df, here::here("documentation", "hbs_tz_food-matches_v.3.0.0.csv"), 
#           row.names = FALSE)

# Selecting variables of interest for the matching

output_df <- output_df %>% 
  rename(food.desc = "food_desc") %>% select(5:6, 12, 7:8, 4, 11)

# Renames data to be merged with FCTs
colnames(output_df) <- c("item_id", "item_desc", "n", "ID_3", 
                       "FoodName_3", 
                       "Confidence", "Weighting_Factor")

# Checking weights:
output_df %>% group_by(item_id) %>% 
  summarise(total = sum(Weighting_Factor)) %>% filter(total<1)

# Recalculation for <1
n <- sum(output_df$Weighting_Factor[output_df$item_id == "1001"])
output_df$Weighting_Factor[output_df$item_id == "1001"] <- output_df$Weighting_Factor[output_df$item_id == "1001"]/n
n <- sum(output_df$Weighting_Factor[output_df$item_id == "504"])
output_df$Weighting_Factor[output_df$item_id == "504"] <- output_df$Weighting_Factor[output_df$item_id == "504"]/n


# Checking duplicated values. 
sum(duplicated(output_df[, c("item_id", "ID_3")]))

output_df %>% select(c("item_id", "ID_3")) %>% distinct()

output_df <- output_df %>% group_by(item_id, item_desc, ID_3, FoodName_3, Confidence) %>% 
  summarise(Wt = sum(Weighting_Factor, na.rm = TRUE)) %>% 
  mutate(Wt = ifelse(Wt ==0, 1, Wt)) # Change weight NA for 1 


# Checking weights:
output_df %>% group_by(item_id) %>% 
  summarise(total = sum(Wt)) %>% filter(total>1)

#sum(1.098/(nct_duplicates$Wt[nct_duplicates$item_id == "1105"])/100)
#sum(nct_duplicates$Wt[nct_duplicates$item_id == "1105"])
sum(output_df$Wt[output_df$item_id == "1001"])
#sum(all_matches$Wt[all_matches$item_id == "1105"])

# Re-scaling for >1 
#output_df$Wt[output_df$item_id == "1105"] <- (output_df$Wt[output_df$item_id == "1105"])/0.999
#output_df$Wt[output_df$item_id == "603"] <-  (output_df$Wt[output_df$item_id == "603"])/0.999
#output_df$Wt[output_df$item_id == "1001"]  <-  (output_df$Wt[output_df$item_id == "1001"])/0.9999


# View(output_df)

# Writing food dict matches 
write.csv(output_df, here::here("inter-output", "tz_food-matches_v.2.0.0.csv"), 
          row.names = FALSE)

