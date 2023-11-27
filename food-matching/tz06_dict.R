
# Loading libraries & functions
library(dplyr)
# source("Fuzzy_Matcher.R")
library(NutritionTools) # this substitute the Fuzzy_Matcher script

# Loading data 
tz06 <- read.csv(here::here("data", "TZ08_tidied_FCT_v2.csv")) %>% 
  rename(VITB12mcg = "VITB12mg") #wrong units

#fcts <- read.csv(here::here("data", "FCTs_dict_compiled_v1.0.0.csv"))
dictionary.df <- readRDS(here::here("metadata", "dictionary.df.rds"))

# Checking data
head(tz06)

# Changing data type
#tz06$fdc_id <- as.character(tz06$fdc_id)

# First matches (fuzzy)
#Fuzzy_Matcher(tz06[,c(1:2)], dictionary.df[,c(7,9)])

# Loading the matches performed with fuzzy
tz_matches  <- read.csv(here::here("inter-output", "tz_dict_matches.csv"))
names(tz_matches)

# fixing some matches
tz_matches$ID_3[tz_matches$fdc_id == "6"] <- "F0022.13"
tz_matches$ID_3[tz_matches$fdc_id == "113"] <- "1316.05"
tz_matches$ID_3[tz_matches$fdc_id == "423"] <- "1234.02"
tz_matches$ID_3[tz_matches$fdc_id == "1114"] <- "1460.01"

#Removing incorrect matches
tz_matches <-  subset(tz_matches, !fdc_id %in%c("1108", "172"))


# checking matches
dictionary.df %>% filter(grepl("", FoodName_3, ignore.case = TRUE) &
                           grepl("seed", FoodName_3, ignore.case = TRUE))



dictionary.df %>% filter(grepl("^1214.04", ID_3, ignore.case = TRUE))
dictionary.df %>% filter(grepl("^2630", ID_1, ignore.case = TRUE))
tz_matches %>% filter(grepl("1234.02", ID_3, ignore.case = TRUE))
#tz06_dict %>% filter(grepl("1290.9.027", ID_3, ignore.case = TRUE))
#output_df %>% filter(grepl("1234.03", ID_3, ignore.case = TRUE))

class(tz_matches$fdc_id)
#tz_matches$fdc_id <- as.character()

# Checking missing dict codes (from fuzzy) in FCT

tz06 %>% left_join(., tz_matches) %>% 
  filter(is.na(ID_3) &  Food_Group_Code == "A1") # %>% View()

dict_codes <- tribble(
  ~fdc_id,   ~ID_3, ~Confidence, 
  "11", "112.01", "medium",
  "12", "23120.03.02", "high", 
  "14", "1290.01.01", "medium",
  "15", "23120.03.03", "high", 
  "16", "118.03", "high",
  "22", "23120.01.0", "high",
  "29", "23110.01", "high",
  
 
  "114", "1323.01", "high",  
  "118", "1342.01.01", "high", 
  "121", "1346.01", "medium", 
  "132", "1319.05", "high", 
  
  "160", "1460.02", "high", 
  "162", "1241.9.01", "high", 
  "163", "1706.01", "high", 
  "164", "21421.01", "high", 
  "168", "141.01", "medium", 
  "173", "1709.9.02", "high", 
  
  "204", "21111.02.02", "high",
  "208", "21121.03", "high", 
  "255", "2211.01", "high", 
  
  "312", "1527.02", "medium", 
  
  "421", "1290.9.04", "medium", 
 # "423", "1234.02", "high", 
  "425", "1241.9.02", "medium",
  "455", "F0666.01", "medium" ,
  
  "519", "F1232.17", "low", 
  "534", "F0022.11", "high",
  
  "1001", "23914.01", "low", 
  "1004", "23912.02.02", "medium", 
 # "1006", "23912.02.02", "medium", 
 # "1109", "21691.07.01", "high",
  "1102", "22241.02.01", "medium",
  )

# Merging fuzzy and manual dict
dict_codes <- tz_matches %>% rbind(., dict_codes)

#Checking matches
sum(duplicated(dict_codes[,1]))
dict_codes[which(duplicated(dict_codes[,1])),]

# Changing data type
tz06$fdc_id <- as.character(tz06$fdc_id)  

# Merging FCT & dict codes ()
tz06_dict <- tz06 %>% left_join(., dict_codes) %>% 
  relocate(., c("ID_3", "Confidence"), .after = food_description) %>% 
  rename(food_desc = "food_description")

# Excluding items (ID_3 to NA) - Quality issues
tz06_dict$ID_3[grepl("Sweet potato", tz06_dict$food_desc, ignore.case = TRUE)] <- NA

# Checking values
tz06_dict %>% 
  filter(grepl("pump", food_desc, ignore.case = TRUE) &
           grepl("", food_desc, ignore.case = TRUE)) # %>%  
#   select(fdc_id , food_desc, ID_3, ENERCkcal, VITA_RAEmcg, FAT_g) %>% 
#  arrange(desc(VITA_RAEmcg))

