
## This scripts assign the dictionary codes & 
## prepare the weights of the multiple items/matches for the TZ HCES 

# Cleaning the environment before running the scripts. 
rm(list = ls())


## If NutritionTools is not installed
#
# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("TomCodd/NutritionTools")


# Loading data & libraries
library(NutritionTools) # Nutrition tools
library(dplyr) # Data wrangling 

# Taking the most updated version of the matches:
matches_file <-  sort(grep("tz_food-matches_v", 
                           list.files(here::here("inter-output")),
                           value = TRUE), decreasing = TRUE)[1]

output_df <- read.csv(here::here("inter-output", matches_file ))

# Selecting column of interest
names(output_df)

#output_df <- output_df[,c( "food_id",   "food_desc", "itemcode" ,  "item_desc", "ID_3"  ,    "FoodName_3", 
 #            "wt"     ,    "N_HH"   ,    "perc" )]

n <- ncol(output_df)


source("tz06_dict.R") # TZ FCT
# Taking the most updated version of the fct:
file_name <-  sort(grep("FCTs_dict_compiled", 
                           list.files(here::here("data")),
                           value = TRUE), decreasing = TRUE)[1]

fct_dict <- read.csv(here::here("data", file_name)) %>% 
  filter(!is.na(ID_3)) # Other FCTs



# Generating NCT: Matching with FCT
names(tz06_dict) 

# Matches w/ KE18> WA19> US19 > TZ06 

# output_df %>% 
#   filter(is.na(ENERCkcal)) #%>% View()


KE18_match <- output_df %>% 
  left_join(., fct_dict %>% filter(source_fct == "KE18"), by = "ID_3") %>%
  filter(!is.na(ENERCkcal)) 

WA19_match <- output_df %>% filter(!ID_3 %in% KE18_match$ID_3) %>% 
  left_join(., fct_dict %>% filter(source_fct == "WA19"), by = "ID_3") %>% 
  filter(!is.na(ENERCkcal))

US19_match <- output_df %>% 
  filter(!ID_3 %in% c(KE18_match$ID_3, WA19_match$ID_3)) %>% 
  left_join(., fct_dict %>% filter(source_fct == "US19"), by = "ID_3") %>%
  filter(!is.na(ENERCkcal))

TZ06_match <- output_df %>% 
  filter(!ID_3 %in% c(KE18_match$ID_3, WA19_match$ID_3, US19_match$ID_3)) %>% 
  left_join(., tz06_dict %>% filter(!is.na(ID_3)), by = "ID_3") %>% 
     filter(!is.na(ENERCkcal))
  
# WA19_match <- output_df %>% 
#   left_join(., fct_dict %>% filter(source_fct == "KE18"), by = "ID_3") %>%
#   filter(is.na(ENERCkcal)) %>% select(1:n) %>% 
#   left_join(., fct_dict %>% filter(source_fct == "WA19"), by = "ID_3") %>% 
#   filter(!is.na(ENERCkcal))
# 
# US19_match <- output_df %>% 
#   left_join(., fct_dict %>% filter(source_fct == "KE18"), by = "ID_3") %>%
#   filter(is.na(ENERCkcal)) %>% select(1:n) %>% 
#   left_join(., fct_dict %>% filter(source_fct == "WA19"), by = "ID_3") %>% 
#   filter(is.na(ENERCkcal)) %>%  select(1:n) %>% 
#   left_join(., fct_dict %>% filter(source_fct == "US19"), by = "ID_3") %>%
#   filter(!is.na(ENERCkcal))
#   
# TZ06_match <- output_df %>% 
#   left_join(., fct_dict %>% filter(source_fct == "KE18"), by = "ID_3") %>%
#   filter(is.na(ENERCkcal)) %>% select(1:n) %>% 
#   left_join(., fct_dict %>% filter(source_fct == "WA19"), by = "ID_3") %>% 
#   filter(is.na(ENERCkcal)) %>%  select(1:n) %>% 
#   left_join(., fct_dict %>% filter(source_fct == "US19"), by = "ID_3") %>%
#   filter(is.na(ENERCkcal)) %>%  select(1:n) %>% 
#   left_join(., tz06_dict %>% filter(!is.na(ID_3)), by = "ID_3") %>% 
#   filter(!is.na(ENERCkcal))


# Binding - re-using FAO scripts

#Need to introduce a duplication check here for averages tab candidates, and to extract the duplicates. To do this we'll merge all the matched tables together, and then find the codes which are duplicated.

grep("match$",ls(),value = TRUE)

all_matches <- data.table::rbindlist(list(KE18_match, US19_match, WA19_match, TZ06_match), use.names = T, fill = T)

all_matches %>% 
     filter(is.na(ENERCkcal)) #%>% View()
  
# Compiling FAT_g_standardised

for(i in 1:nrow(all_matches)){
  if (!is.na(all_matches$FATg[i])) {
    all_matches$FAT_g_standardised[i] <- all_matches$FATg[i]
  }  
  if (is.na(all_matches$FATg[i])) { 
    all_matches$FAT_g_standardised[i] <- all_matches$FAT_g[i]
  } 
  if (is.na(all_matches$FATg[i]) & is.na(all_matches$FAT_g[i])) {
    all_matches$FAT_g_standardised[i] <- all_matches$FATCEg[i]
  }
  if (is.na(all_matches$FATg[i]) & is.na(all_matches$FAT_g[i]) & 
      is.na(all_matches$FATCEg[i])) {
    all_matches$FAT_g_standardised[i] <- NA
  }
}

# Compiling VITB6_mg_standardised

for(i in 1:nrow(all_matches)){
  if (!is.na(all_matches$VITB6Amg[i])) {
    all_matches$VITB6_mg_standardised[i] <- all_matches$VITB6Amg[i]
  }  
  if (is.na(all_matches$VITB6Amg[i])) { 
    all_matches$VITB6_mg_standardised[i] <- all_matches$VITB6Cmg[i]
  } 
  if (is.na(all_matches$VITB6Amg[i]) & is.na(all_matches$VITB6Cmg[i])) {
    all_matches$VITB6_mg_standardised[i] <- all_matches$VITB6_mg[i]
  }
  if (is.na(all_matches$VITB6Amg[i]) & is.na(all_matches$VITB6Cmg[i]) & 
      is.na(all_matches$VITB6_mg[i])) {
    all_matches$VITB6_mg_standardised[i] <- NA
  }
}

# Compiling FOLmcg_standardised

for(i in 1:nrow(all_matches)){
  if (!is.na(all_matches$FOLFDmcg[i])) {
    all_matches$FOLmcg_standardised[i] <- all_matches$FOLFDmcg[i]
  }  
  if (is.na(all_matches$FOLFDmcg[i])) { 
    all_matches$FOLmcg_standardised[i] <- all_matches$FOLmcg[i]
  } 
  if (is.na(all_matches$FOLFDmcg[i]) & is.na(all_matches$FOLmcg[i])) {
    all_matches$FOLmcg_standardised[i] <- all_matches$FOLSUMmcg[i]
  }
  if (is.na(all_matches$FOLFDmcg[i]) & is.na(all_matches$FOLmcg[i]) & 
      is.na(all_matches$FOLSUMmcg[i])) {
    all_matches$FOLmcg_standardised[i] <- NA
  }
}

# Completing missing values in FCTs
source("missing_values.R")


# Checking duplicated
all_matches_duplicate_codes <- all_matches$item_id[duplicated(all_matches$item_id)]
nct_duplicates <- subset(all_matches, item_id %in% all_matches_duplicate_codes)

# Calculating multiple matches weighted averages

#source(here::here("functions", "Group_Summariser.R"))

Averages_Table <- Group_Summariser(nct_duplicates, "item_id", 
                    input_weighting_column = "Wt", weighting_leniency = 0.01,
                    blank_cols = c("fdc_id", "ID_3"), sep_row = T, seq_col = T,
                    weighting_col = T, round_weighting = F)

Averages_Table$source_fct[is.na(Averages_Table$ID_3)] <- ""

Averages_Table_slimmed <-
  Averages_Table %>%
  select(
    item_id,
    item_desc,
    Sequence,
    Wt,
    source_fct,
    fdc_id,
    food_desc,
    Edible_factor_in_FCT,
    ENERCkJ,
    ENERCkcal,
    WATERg,
    ASHg,
    PROCNTg,
    FAT_g_standardised,
    FIBTGg,
    ALCg,
    CHOAVLg,
    CHOAVLDFg,
    # CAmg,
    FEmg,
    #  MGmg,
    #  Pmg,
    #  Kmg,
    ZNmg,
    #    VITCmg,
    VITA_RAEmcg,
    THIAmg,
    RIBFmg,
    NIAmg,
    FOLmcg_standardised,
    VITB6_mg_standardised,
    VITB12mcg,
    RETOLmcg,
    CARTBmcg,
    CARTAmcg,
    CRYPXBmcg,
    CARTBEQmcg
  )


#Next to create the Compiled tab - essentially the nct, but any results from the averages tab are copied across and replace multiple matches

#IMPORTANT NOTE
#compiled tab has extra/different columns compared to averages - at the start:
#number of households reporting
#Weighting factor changed to "Food Item Index Matching (Read the text in the comment of this cell)" 

#at the end:
#Computed section (AK - AQ)
#Item group code
#Item group description

#From "Reference Food Composition Table/Database (FCT/FCDB)" to "Betacarotene equivalents (micrograms)" (end of Averages) they're the same


TZ_nct_compiled <- all_matches
TZ_nct_compiled$Sequence <- "" #Creates an empty Seq column
TZ_nct_compiled$Wt <- "" #Creates an empty Weighting factor column. not needed for compiled tab, but needed for merging with part of the Averages tab before creating the compiled tab


TZ_nct_compiled <- #cuts the columns down to the ones we want
  TZ_nct_compiled %>% select(
    item_id,
    item_desc,
    Sequence,
    Wt,
    source_fct,
    fdc_id,
    food_desc,
    Edible_factor_in_FCT,
    ENERCkJ,
    ENERCkcal,
    WATERg,
    ASHg,
    PROCNTg,
    FAT_g_standardised,
    FIBTGg,
    ALCg,
    CHOAVLg,
    CHOAVLDFg,
    # CAmg,
    FEmg,
    #  MGmg,
    #  Pmg,
    #  Kmg,
    ZNmg,
    #    VITCmg,
    VITA_RAEmcg,
    THIAmg,
    RIBFmg,
    NIAmg,
    FOLmcg_standardised,
    VITB6_mg_standardised,
    VITB12mcg,
    RETOLmcg,
    CARTBmcg,
    CARTAmcg,
    CRYPXBmcg,
    CARTBEQmcg
  )



#Need to find the averages rows from the averages tab to insert into the compiled tab
#ID_col_name <- colnames(Averages_Table_slimmed)[1] #For whatever reason grepl won't work with this long name, so saving it, changing it, running grepl, then changing it back
#colnames(Averages_Table_TZ_ID_slimmed)[1] <- "ID_col"
summary_rows_index <- which(grepl("SUMMARY", Averages_Table_slimmed$item_id)) #creates a list of summary rows
#colnames(Averages_Table_TZ_ID_slimmed)[1] <- ID_col_name
summary_rows_subset <- Averages_Table_slimmed[summary_rows_index,]
summary_rows_subset$source_fct  <- "From averages worksheet"
summary_rows_subset$fdc_id  <- ""
summary_rows_subset$food_desc <- ""

#Removing the summary row label
summary_rows_subset[,1] <- gsub("SUMMARY ROW - ", "", summary_rows_subset[,1]) #Removes the "SUMMARY ROW - " bit from the summary rows
summary_row_IDs <- summary_rows_subset[,1]

#Need to remove the rows that have summary rows from the averages tab
#ID_col_name <- colnames(TZ_nct_compiled)[1] #won't work with this long name, so saving it, changing it, running grepl, then changing it back
#colnames(TZ_nct_compiled)[1] <- "ID_col"
TZ_nct_compiled <- subset(TZ_nct_compiled, !(item_id %in% summary_row_IDs))
#colnames(TZ_nct_compiled)[1] <- ID_col_name

TZ_nct_compiled <- data.table::rbindlist(list(TZ_nct_compiled, summary_rows_subset))


## Missing values 
# Vitamin A - done
# thiamine (B1) - done
# riboflavin (B2) - done
# niacin (B3) - done
# pyridoxine (B6) - done
# folate (B9) - done
# cyanocobalamin (B12) - done
# iron - done
# zinc - done

item <- summary_rows_subset$item_id[is.na(summary_rows_subset$VITB6_mg_standardised)]

subset(Averages_Table_slimmed, item_id %in% item) %>% View()

subset(Averages_Table_slimmed, is.na(VITB6_mg_standardised)) %>% View()

TZ_nct_compiled$item_id[is.na(TZ_nct_compiled$VITB6_mg_standardised)]

TZ_nct_compiled$fdc_id[is.na(TZ_nct_compiled$VITB6_mg_standardised)]
Averages_Table_slimmed$fdc_id[is.na(Averages_Table_slimmed$VITB6_mg_standardised)]

# ids <- grep("[[:alnum:]]", TZ_nct_compiled$fdc_id[is.na(TZ_nct_compiled$VITB6_mg_standardised)], value = TRUE)
# ids <- c(ids, grep("[[:alnum:]]", Averages_Table_slimmed$fdc_id[is.na(Averages_Table_slimmed$VITB6_mg_standardised)], value = TRUE))
# fct_dict %>% filter(fdc_id %in% ids) %>% View()
# ids_id3 <- fct_dict %>% filter(fdc_id %in% ids) %>% distinct(ID_3, fdc_id) 
# 
# vitB6 <- fct_dict %>% filter(ID_3 %in% ids_id3[,1]) %>% select(fdc_id, ID_3, WATERg, starts_with("VITB6")) %>%
#   filter(!is.na(VITB6mg_std)| !is.na(VITB6Amg) | !is.na(VITB6_mg) | !is.na(VITB6Cmg))

#vitB6_wa19 <- fct_dict %>% filter(source_fct == "WA19" & ID_3 %in% ids_id3[,1]) %>% select(fdc_id, ID_3, WATERg, VITB6mg_std)
#vitB6_wa19 <- left_join(ids_id3, vitB6, by = "ID_3") %>% filter(!is.na(VITB6mg_std))

# saveRDS(vitB6_wa19, here::here("inter-output", "VitB6_missing-fix_WA19.RDS"))


subset(Averages_Table_slimmed, is.na(VITB6_mg_standardised)) %>%
  distinct(fdc_id) %>% count()

#write.csv(TZ_nct_compiled, here::here("output", "TFNC_NCT_NTPS20_v.3.0.0.csv"), 
 #         row.names = FALSE)

# write.csv(Averages_Table_slimmed, here::here("documentation", "TFNC_NCT_NTPS20_Averages-docu_v.3.0.0.csv"), 
 #         row.names = FALSE)


