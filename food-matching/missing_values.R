
fcts <- read.csv(here::here("data", "FCTs_dict_compiled_v1.0.0.csv")) 

fcts %>% filter(grepl("orange", food_desc, ignore.case = TRUE) &
                  grepl("juice", food_desc, ignore.case = TRUE) &
                  grepl("bott|cann", food_desc, ignore.case = TRUE) ) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, VITB6mg_std, VITA_RAEmcg, FATg,
         FAT_g, ID_3)

fcts %>% filter(ID_3 == "23912.02.02" ) %>% 
  select(source_fct, fdc_id, food_desc, WATERg, starts_with("VITB6"))

#all_matches$food_desc[all_matches$fdc_id == "35238"]
#all_matches$ID_3[all_matches$fdc_id == "35238"]
#all_matches$VITA_RAEmcg[all_matches$fdc_id == "35238"] <- 0
#all_matches$THIAmg[all_matches$fdc_id == "35238"] <- 0 


all_matches$food_desc[all_matches$fdc_id == "10994"]
fcts$THIAmg[fcts$fdc_id == "19-649"]
all_matches$THIAmg[all_matches$fdc_id == "10994"] <- 0.58 # From UK21(19-649)
fcts$RIBFmg[fcts$fdc_id == "19-649"]
all_matches$RIBFmg[all_matches$fdc_id == "10994"]  <- fcts$RIBFmg[fcts$fdc_id == "19-649"]
fcts$NIAmg[fcts$fdc_id == "19-649"]
all_matches$NIAmg[all_matches$fdc_id == "10994"]  <- fcts$NIAmg[fcts$fdc_id == "19-649"]
fcts$FOLmcg[fcts$fdc_id == "19-649"]
all_matches$FOLmcg[all_matches$fdc_id == "10994"]  <- fcts$FOLmcg[fcts$fdc_id == "19-649"]
fcts$VITB12mcg[fcts$fdc_id == "19-649"]
all_matches$VITB12mcg[all_matches$fdc_id == "10994"]  <- fcts$VITB12mcg[fcts$fdc_id == "19-649"]


all_matches$THIAmg_std[all_matches$fdc_id == "14001"]

all_matches$FOLmcg_standardised[all_matches$fdc_id == "1038"] <- 14 # From TZ06(26)

# Vitamin B6
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "9011"] <- 0 # Assumed zero
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "13028"] <- 0 # Assumed zero
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "13017"] <- 0 # Assumed zero

# Ginger, dried
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "04_082"]*(100-fcts$WATERg[fcts$fdc_id == "13020" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "04_082"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "13020"] <- new_value  # Water adjusted from WA19(04_082) 
# Curry, powder
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "13-876"]*(100-fcts$WATERg[fcts$fdc_id == "13015" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "13-876"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "13015"] <- new_value  # Water adjusted
#Rice
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "01_037"]*(100-fcts$WATERg[fcts$fdc_id == "1033" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "01_037"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1033"] <- new_value  # Water adjusted 
# Maize grain (1019)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "50057"]*(100-fcts$WATERg[fcts$fdc_id == "1019" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "50057"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1019"] <- new_value  # Water adjusted 
# Maize flour (1022)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "MW01_0019"]*(100-fcts$WATERg[fcts$fdc_id == "1022" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "MW01_0019"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1022"] <- new_value  # Water adjusted 
#Finger millet (1027)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "MW01_0016"]*(100-fcts$WATERg[fcts$fdc_id == "1027" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "MW01_0016"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1027"] <- new_value  # Water adjusted 
#Pearl millet flour (1024)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "01_095"]*(100-fcts$WATERg[fcts$fdc_id == "1024" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "01_095"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1024"] <- new_value  # Water adjusted 
#Sorghum, Grain, Red, Flour (1038)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "01_041"]*(100-fcts$WATERg[fcts$fdc_id == "1038" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "01_041"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1038"] <- new_value  # Water adjusted 
#Bread brown (1005)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "MW01_0003"]*(100-fcts$WATERg[fcts$fdc_id == "1005" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "MW01_0003"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1005"] <- new_value  # Water adjusted 
#Rotli (Indian Chapati) (15130)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "01_046"]*(100-fcts$WATERg[fcts$fdc_id == "15130" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "01_046"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "15130"] <- new_value  # Water adjusted 
#Bread, Sweet (1006)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "01_046"]*(100-fcts$WATERg[fcts$fdc_id == "1006" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "01_046"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1006"] <- new_value  # Water adjusted 
#Buns, Currant (1011)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "MW01_0014"]*(100-fcts$WATERg[fcts$fdc_id == "1011" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "MW01_0014"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1011"] <- new_value  # Water adjusted 
#Scone, plain, commercial (1036)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "11-827"]*(100-fcts$WATERg[fcts$fdc_id == "1036" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "11-827"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1036"] <- new_value  # Water adjusted 
#Cake, Fruit (1013)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "11-829"]*(100-fcts$WATERg[fcts$fdc_id == "1013" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "11-829"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1013"] <- new_value  # Water adjusted 
#Cake, Sponge, without fat (1015)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "11-957"]*(100-fcts$WATERg[fcts$fdc_id == "1015" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "11-957"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1015"] <- new_value  # Water adjusted 
# Biscuit, Savoury (1003)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "01_188"]*(100-fcts$WATERg[fcts$fdc_id == "1003" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "01_188"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1003"] <- new_value  # Water adjusted 
# Drinking chocolate, powder (12004)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "17-498"]*(100-fcts$WATERg[fcts$fdc_id == "12004" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "17-498"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "12004"] <- new_value  # Water adjusted 
# Tangerine, juice, non-commercial (5035)	
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "9219"]*(100-fcts$WATERg[fcts$fdc_id == "5035" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "9219"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "5035"] <- new_value  # Water adjusted 
# Lemon, juice, home squeezed (5013)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "12_010"]*(100-fcts$WATERg[fcts$fdc_id == "5013" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "12_010"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "5013"] <- new_value  # Water adjusted 
# Lime, juice	(5016)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "9160"]*(100-fcts$WATERg[fcts$fdc_id == "5016" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "9160"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "5016"] <- new_value  # Water adjusted 
# Wine, White, Dry (12008)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "17-755"]*(100-fcts$WATERg[fcts$fdc_id == "12008" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "17-755"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "12008"] <- new_value  # Water adjusted 
# Wine, Red (12007)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "17-752"]*(100-fcts$WATERg[fcts$fdc_id == "12007" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "17-752"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "12007"] <- new_value  # Water adjusted 
# Pasta, spaghetti, dry, raw-imported	(1031)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "13007" & fcts$source_fct == "LS06"]*(100-fcts$WATERg[fcts$fdc_id == "1031" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "13007" & fcts$source_fct == "LS06"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1031"] <- new_value  # Water adjusted 

# Teff, raw (1041)
#new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "13007"]*(100-fcts$WATERg[fcts$fdc_id == "1041"]))/(100-fcts$WATERg[fcts$fdc_id == "13007"])
#all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1041"] <- new_value  # Water adjusted 

# Amaranth, raw (1001)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "1001" & fcts$source_fct == "JA15"]*(100-fcts$WATERg[fcts$fdc_id == "1001" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "1001"& fcts$source_fct == "JA15"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "1001"] <- new_value  # Water adjusted 
# Sweet potato (2013)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "02_022"]*(100-fcts$WATERg[fcts$fdc_id == "2013" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "02_022"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "2013"] <- new_value  # Water adjusted
# Pineapple, canned  (5029)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "14-212"]*(100-fcts$WATERg[fcts$fdc_id == "5029" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "14-212"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "5029"] <- new_value  # Water adjusted
# Beans, broad, fresh  (3002)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "11088" & fcts$source_fct == "US19"]*(100-fcts$WATERg[fcts$fdc_id == "3002" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "11088" & fcts$source_fct == "US19"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "3002"] <- new_value  # Water adjusted
# Beans, lima, dried (3005)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "16074"]*(100-fcts$WATERg[fcts$fdc_id == "3005" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "16074"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "3005"] <- new_value  # Water adjusted
# Beans, broad, dried  (3001)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "13-067"]*(100-fcts$WATERg[fcts$fdc_id == "3001" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "13-067"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "3001"] <- new_value  # Water adjusted
# Garden peas, dry, raw  (3016)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "13-130"]*(100-fcts$WATERg[fcts$fdc_id == "3016" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "13-130"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "3016"] <- new_value  # Water adjusted
# Gram, black, dry, raw (3017)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "6289"]*(100-fcts$WATERg[fcts$fdc_id == "3017" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "6289"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "3017"] <- new_value  # Water adjusted
# Nut, pistachio, raw, unsalted (10011)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "12151"]*(100-fcts$WATERg[fcts$fdc_id == "10011" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "12151"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "10011"] <- new_value  # Water adjusted
# Nut, macadamia, raw, unsalted (10010)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "12131"]*(100-fcts$WATERg[fcts$fdc_id == "10010" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "12131"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "10010"] <- new_value  # Water adjusted
# Onion, spring, raw (13024)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "11291" & fcts$source_fct == "US19"]*(100-fcts$WATERg[fcts$fdc_id == "13024"& fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "11291" & fcts$source_fct == "US19"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "13024"] <- new_value  # Water adjusted
# Onion, mature, red skinned, (13023)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "6156"]*(100-fcts$WATERg[fcts$fdc_id == "13023" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "6156"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "13023"] <- new_value  # Water adjusted
# Kale, Ethiopian (kanzera) (4019)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "50037"]*(100-fcts$WATERg[fcts$fdc_id == "4019" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "50037"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "4019"] <- new_value  # Water adjusted
# Kale, Ethiopian (kanzera) (4019)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "50037"]*(100-fcts$WATERg[fcts$fdc_id == "4019" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "50037"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "4019"] <- new_value  # Water adjusted
# Cabbage, leaf head (4006)
new_value <- (fcts$VITB6mg_std[fcts$fdc_id == "11112"& fcts$source_fct == "US19"]*(100-fcts$WATERg[fcts$fdc_id == "4006" & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id == "11112"& fcts$source_fct == "US19"])
all_matches$VITB6_mg_standardised[all_matches$fdc_id == "4006"] <- new_value  # Water adjusted
# Spinach, leaves (4030) 
#	Sweet Potato (Leaves) (4034)
# Vine (African) spinach (4038)
fct_value <- "US19"
id_value <- "11587"
id_missing <- "4038"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Coriander Leaves (13011)
fct_value <- "US19"
id_value <- "11165"
id_missing <- "13011"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Mushroom, dried (4024)
fct_value <- "UK21"
id_value <- "17-416"
id_missing <- "4024"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Mushroom, raw, canned  (4025)
fct_value <- "US19"
id_value <- "11264"
id_missing <- "4025"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Tomato, canned  (4037)
# Loquat, peeled (5018)
fct_value <- "US19"
id_value <- "9174"
id_missing <- "5018"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Strawberry (5034)
fct_value <- "US19"
id_value <- "9316"
id_missing <- "5034"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Mulberry (5021)
fct_value <- "US19"
id_value <- "9190"
id_missing <- "5021"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Pomegranate (5032)
#	Goat, lean (7014)
# Goat, medium fat (7016)
# Sausage, beef (7022)
fct_value <- "UK21"
id_value <- "19-656"
id_missing <- "7022"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Sausage, pork (7025)
fct_value <- "UK21"
id_value <- "19-656"
id_missing <- "7025"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Bacon, pre-sliced (10994)  # This is US19 missing value 
fct_value <- "UK21"
id_value <- "19-649"
id_missing <- "10994"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "US19"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Duck, meat and skin (7008)
fct_value <- "US19"
id_value <- "5139"
id_missing <- "7008"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Guinea fowl, meat and skin (7017)
# Quail flesh and skin (7026)
fct_value <- "US19"
id_value <- "5157"
id_missing <- "7026"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Dagaa fish, dried (8002)
fct_value <- "MW19"
id_value <- "MW03_0027"
id_missing <- "8002"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Nile perch, dry (8008)
fct_value <- "MW19"
id_value <- "MW03_0031"
id_missing <- "8008"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
#  Milk, cow, skimmed, raw (6019)
# Cheese, cottage, Milk, Cow, Sour (6006)
fct_value <- "US19"
id_value <- "1012"
id_missing <- "6006"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Cheese, cottage (cow milk), plain, skimmed (6004)
fct_value <- "US19"
id_value <- "1015"
id_missing <- "6004"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Cheese, cottage (cow milk) (6005)
fct_value <- "JA15"
id_value <- "13033"
id_missing <- "6005"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Milk, cow, whole, fermented  (6021)
fct_value <- "WA19"
id_value <- "10_022"
id_missing <- "6021"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted
# Milk, cow, condensed, skimmed, sweetened, raw (6015)
fct_value <- "US19"
id_value <- "1097"
id_missing <- "6015"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted

# Orange, Juice (5022)
fct_value <- "US19"
id_value <- "9207"
id_missing <- "5022"
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing] <- new_value  # Water adjusted

vitB6_wa19 <- readRDS(here::here("inter-output", "VitB6_missing-fix_WA19.RDS"))

# Vitamin B6
fct_value <- "WA19"
id_value <- vitB6_wa19$fdc_id.y
id_missing  <- vitB6_wa19$fdc_id.x

for(i in 1:length(id_value)){
  
new_value <- (fcts$VITB6mg_std[fcts$fdc_id %in% id_value[i] & fcts$source_fct %in% fct_value]*(100-fcts$WATERg[fcts$fdc_id %in% id_missing[i] & fcts$source_fct == "KE18"]))/(100-fcts$WATERg[fcts$fdc_id %in% id_value[i] & fcts$source_fct %in% fct_value])
all_matches$VITB6_mg_standardised[all_matches$fdc_id %in% id_missing[i]] <- new_value  # Water adjusted

print(i)

}













