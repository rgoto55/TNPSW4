
#Library loading
library(tidyverse)

# Loading data
data.df <- read.csv(here::here("output", "TFNC_NCT_NTPS20_v.1.0.0.csv")) 

hist(data.df$THIAmg)

data.df$food_desc[data.df$THIAmg>1]
subset(data.df, THIAmg>1)