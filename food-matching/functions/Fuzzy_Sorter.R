library(tidyverse)
library(fuzzyjoin)


#Currently, the input is two dataframes, stripped down to their ID and item name 
#columns, in that order

Fuzzy_Sorter <- function(search_item, df1, answer_length = 20){

  stopifnot("df1 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df1))
  stopifnot("df1 is too long - please make sure the input dataframes are two columns in length." = (length(df1) == 2))
  stopifnot("The search term is not a character or string - please input a character or string, e.g. 'mangoes, raw'" = is.character(search_item))
  
  
  ID_col <- c("1")
  Name_col <- c(search_item)
  search_df <- data.frame(ID_col, Name_col)
  
  colnames(df1)[2] <- "Name_col"
  
  fuzzy_output <- stringdist_join(search_df, df1, #This selects the two lists to check matches against
                                  by = "Name_col", #This allows you to select the field by which the search will be done
                                  mode = "left",
                                  method = "jw", #the fuzzy search method - more info here, need to do some research
                                  ignore_case=TRUE,
                                  distance_col = "dist") #This lists the distances and sets the column name they should be listed under - a perfect match should be 0
  
  sorted_fuzzy_output <- fuzzy_output[order(fuzzy_output[,5], na.last=TRUE),] #resorts the table based on pseudotable, putting NA matches at the bottom
  stripped_fuzzy_output <- sorted_fuzzy_output[, c(3,4,5)]
  print(head(stripped_fuzzy_output, n=answer_length))
  
}


Fuzzy_Sorter_df <- function(search_item, df1){
  
  stopifnot("df1 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df1))
  stopifnot("df1 is too long - please make sure the input dataframes are two columns in length." = (length(df1) == 2))
  stopifnot("The search term is not a character or string - please input a character or string, e.g. 'mangoes, raw'" = is.character(search_item))
  
  
  ID_col <- c("1")
  Name_col <- c(search_item)
  search_df <- data.frame(ID_col, Name_col)
  
  colnames(df1)[2] <- "Name_col"
  
  fuzzy_output <- stringdist_join(search_df, df1, #This selects the two lists to check matches against
                                  by = "Name_col", #This allows you to select the field by which the search will be done
                                  mode = "left",
                                  method = "jw", #the fuzzy search method - more info here, need to do some research
                                  ignore_case=TRUE,
                                  distance_col = "dist") #This lists the distances and sets the column name they should be listed under - a perfect match should be 0
  
  sorted_fuzzy_output <- fuzzy_output[order(fuzzy_output[,5], na.last=TRUE),] #resorts the table based on pseudotable, putting NA matches at the bottom
  stripped_fuzzy_output <- sorted_fuzzy_output[, c(3,4,5)]
  print(head(stripped_fuzzy_output, n=20L))
  
  return(sorted_fuzzy_output)
}
