# Work on US import data of fish and seafood, by value and quantity
# Simeone Consulting, LLC


# Add key to .Renviron
Sys.setenv(CENSUS_KEY="") #insert US Census key 
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

dataPath <- "C:\\Users\\simeo\\Desktop\\US_FishSeafood_Imports_HTS\\"

fish_hts_to_query <- read.csv(paste0(dataPath, "FishSeafood_HTS_to_Query.csv"), stringsAsFactors = FALSE, colClasses = c(HS = "character")) 

library(tidyr)
#library(fuzzyjoin)
library(data.table)
library(dplyr)
library(readxl)
library(censusapi)
library(rjson)
library(jsonlite)
library(httr)
library(tibble)
library(janitor)
#library(stringr)
#library(lubridate)

########### Practice using cencusapi package to retrieve data from US Census Int'l Trade import data, 
#Abandonded because if interested in quantity and therefore unit variable, units comes in as NAs rather than actual units
# intltrade_vars <- listCensusMetadata(
#   name = "timeseries/intltrade/imports/hs", 
#   type = "variables")
# head(intltrade_vars)
# 
# ch44_imports <- getCensus(
#   name = "timeseries/intltrade/imports/hs",
#   vars = c("MONTH" , "I_COMMODITY_LDESC", "CTY_NAME","DISTRICT","DIST_NAME","GEN_VAL_MO", "CON_VAL_MO", "GEN_QY1_MO	", "CON_QY1_MO", "UNIT_QY1"),
#   #time = "from+2020-01+to+2021-06", 
#   YEAR = "2021", 
#   COMM_LVL = "HS10", 
#   I_COMMODITY = "44*")
# head(ch44_imports)
# 
# 
# ch44_imports_all_rows <- getCensus(
#   name = "timeseries/intltrade/imports/hs",
#   vars = c("MONTH", "I_COMMODITY_LDESC","DISTRICT","CTY_CODE","CTY_NAME","DIST_NAME","GEN_VAL_MO", "CON_VAL_MO","GEN_QY1_MO","CON_QY1_MO"),
#   #time = "from+2020-01+to+2021-06", 
#   YEAR = "2021", 
#   COMM_LVL = "HS10", 
#   I_COMMODITY = "4403*", 
#   UNIT_QY1 = "M")
# head(ch44_imports_all_rows)


#######Testing calling US Census data via webrowser, saving as JSON, and bringing into R
# data <- fromJSON(file = paste0(dataPath, "hs44_2021.json"))
# data_df <- as.data.frame(data)
# t_data_df <- t(data_df)
# t_data_df <- as.data.frame(t_data_df)


######### Test calling Census api through R (using httr) and convert matrix to data frame w/ first row as column headings
url <-"https://api.census.gov/data/timeseries/intltrade/imports/hs?key=6a6b224a3057a174ebd5cd67109f2f4800d270a9&get=MONTH%2CI_COMMODITY_LDESC%2CDISTRICT%2CCTY_CODE%2CCTY_NAME%2CDIST_NAME%2CGEN_VAL_MO%2CCON_VAL_MO%2CGEN_QY1_MO%2CCON_QY1_MO%2CUNIT_QY1&YEAR=2021&COMM_LVL=HS10&I_COMMODITY=03061440%2A"
batch <- httr::GET(url)
#cont_raw <- httr::content(batch)
#str(cont_raw, max.level = 3, list.len = 4)

batch_raw <- jsonlite::fromJSON(rawToChar(batch$content))
glimpse(batch_raw, max.level = 3, list.len = 4)


df_batch_raw <- as.data.frame(batch_raw)
df_batch_raw_col <- row_to_names(df_batch_raw, row_number =1)


########Census API Retrieval: Loop to bring in batches of HTS covering wood and forest products for 2019, 2020, 2021

#files <- list.files(inputPath)

compiled <- data.frame()
i=0

for(hts in fish_hts_to_query$HS){
 print(hts)
  
  #inner loop cycling through years 
  for(year in 2015:2021){
    print(year)
    i=i+1
    
    ######### Batch to call api and convert matrix to data frame w/ first row as column headings
    url <- paste0("https://api.census.gov/data/timeseries/intltrade/imports/hs?key=6a6b224a3057a174ebd5cd67109f2f4800d270a9&get=MONTH%2CI_COMMODITY_LDESC%2CDISTRICT%2CCTY_CODE%2CCTY_NAME%2CDIST_NAME%2CGEN_VAL_MO%2CCON_VAL_MO%2CGEN_QY1_MO%2CCON_QY1_MO%2CUNIT_QY1&",
                  "YEAR=",year,"&COMM_LVL=HS10&I_COMMODITY=",hts,"%2A")
    print(url)
    batch <- httr::GET(url)
    #cont_raw <- httr::content(batch)
    #str(cont_raw, max.level = 3, list.len = 4)
    
    batch_raw <- jsonlite::fromJSON(rawToChar(batch$content))
    df_batch_raw <- as.data.frame(batch_raw) %>%
      row_to_names(row_number=1) %>%
      mutate_if(is.factor, as.character) %>%
      mutate(GEN_VAL_MO = as.numeric(GEN_VAL_MO), 
             CON_VAL_MO = as.numeric(CON_VAL_MO), 
             GEN_QY1_MO = as.numeric(GEN_QY1_MO), 
             CON_QY1_MO = as.numeric(CON_QY1_MO)) %>%
      filter(DISTRICT != "-") %>%
      filter(CTY_CODE != "-") %>%
      filter((substr(CTY_CODE, start = 1, stop = 2) != "00")) %>%
      filter((substr(CTY_CODE, start = 2, stop = 4) != "XXX")) %>%
      filter(GEN_VAL_MO != 0 | CON_VAL_MO != 0)
    
    if(i==1){
      compiled <- df_batch_raw
    }else{
      compiled <- rbind(compiled, df_batch_raw)
    }
    
  }

}
compiled <- compiled %>% 
  mutate(i_commodity = as.character(i_commodity))
fwrite(compiled, paste0(dataPath, "US_imports_fish_seafood_2019_2020_2021.csv"))



