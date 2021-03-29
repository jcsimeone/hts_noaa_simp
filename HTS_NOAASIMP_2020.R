# # Code to analyze NOAA SIMP Species, HTS Requirements, and close species substitutes in relation to US import data, by value 
# Simeone Consulting, LLC
# February 2021
# Funding: World Wildlife Fund (WWF) 
#
#
library(tidyr)
#library(fuzzyjoin)
library(data.table)
library(dplyr)
library(readxl)
library(writexl)
#library(writexlsx)
library(ggplot2)
library(zoo)
library(stringr)
#library(lubridate)
options(scipen = 999) 

dataPath <- "C:\\Users\\simeo\\Desktop\\SIMP_SpeciesHTSRequirements_DataAnalysis_2020\\"

#confirm or pre-process raw data file downloaded or recieved from US Census or International 
#Trade Commission (ITC) so that the column headings have no spaces 
#(e.g. use underscore instead of space)


##### Read in raw data and combine together with table specifying HTS codes that cover SIMP Species, as well as close substitutes #####

#read in yearly data files received directly from US Census and filter out rows where quantities 
#and values are both 0
HTSdata2015 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_June2020\\USCensus_FishSeafoodImports_2015.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)
  
HTSdata2016 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_June2020\\USCensus_FishSeafoodImports_2016.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

HTSdata2017 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_June2020\\USCensus_FishSeafoodImports_2017.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

HTSdata2018 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_June2020\\USCensus_FishSeafoodImports_2018.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

HTSdata2019 <- read_excel(paste0(dataPath, "RawDataFiles\\USCensus_June2020\\USCensus_FishSeafoodImports_2019.xlsx")) %>%
  filter(gen_val_mo != 0 | con_val_mo != 0)

#combine all years
htstradedata <- HTSdata2015 %>%
  rbind(HTSdata2016) %>%
  rbind(HTSdata2017) %>%
  rbind(HTSdata2018) %>%
  rbind(HTSdata2019) 

# remove yearly files now that they're combined
rm(HTSdata2015, HTSdata2016, HTSdata2017, HTSdata2018, HTSdata2019)

#write out combined data file for all years
fwrite(htstradedata, 
       paste0(dataPath, "March2021\\htstradedata_FishSeafood_2015_2019.csv"), dateTimeAs = "write.csv")

#read in simp_req filter as excel file
simp_req <- read_excel(paste0(dataPath, "RawDataFiles\\PreprocessedFilterData\\FILTER_SIMP_SpeciesTargets_CloseSubs_HTS_28Feb2021.xlsx")) %>%
  mutate(SIMP_RequiredBeginningDate = as.POSIXct(SIMP_RequiredBeginningDate, format = "%m/%d/%Y"), 
         commodity = as.character(commodity))

########join the pre-processed table that specifies HTS codes and species indicating whether HTS is SIMP-listed to the trade data
# create columns in hts trade data representing 2, 4, 6, 8, and 10 digit codes
htstradedata <- htstradedata %>%
  mutate(HTS2 = substr(commodity, 1, 2),
         HTS4 = substr(commodity, 1, 4),
         HTS6 = substr(commodity, 1, 6),
         HTS8 = substr(commodity, 1, 8),
         HTS10 = substr(commodity, 1, 10)) %>%
  select(-commodity)

# join all 10-digit hts codes to 10 digit codes in table with SIMP HTS
htstradedata_joined_simp <- left_join(htstradedata, simp_req, by = c("HTS10" = "commodity"))

#add in columns 
htstradedata_joined_simp <- htstradedata_joined_simp %>%
   #add in column to compare value of "General US imports" to "US Imports for Consumption" (https://www.census.gov/foreign-trade/guide/sec2.html#gen_imports) 
  mutate(diff_gen_val_minus_con_val = gen_val_mo - con_val_mo) %>%
  #add in column with link to CBP CROSS Database for each HTS10 commodity
  mutate(Examples_from_CBP_CROSS = paste0("https://rulings.cbp.gov/search?term=", 
                                          substr(HTS10,1,4), ".", 
                                          substr(HTS10,5,6), ".",
                                          substr(HTS10,7,10)),
         SIMP_Target = ifelse(is.na(SIMP_Target), 0, SIMP_Target), 
         HTS_SIMP_Listed = ifelse(is.na(HTS_SIMP_Listed), 0, HTS_SIMP_Listed))

#write out combined data with CROSS rulings hyperlinks as .CSV htstradedata_joined_simp file 
fwrite(htstradedata_joined_simp, 
       paste0(dataPath, "March2021\\htstradedata_joined_simp.csv"), dateTimeAs = "write.csv")

htstradedata_joined_simp <- htstradedata_joined_simp %>%
  mutate(Duplicate_HTS = ifelse(is.na(Duplicate_HTS), 0, Duplicate_HTS))

########### Output Tables for Report 

####### Results: Imports of SIMP-listed HTS Compared to Total US Seafood Imports ######

year_SIMP_summary <- htstradedata_joined_simp %>%
  filter(Duplicate_HTS == 0) %>%
  mutate(HTS_SIMP = ifelse(HTS_SIMP_Listed =="2", 1, HTS_SIMP_Listed)) %>%
  group_by(year, HTS_SIMP)%>%
  summarize(Value = sum(as.numeric(gen_val_mo)))

year_SIMP_listed <- year_SIMP_summary %>%
  filter(HTS_SIMP == 1) %>%
  mutate(HTS_SIMP_listed = Value) %>%
  select(-HTS_SIMP, -Value)

year_SIMP_not_listed <- year_SIMP_summary %>%
  filter(HTS_SIMP == 0) %>%
  mutate(HTS_SIMP_not_listed = Value) %>%
  select(-HTS_SIMP, -Value)

year_total_value <- year_SIMP_summary %>%
    group_by(year) %>%
  summarize(total_value = sum(Value))
  
yearly_summary_SIMP <- inner_join(year_SIMP_not_listed, year_SIMP_listed, by="year") 
yearly_summary_SIMP <- inner_join(yearly_summary_SIMP, year_total_value, by="year")  
yearly_summary_SIMP <- yearly_summary_SIMP %>%
  mutate(Percent_Tot_Imported_SIMP_listed = HTS_SIMP_listed / total_value)

rm(year_SIMP_summary, year_SIMP_listed, year_SIMP_not_listed, year_total_value)

####### Results: Atlantic and Pacific Cod (cod) #######
summary_cod <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic and Pacific Cod")) %>%
  group_by(SIMP_Target, HTS_SIMP_Listed, close_sub_binary) %>%
  summarise(quantity = sum(as.numeric(gen_qy1_mo)), 
            value = sum(as.numeric(gen_val_mo)))

cod_SIMP_listed_exclusive <- summary_cod %>%
  ungroup() %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 0) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary) 

cod_SIMP_listed_notexclusive <- summary_cod %>%
  ungroup() %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 1) %>%
  select(-SIMP_Target,-HTS_SIMP_Listed, -close_sub_binary)

cod_SIMP_listed_total <- cod_SIMP_listed_exclusive + cod_SIMP_listed_notexclusive 

cod_close_subs <- summary_cod %>%
  ungroup() %>%
  filter(SIMP_Target == 0 & HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

cod_nonSIMP <- summary_cod %>%
  ungroup() %>%
  filter(SIMP_Target == 1, HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

total_HTS_cod <- cod_SIMP_listed_total + cod_nonSIMP

percent_cod_HTS_coveredby_SIMP <- cod_SIMP_listed_total / total_HTS_cod

cod_SIMP_listed_exclusive <- cod_SIMP_listed_exclusive %>%
  mutate(Label = "HTS exclusively Atlantic & Pacific Cod")

cod_SIMP_listed_notexclusive <- cod_SIMP_listed_notexclusive %>%
  mutate(Label = "HTS may/likely contain Atlantic & Pacific Cod, but do not exclusively contain it")

cod_SIMP_listed_total <- cod_SIMP_listed_total %>%
  mutate(Label= "SIMP-covered HTS codes")

cod_close_subs <- cod_close_subs %>%
  mutate(Label= "HTS Close Substitutes to Atlantic & Pacific Cod")

cod_nonSIMP <- cod_nonSIMP %>%
  mutate(Label= "Non-SIMP covered HTS that may contain Atlantic & Pacific Cod")

total_HTS_cod <- total_HTS_cod %>%
  mutate(Label= "Total HTS covering Atlantic & Pacific Cod")

percent_cod_HTS_coveredby_SIMP <- percent_cod_HTS_coveredby_SIMP %>%
  mutate(Label= "% of HTS covering Atlantic & Pacific Cod that are covered by SIMP")

cod_results_table <- cod_SIMP_listed_total %>%
  rbind(cod_SIMP_listed_exclusive) %>%
  rbind(cod_SIMP_listed_notexclusive) %>%
  rbind(cod_nonSIMP) %>%
  rbind(total_HTS_cod) %>%
  rbind(percent_cod_HTS_coveredby_SIMP) %>%
  rbind(cod_close_subs) 

cod_results_table <- cod_results_table[c("Label", "value", "quantity")]

rm(cod_SIMP_listed_total, cod_SIMP_listed_exclusive, cod_SIMP_listed_notexclusive,
   cod_nonSIMP, total_HTS_cod, percent_cod_HTS_coveredby_SIMP, cod_close_subs)

####### Results: Atlantic Blue Crab (abc)   ########
summary_abc <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic Blue Crab")) %>%
  group_by(SIMP_Target, HTS_SIMP_Listed, close_sub_binary) %>%
  summarise(quantity = sum(as.numeric(gen_qy1_mo)), 
            value = sum(as.numeric(gen_val_mo)))

abc_SIMP_listed_exclusive <- summary_abc %>%
  ungroup() %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 0) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

if(nrow(abc_SIMP_listed_exclusive)==0){
  abc_SIMP_listed_exclusive <- abc_SIMP_listed_exclusive %>%
    rbind(c(0,0))
  colnames(abc_SIMP_listed_exclusive) = c("quantity", "value")
  }

abc_SIMP_listed_notexclusive <- summary_abc %>%
  ungroup() %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 1) %>%
  select(-SIMP_Target,-HTS_SIMP_Listed, -close_sub_binary)

abc_SIMP_listed_total <-abc_SIMP_listed_notexclusive  + abc_SIMP_listed_exclusive 

abc_close_subs <- summary_abc %>%
  ungroup() %>%
  filter(SIMP_Target == 0 & HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

#abc_nonSIMP <- summary_abc %>%
#  ungroup() %>%
#  filter(SIMP_Target == 1, HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
#  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

total_HTS_abc <- abc_SIMP_listed_total #+ abc_nonSIMP

percent_abc_HTS_coveredby_SIMP <- abc_SIMP_listed_total / total_HTS_abc

abc_SIMP_listed_exclusive <- abc_SIMP_listed_exclusive %>%
  mutate(Label = "HTS exclusively Atlantic Blue Crab")

abc_SIMP_listed_notexclusive <- abc_SIMP_listed_notexclusive %>%
  mutate(Label = "HTS may/likely contain Atlantic Blue Crab, but do not exclusively contain it")

abc_SIMP_listed_total <- abc_SIMP_listed_total %>%
  mutate(Label= "SIMP-covered HTS codes")

abc_close_subs <- abc_close_subs %>%
  mutate(Label= "HTS Close Substitutes to Atlantic Blue Crab")

#abc_nonSIMP <- abc_nonSIMP %>%
#  mutate(Label= "Non-SIMP covered HTS that may contain Atlantic Blue Crab")

total_HTS_abc <- total_HTS_abc %>%
  mutate(Label= "Total HTS covering Atlantic Blue Crab")

percent_abc_HTS_coveredby_SIMP <- percent_abc_HTS_coveredby_SIMP %>%
  mutate(Label= "% of HTS covering Atlantic Blue Crab that are covered by SIMP")

abc_results_table <- abc_SIMP_listed_total %>%
  rbind(abc_SIMP_listed_exclusive) %>%
  rbind(abc_SIMP_listed_notexclusive) %>%
  #rbind(abc_nonSIMP) %>%
  rbind(total_HTS_abc) %>%
  rbind(percent_abc_HTS_coveredby_SIMP) %>%
  rbind(abc_close_subs) 

abc_results_table <- abc_results_table[c("Label", "value", "quantity")]

rm(abc_SIMP_listed_total, abc_SIMP_listed_exclusive, abc_SIMP_listed_notexclusive, 
   total_HTS_abc, percent_abc_HTS_coveredby_SIMP, abc_close_subs)

####### Results: Red King Crab (rkc) ###########

summary_rkc <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Red King Crab")) %>%
  group_by(SIMP_Target, HTS_SIMP_Listed, close_sub_binary) %>%
  summarise(quantity = sum(as.numeric(gen_qy1_mo)), 
            value = sum(as.numeric(gen_val_mo)))

rkc_SIMP_listed_exclusive <- summary_rkc %>%
  ungroup() %>%
  filter(SIMP_Target == 1 & HTS_SIMP_Listed == 1 & close_sub_binary == 0) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary) 

rkc_SIMP_listed_notexclusive <- summary_rkc %>%
  ungroup() %>%
  filter(SIMP_Target == 1 & HTS_SIMP_Listed == 1 & close_sub_binary == 1) %>%
  select(-SIMP_Target,-HTS_SIMP_Listed, -close_sub_binary)

rkc_SIMP_listed_total <- rkc_SIMP_listed_exclusive + rkc_SIMP_listed_notexclusive 

rkc_close_subs <- summary_rkc %>%
  ungroup() %>%
  filter(SIMP_Target == 0 & HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

rkc_nonSIMP <- summary_rkc %>%
  ungroup() %>%
  filter(SIMP_Target == 1 & HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

rkc_close_subs <- rkc_close_subs + rkc_nonSIMP

total_HTS_rkc <- rkc_SIMP_listed_total + rkc_nonSIMP

percent_rkc_HTS_coveredby_SIMP <- rkc_SIMP_listed_total / total_HTS_rkc

rkc_SIMP_listed_exclusive <- rkc_SIMP_listed_exclusive %>%
  mutate(Label = "HTS exclusively Red King Crab")

rkc_SIMP_listed_notexclusive <- rkc_SIMP_listed_notexclusive %>%
  mutate(Label = "HTS may/likely contain Red King Crab, but do not exclusively contain it")

rkc_SIMP_listed_total <- rkc_SIMP_listed_total %>%
  mutate(Label= "SIMP-covered HTS codes")

rkc_close_subs <- rkc_close_subs %>%
  mutate(Label= "HTS Close Substitutes to Red King Crab")

rkc_nonSIMP <- rkc_nonSIMP %>%
  mutate(Label= "Non-SIMP covered HTS that may contain Red King Crab")

total_HTS_rkc <- total_HTS_rkc %>%
  mutate(Label= "Total HTS covering Red King Crab")

percent_rkc_HTS_coveredby_SIMP <- percent_rkc_HTS_coveredby_SIMP %>%
  mutate(Label= "% of HTS covering Red King Crab that are covered by SIMP")

rkc_results_table <- rkc_SIMP_listed_total %>%
  rbind(rkc_SIMP_listed_exclusive) %>%
  rbind(rkc_SIMP_listed_notexclusive) %>%
  rbind(rkc_nonSIMP) %>%
  rbind(total_HTS_rkc) %>%
  rbind(percent_rkc_HTS_coveredby_SIMP) %>%
  rbind(rkc_close_subs) 

rkc_results_table <- rkc_results_table[c("Label", "value", "quantity")]

rm(rkc_SIMP_listed_total, rkc_SIMP_listed_exclusive, rkc_SIMP_listed_notexclusive, 
   rkc_nonSIMP, total_HTS_rkc, percent_rkc_HTS_coveredby_SIMP, rkc_close_subs)

####### Results: Northern Red Snapper (nrs) #################

summary_nrs <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Red Snapper")) %>%
  group_by(SIMP_Target, HTS_SIMP_Listed, close_sub_binary) %>%
  summarise(quantity = sum(as.numeric(gen_qy1_mo)), 
            value = sum(as.numeric(gen_val_mo)))

nrs_SIMP_listed_exclusive <- summary_nrs %>%
 ungroup() %>%
 filter(HTS_SIMP_Listed == 1 & close_sub_binary == 0) %>%
 select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

if(nrow(nrs_SIMP_listed_exclusive)==0){
  nrs_SIMP_listed_exclusive <- nrs_SIMP_listed_exclusive %>%
    rbind(c(0,0))
  colnames(nrs_SIMP_listed_exclusive) = c("quantity", "value")
}

nrs_SIMP_listed_notexclusive <- summary_nrs %>%
  ungroup() %>%
  filter(SIMP_Target == 1 & HTS_SIMP_Listed == 1 & close_sub_binary == 1) %>%
  select(-SIMP_Target,-HTS_SIMP_Listed, -close_sub_binary)

nrs_SIMP_listed_total <-nrs_SIMP_listed_notexclusive + nrs_SIMP_listed_exclusive 

nrs_close_subs <- summary_nrs %>%
  ungroup() %>%
  filter(SIMP_Target == 0 & HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

nrs_nonSIMP <- summary_nrs %>%
  ungroup() %>%
  filter(SIMP_Target == 1, HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

total_HTS_nrs <- nrs_SIMP_listed_total + nrs_nonSIMP

percent_nrs_HTS_coveredby_SIMP <- nrs_SIMP_listed_total / total_HTS_nrs

nrs_SIMP_listed_exclusive <- nrs_SIMP_listed_exclusive %>%
  mutate(Label = "HTS exclusively Northern Red Snapper")

nrs_SIMP_listed_notexclusive <- nrs_SIMP_listed_notexclusive %>%
  mutate(Label = "HTS may/likely contain Northern Red Snapper, but do not exclusively contain it")

nrs_SIMP_listed_total <- nrs_SIMP_listed_total %>%
  mutate(Label= "SIMP-covered HTS codes")

nrs_close_subs <- nrs_close_subs %>%
  mutate(Label= "HTS Close Substitutes to Northern Red Snapper")

nrs_nonSIMP <- nrs_nonSIMP %>%
  mutate(Label= "Non-SIMP covered HTS that may contain Northern Red Snapper")

total_HTS_nrs <- total_HTS_nrs %>%
  mutate(Label= "Total HTS covering Northern Red Snapper")

percent_nrs_HTS_coveredby_SIMP <- percent_nrs_HTS_coveredby_SIMP %>%
  mutate(Label= "% of HTS covering Northern Red Snapper that are covered by SIMP")

nrs_results_table <- nrs_SIMP_listed_total %>%
  rbind(nrs_SIMP_listed_exclusive) %>%
  rbind(nrs_SIMP_listed_notexclusive) %>%
  rbind(nrs_nonSIMP) %>%
  rbind(total_HTS_nrs) %>%
  rbind(percent_nrs_HTS_coveredby_SIMP) %>%
  rbind(nrs_close_subs) 

nrs_results_table <- nrs_results_table[c("Label", "value", "quantity")]

rm(nrs_SIMP_listed_total, nrs_SIMP_listed_exclusive, nrs_SIMP_listed_notexclusive, 
   nrs_nonSIMP, total_HTS_nrs, percent_nrs_HTS_coveredby_SIMP, nrs_close_subs)

####### Results: Tunas (tuna)  ###############

summary_tuna <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Tunas")) %>%
  group_by(SIMP_Target, HTS_SIMP_Listed, close_sub_binary) %>%
  summarise(quantity = sum(as.numeric(gen_qy1_mo)), 
            value = sum(as.numeric(gen_val_mo)))

tuna_SIMP_listed_exclusive <- summary_tuna %>%
  ungroup() %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 0) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary) 

tuna_SIMP_listed_notexclusive <- summary_tuna %>%
  ungroup() %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 1) %>%
  select(-SIMP_Target,-HTS_SIMP_Listed, -close_sub_binary)

tuna_SIMP_listed_total <- tuna_SIMP_listed_exclusive + tuna_SIMP_listed_notexclusive 

tuna_close_subs <- summary_tuna %>%
  ungroup() %>%
  filter(SIMP_Target == 0 & HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

tuna_nonSIMP <- summary_tuna %>%
  ungroup() %>%
  filter(SIMP_Target == 1, HTS_SIMP_Listed == 0 & close_sub_binary == 1) %>%
  select(-SIMP_Target, -HTS_SIMP_Listed, -close_sub_binary)

if(nrow(tuna_nonSIMP)==0){
  tuna_nonSIMP <- tuna_nonSIMP %>%
    rbind(c(0,0))
  colnames(tuna_nonSIMP) = c("quantity", "value")
}

total_HTS_tuna <- tuna_SIMP_listed_total + tuna_nonSIMP

percent_tuna_HTS_coveredby_SIMP <- tuna_SIMP_listed_total / total_HTS_tuna

tuna_SIMP_listed_exclusive <- tuna_SIMP_listed_exclusive %>%
  mutate(Label = "HTS exclusively Tunas")

tuna_SIMP_listed_notexclusive <- tuna_SIMP_listed_notexclusive %>%
  mutate(Label = "HTS may/likely contain Tunas, but do not exclusively contain it")

tuna_SIMP_listed_total <- tuna_SIMP_listed_total %>%
  mutate(Label= "SIMP-covered HTS codes")

tuna_close_subs <- tuna_close_subs %>%
  mutate(Label= "HTS Close Substitutes to Tunas")

tuna_nonSIMP <- tuna_nonSIMP %>%
  mutate(Label= "Non-SIMP covered HTS that may contain Tunas")

total_HTS_tuna <- total_HTS_tuna %>%
  mutate(Label= "Total HTS covering Tunas")

percent_tuna_HTS_coveredby_SIMP <- percent_tuna_HTS_coveredby_SIMP %>%
  mutate(Label= "% of HTS covering Tunas that are covered by SIMP")

tuna_results_table <- tuna_SIMP_listed_total %>%
  rbind(tuna_SIMP_listed_exclusive) %>%
  rbind(tuna_SIMP_listed_notexclusive) %>%
  rbind(tuna_nonSIMP) %>%
  rbind(total_HTS_tuna) %>%
  rbind(percent_tuna_HTS_coveredby_SIMP) %>%
  rbind(tuna_close_subs) 

tuna_results_table <- tuna_results_table[c("Label", "value", "quantity")]

rm(tuna_SIMP_listed_total, tuna_SIMP_listed_exclusive, tuna_SIMP_listed_notexclusive,
   tuna_nonSIMP, total_HTS_tuna, percent_tuna_HTS_coveredby_SIMP, tuna_close_subs)

####### Results: SIMP-Species Case Studies: Overview ###########

overview_results_table <- data.frame("Case_Study_Species" = "Atlantic & Pacific Cod",
                                     "SIMP_listed_HTS" = cod_results_table[1,2], 
                                     "Close_Substitutes_HTS" = cod_results_table[7,2], stringsAsFactors = FALSE) %>%
  rbind(list("Atlantic Blue Crab", abc_results_table[1,2], abc_results_table[6,2])) %>%
  rbind(list("Red King Crab", rkc_results_table[1,2], rkc_results_table[7,2])) %>%
  rbind(list("Northern Red Snapper", nrs_results_table[1,2], nrs_results_table[7,2])) %>%
  rbind(list("Tunas", tuna_results_table[1,2], tuna_results_table[7,2]))
  


########### Tables in Appendices


######## Appendix 2: Top US Imports of Non-SIMP-listed HTS for Seafood ######
appendix2 <- htstradedata_joined_simp %>%
  filter(year == 2019, Duplicate_HTS == 0, HTS_SIMP_Listed==0) %>%
  group_by(HTS10, descriptn) %>%
  summarize (Value = sum(as.numeric(gen_val_mo)))

appendix2 <- appendix2[with(appendix2, order(-Value)), ] %>% 
  mutate(prct2019total = (Value / sum(appendix2$Value))) %>%
  ungroup() %>%
  mutate(RunTot_2019percent = cumsum(prct2019total))

######## Appendix 3.1: Atlantic and Pacific Cod   #####

#Table 3.1a: SIMP-Covered HTS for All Likely Product Forms

appendix3_1a_cod <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic and Pacific Cod")) %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 0)%>%
  group_by(species, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

# Table 3.1b: HTS also SIMP-listed for Cod, though may contain close-substitutes 
appendix3_1b_cod <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic and Pacific Cod")) %>%
  filter(HTS_SIMP_Listed == 1 & close_sub_binary == 1)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

# Table 3.1c: Non-SIMP covered HTS that may contain Atlantic & Pacific Cod
appendix3_1c_cod <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic and Pacific Cod")) %>%
  filter(SIMP_Target == 1, HTS_SIMP_Listed == 0 & close_sub_binary == 1)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

# Table 3.1d: HTS Close Substitutes to Atlantic & Pacific Cod 
appendix3_1d_cod <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic and Pacific Cod")) %>%
  filter(HTS_SIMP_Listed == 0 & close_sub_binary == 1)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

rm(summary_cod)

######## Appendix 3.2: Atlantic Blue Crab   #######

#Table 3.2a: SIMP Coverage of Species for All Likely Product Forms
appendix3_2a_abc <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic Blue Crab")) %>%
  filter(HTS_SIMP_Listed == 1)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

#Table 3.2b: Non-SIMP-Listed HTS Codes that May Contain the Species
appendix3_2b_abc <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Atlantic Blue Crab")) %>%
  filter(HTS_SIMP_Listed == 0)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

rm(summary_abc)

######## Appendix 3.3: Red King Crab   #######

#Table 3.3a: SIMP Coverage of Species for All Likely Product Forms
appendix3_3a_rkc <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Red King Crab")) %>%
  filter(HTS_SIMP_Listed == 1)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

#Table 3.3b: Non-SIMP-Listed HTS Codes that May Contain the Species
appendix3_3b_rkc <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Red King Crab")) %>%
  filter(HTS_SIMP_Listed == 0)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

rm(summary_rkc)

######## Appendix 3.4: Northern Red Snapper   #######

#Table 3.4a: SIMP Coverage of Species for All Likely Product Forms
appendix3_4a_nrs <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Red Snapper")) %>%
  filter(HTS_SIMP_Listed == 1)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

#Table 3.4c: Non-SIMP covered HTS that may contain Red Snapper 

appendix3_4c_nrs <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Red Snapper")) %>%
  filter(SIMP_Target == 1, HTS_SIMP_Listed == 0)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

#Table 3.4d: Non-SIMP-Listed HTS Codes that May Contain the Species
appendix3_4d_nrs <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Red Snapper")) %>%
  filter(SIMP_Target == 0, HTS_SIMP_Listed == 0)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

rm(summary_nrs)

######## Appendix 3.5: Tunas   #######

#Table 3.5a: SIMP Coverage of Species for All Likely Product Forms
appendix3_5a_tuna <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Tunas")) %>%
  filter(HTS_SIMP_Listed == 1)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

#Table 3.5b: Non-SIMP-Listed HTS Codes that May Contain the Species
appendix3_5b_tuna <- htstradedata_joined_simp %>%
  filter(year == 2019)%>%
  filter(str_detect(SIMP_SpeciesName, "Tunas")) %>%
  filter(HTS_SIMP_Listed == 0)%>%
  group_by(close_sub, prod_descrip, HTS10, descriptn) %>%
  summarise(value = sum(as.numeric(gen_val_mo)))

rm(summary_tuna)


############### Write all final data tables to tabs in one Excel file ##########
sheets <- list("R_SIMP_Overview" = yearly_summary_SIMP,
               "R_cod" = cod_results_table,
               "R_abc" = abc_results_table,
               "R_rkc" = rkc_results_table,
               "R_nrs" = nrs_results_table,
               "R_tuna" = tuna_results_table,
               "R_CaseStudy_results" = overview_results_table,
               "A2" = appendix2,
               "A3_1a_cod" = appendix3_1a_cod,
               "A3_1b_cod" = appendix3_1b_cod,
               "A3_1c_cod" = appendix3_1c_cod,
               "A3_1d_cod" = appendix3_1d_cod,
               "A3_2a_abc" = appendix3_2a_abc,
               "A3_2b_abc" = appendix3_2b_abc, 
               "A3_3a_rkc" = appendix3_3a_rkc,
               "A3_3b_rkc" = appendix3_3b_rkc, 
               "A3_4a_nrs" = appendix3_4a_nrs,
               "A3_4c_nrs" = appendix3_4c_nrs, 
               "A3_4d_nrs" = appendix3_4d_nrs,
               "A3_5a_tuna" = appendix3_5a_tuna,
               "A3_5b_tuna" = appendix3_5b_tuna)
write_xlsx(sheets, 
paste0(dataPath, "OutputFiles\\Final_SIMP_AnalysisTables.xlsx"))

rm(yearly_summary_SIMP, 
   cod_results_table, 
   abc_results_table, 
   rkc_results_table, 
   nrs_results_table,
   tuna_results_table, 
   overview_results_table, 
   appendix2, 
   appendix3_1a_cod, 
   appendix3_1b_cod, 
   appendix3_1c_cod, 
   appendix3_1d_cod, 
   appendix3_2a_abc, 
   appendix3_2b_abc, 
   appendix3_3a_rkc, 
   appendix3_3b_rkc, 
   appendix3_4a_nrs, 
   appendix3_4c_nrs, 
   appendix3_4d_nrs, 
   appendix3_5a_tuna, 
   appendix3_5b_tuna)



######## End of SIMP code #############

########### Testing out figures ##################






SIMP_ports_summary <- htstradedata_joined_simp %>%
  filter(Duplicate_HTS == 0, year == "2019") %>%
  mutate(HTS_SIMP = ifelse(HTS_SIMP_Listed =="2", 1, HTS_SIMP_Listed)) %>%
  group_by(district)%>%
  summarize(Value = sum(as.numeric(gen_val_mo)))

year_species_SIMP_summary <- htstradedata_joined_simp %>%
  group_by(year, SIMP_Target, HTS_SIMP_Listed)%>%
  summarize(sum(as.numeric(gen_val_mo)))

year_species1_SIMP_summary <- htstradedata_joined_simp %>%
  group_by(year,HTS_SIMP_Listed, ExclusivelyContainsSIMPTarget, SIMP_SpeciesName) %>%
  summarize (sum_gen_val_mo = sum(as.numeric(gen_val_mo)))

ggplot(year_species1_SIMP_summary, aes(year, sum_gen_val_mo, col = SIMP_SpeciesName)) + 
  geom_point(alpha =0.5) 

year_species_closesub_SIMP_summary <- htstradedata_joined_simp %>%
  group_by(year,HTS_SIMP_Listed, close_sub_binary, ExclusivelyContainsSIMPTarget, SIMP_SpeciesName) %>%
  summarize (sum_gen_val_mo = sum(as.numeric(gen_val_mo)))

ggplot(filter(year_species_closesub_SIMP_summary, year == "2019"), aes(close_sub_binary, sum_gen_val_mo, col = SIMP_SpeciesName)) + 
  geom_point(alpha =0.5) 

test <- "2019"
as.POSIXct(test, format = "%Y")

htstradedata_joined_simp$year_as_date <- as.yearmon(paste(htstradedata_joined_simp$year, htstradedata_joined_simp$month, "%Y %m"))

htstradedata_joined_simp <- htstradedata_joined_simp %>%
  mutate(year_as_date = as.yearmon(paste(htstradedata_joined_simp$year, htstradedata_joined_simp$month, "%Y %m")))

#plot bar graph w/ 1 year of data of just SIMP-listed species
ggplot(filter(year_species1_SIMP_summary, year == "2019" & !is.na(SIMP_SpeciesName)), aes(SIMP_SpeciesName, sum_gen_val_mo, fill = SIMP_SpeciesName)) + 
  geom_col(alpha =0.5) 
  
  #facet_grid(year ~ .)

#+
#  scale_x_datetime(limit=c(as.POSIXct("2018-01-01"),as.POSIXct("2019-01-01"))) +
#  ylim(0,6000000000)

?geom_bar
?scale_x_date
as.POSIXct
?as.yearmon
?df$Date

#+ geom_text(rownames(SIMP_SpeciesName))

#write out combined data as .XLSX htstradedata_joined_simp file 
write_xlsx(
  htstradedata_joined_simp,
  path = paste0(dataPath, "OutputFiles\\htstradedata_joined_simp.xlsx"),
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)





# htstradedata <- join_hts10 %>%
#   rbind(join_hts2_matched) %>%
#   rbind(join_hts4_matched) %>%
#   rbind(join_hts6_matched) %>%
#   rbind(join_hts8_matched)

# clean up extra variables
rm(join_hts10, join_hts2, join_hts2_matched, join_hts4, join_hts4_matched, join_hts6, join_hts6_matched, join_hts8, join_hts8_matched)
rm(nas_hts2, nas_hts4, nas_hts6, nas_hts8)

# overwrite exclusions
exclusions <- laceydeclarations %>%
  filter(ExclusionsNoDecReq == 1) %>%
  mutate(code_length = nchar(HTS_Codes))
# * will need to modify code to account for future exclusions of different hts code lengths 
htstradedata <- htstradedata %>%
  mutate(dec_req = ifelse(substr(HTS10, 1, 8) %in% exclusions$HTS_Codes, "Declaration Form Not Required", dec_req)) %>%
  select(-ExclusionsNoDecReq)

# mark everything in ch 44 and 6 hts6 codes in ch 94 as exclusively wood
htstradedata <- htstradedata %>%
  mutate(ExclusivelyContainsWood = ifelse(HTS2 == "44" | HTS2 == "47" | HTS2 == "48", 1, ExclusivelyContainsWood),
         ExclusivelyContainsWood = ifelse(HTS6 == "940161" | HTS6 == "940169"  | HTS6 == "940330" |
                                          HTS6 == "940340" | HTS6 == "940350" | HTS6 == "940360", 
                                          1, ExclusivelyContainsWood))

#write out combined data file for all years
fwrite(htstradedata, 
       paste0(dataPath, "OutputFiles\\htstradedata_laceydeclarations.csv"), dateTimeAs = "write.csv")






