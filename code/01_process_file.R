library(data.table)
library(tidyverse)
library(sqldf)

f <- "E:/national/VM2--WI--2020-03-21/VM2--WI--2020-03-21-DEMOGRAPHIC.tab"

k <- fread(f,
  select = c("LALVOTERID", "Voters_Active", 
         "Voters_StateVoterID", "Voters_CountyVoterID", 
         "Residence_Addresses_CensusTract",
         "Residence_Addresses_CensusBlockGroup",
         "Residence_Addresses_CensusBlock",
         "Residence_Addresses_Latitude",
         "Residence_Addresses_Longitude", 
         "Voters_Gender", "Voters_Age", "Voters_BirthDate", 
         "DateConfidence_Description", "Parties_Description", 
         "EthnicGroups_EthnicGroup1Desc", "Voters_CalculatedRegDate", 
         "Voters_OfficialRegDate", "US_Congressional_District", 
         "State_Senate_District", "State_House_District", 
         "State_Legislative_District", "County", "Voters_FIPS", 
         "Precinct", "CommercialData_EstimatedHHIncome",
         "CommercialData_Education"))

f <- "E:/national/VM2--WI--2020-03-21/VM2--WI--2020-03-21-VOTEHISTORY.tab"

hist <- fread(f, 
           select = c("LALVOTERID", "Primary_2018_08_14",
                      "General_2018_11_06",
                      "General_2016_11_08",
                      "Presidential_Primary_2016_04_05"))

hist <- hist %>% 
  mutate_at(vars("Primary_2018_08_14",
                 "General_2018_11_06",
                 "General_2016_11_08",
                 "Presidential_Primary_2016_04_05"),
            ~ ifelse(. == "Y", 1, 0))

k <- full_join(k, hist)

#################
prim <- rbindlist(lapply(c("D:/national/near_mke/mke/enhanced_a047e1c5_598d_49b7_8788_d67806362631.csv",
                           "D:/national/near_mke/other_counties/enhanced_dc1856ee_e07e_43cf_a856_5d8d63460caf.csv"),
                         function(f){
                           fread(f, select = c("LALVOTERID", "Presidential_Primary_2020-04-07"))
                         }))

saveRDS(prim, "./temp/mke_2020_prim.rds")
saveRDS(filter(k, Voters_FIPS %in% c(79, 101, 133, 131, 89)), "./temp/mke_voters.rds")
saveRDS(filter(k, County  == "DANE"), "./temp/madison_voters.rds")
