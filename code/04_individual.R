voters <- readRDS("./temp/mke_voters.rds") %>% 
  filter(!is.na(Residence_Addresses_Longitude),
         !is.na(Residence_Addresses_Latitude),
         !(Residence_Addresses_City %in% c("Whitefish Bay", "Bayside")),
         Voters_FIPS != 101) %>% 
  mutate(Precinct = gsub("MT PLEASANT", "MOUNT PLEASANT", Precinct),
         Precinct = gsub(" WARD ", "$", Precinct))

voters <- cSplit(voters, "Precinct", sep = "$", type.convert = F) %>% 
  mutate(Precinct_1 = ifelse(substring(Precinct_1, nchar(Precinct_1) - 2) == "VLG",
                             paste0("VILLAGE OF ", substring(Precinct_1, 1, nchar(Precinct_1) - 4)),
                             Precinct_1),
         Precinct_1 = ifelse(substring(Precinct_1, nchar(Precinct_1) - 3) == "CITY",
                             paste0("CITY OF ", substring(Precinct_1, 1, nchar(Precinct_1) - 5)),
                             Precinct_1),
         Precinct_1 = ifelse(substring(Precinct_1, nchar(Precinct_1) - 3) == "TOWN",
                             paste0("TOWN OF ", substring(Precinct_1, 1, nchar(Precinct_1) - 5)),
                             Precinct_1),
         p2 = as.integer(Precinct_2))

locs_20 <- fread("./temp/locs_20.csv") %>% 
  mutate(V1_22 = as.integer(V1_2))


voters <- left_join(voters, locs_20,
                    by = c("Precinct_1" = "V1_1",
                           "p2" = "V1_22"))
#######################

locs_16 <- fread("./temp/locs_16.csv") %>% 
  mutate(V1_22 = as.integer(V1_2)) %>% 
  rename(lat_16 = latitude,
         long_16 = longitude)

voters <- left_join(voters, locs_16,
                    by = c("Precinct_1" = "V1_1",
                           "p2" = "V1_22")) %>% 
  select(LALVOTERID, gender = Voters_Gender,
         age = Voters_Age,
         dob = Voters_BirthDate,
         party = Parties_Description,
         race = EthnicGroups_EthnicGroup1Desc,
         income = CommercialData_EstimatedHHIncome,
         education = CommercialData_Education,
         primary_18 = Primary_2018_08_14,
         primary_16 = Presidential_Primary_2016_04_05,
         lat = Residence_Addresses_Latitude,
         long = Residence_Addresses_Longitude,
         pp_lat_16 = lat_16,
         pp_long_16 = long_16,
         pp_lat_20 = latitude,
         pp_long_20 = longitude,
         city = Precinct_1)
###############

voters <- left_join(voters,
                    readRDS("./temp/mke_2020_prim.rds")) %>% 
  rename(primary_20 = `Presidential_Primary_2020-04-07`) %>% 
  mutate(primary_20 = ifelse(primary_20 == "Y", 1, 0))



###################

dists <- fread("./temp/dists_border.csv") %>% 
  select(LALVOTERID, distance_to_border = NEAR_DIST) %>% 
  mutate(distance_to_border = distance_to_border * 0.000621371)

voters <- left_join(voters, dists)

###################

voters <- voters %>% 
  mutate(white = race == "European",
         black = race == "Likely African-American",
         latino = race == "Hispanic and Portuguese",
         asian = race == "East and South Asian",
         dem = party == "Democratic",
         rep = party == "Republican",
         mke = city == "CITY OF MILWAUKEE",
         male = gender == "M")

voters <- cSplit(voters, "income", sep = "-", type.convert = F)

voters <- voters %>% 
  mutate(income_1 = as.integer(gsub("[$]|[+]", "", income_1)),
         income_2 = as.integer(income_2),
         income = ifelse(income_1 == 250000, 250000,
                         (income_1 + income_2) / 2))

voters$college <- grepl("Bach|Grad", voters$education)

saveRDS(voters %>% 
          select(LALVOTERID, age, primary_18,
                 primary_16, lat, long,
                 primary_20,
                 distance_border = distance_to_border,
                 white, black, latino, asian,
                 income, college, dem, rep,
                 mke, male), "./temp/match_data.rds")
