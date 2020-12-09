voters <- readRDS("./temp/mke_voters.rds") %>% 
  filter(!is.na(Residence_Addresses_Longitude),
         !is.na(Residence_Addresses_Latitude),
         !(Residence_Addresses_City %in% c("Whitefish Bay", "Bayside")),
         Voters_FIPS != 101,
         !(LALVOTERID %in% fread("temp/whaa.csv")$LALVOTERID)) %>% 
  mutate(Precinct = gsub("MT PLEASANT", "MOUNT PLEASANT", Precinct),
         Precinct = gsub(" WARD ", "$", Precinct))

########## car ownership

cars <- get_acs("tract", variables = c("no_cars" = "B08201_002"), summary_var = "B08201_001",
                state = "WI") %>% 
  mutate(share_car = 1 - (estimate / summary_est)) %>% 
  select(GEOID, share_car)


voters <- left_join(
  voters %>% 
    mutate(GEOID = paste0("55", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                          str_pad(Residence_Addresses_CensusTract, pad = "0", width = 6, side = "left"))),
  cars
)
##########

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


voters <- voters %>% 
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
         city = Precinct_1,
         share_car)
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
                 mke, male, share_car), "./temp/match_data.rds")
