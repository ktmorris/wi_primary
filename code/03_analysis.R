voters <- readRDS("./temp/mke_voters.rds") %>% 
  filter(!is.na(Residence_Addresses_Longitude),
         !is.na(Residence_Addresses_Latitude))


wards <- readOGR("raw_data/WI_20122020_Election_Data_Wards_2017",
                 "WI_20122020_Election Data_Wards_2017", verbose = FALSE)

wards@data$uni <- paste0(wards@data$NAME, wards@data$STR_WARDS)

pings  <- SpatialPoints(voters[c('Residence_Addresses_Longitude','Residence_Addresses_Latitude')],
                        proj4string = wards@proj4string)

voters$ward <- over(pings, wards)$uni

lookup <- fread("./temp/lookup.csv", header = T)

voters <- left_join(voters, lookup, by = c("ward" = "uni")) %>% 
  select(-ward) %>% 
  rename(ward = V4)


ward_count <- voters %>% 
  group_by(ward) %>% 
  summarize(female = mean(Voters_Gender == "F"),
            age = mean(Voters_Age, na.rm = T),
            white = mean(EthnicGroups_EthnicGroup1Desc == "European"),
            black = mean(EthnicGroups_EthnicGroup1Desc == "Likely African-American"),
            n = n())

w2 <- voters %>% 
  group_by(ward) %>% 
  summarize_at(vars("Primary_2018_08_14",
                    "General_2018_11_06",
                    "General_2016_11_08",
                    "Presidential_Primary_2016_04_05"), mean)

ward_count <- full_join(ward_count, w2)

#####################

distances <- fread("./temp/distances.csv")

all <- full_join(distances, ward_count, by = c("lookup_csv_Field4" = "ward"))


all <- all %>% 
  mutate(to = votes / n,
         distance = ifelse(grepl("MILWAUKEE", lookup_csv_Field4), NEAR_DIST * -1, NEAR_DIST),
         distance = distance / 1000,
         first_diff = to - Presidential_Primary_2016_04_05)

ggplot(all %>% 
         filter(to < 1,
                abs(distance) <= 2,
                black > 0.2), aes(x = distance, y = first_diff)) + geom_point() +
  theme_bw() +
  geom_vline(xintercept = 0) +
  labs(x = "Distance from Border with Milwaukee (KM)", y = "Turnout in 2018 Primary") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = comma)

summary(lm(first_diff ~ I(distance > 0) + distance +
             white + female + age, all %>% 
             filter(to < 1,
                    abs(distance) <= 1)))


reg.2 <- RDestimate(first_diff ~ distance + black,
                    data = filter(all, to < 1), cutpoint = 0)
