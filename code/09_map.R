

counties <- readOGR("raw_data/cb_2018_us_county_500k", "cb_2018_us_county_500k")

counties <- subset(counties, STATEFP == "55")

counties <- subset(counties, NAME %in% c("Milwaukee", "Ozaukee", "Washington", "Racine", "Waukesha"))

pps16 <- read_xlsx("raw_data/polling_place_locations_2016_nov_general_xlsx_81288.xlsx") %>% 
  select(lat = Latitude, long = Longitude) %>% 
  mutate(year = "2016 General")

pps20 <- read_xlsx("raw_data/Polling Place List-2020 April Election as of 4-4-20.xlsx.xlsx") %>% 
  select(lat = Latitude, long = Longitude) %>% 
  mutate(year = "2020 Primary")


pps <- bind_rows(pps16, pps20)

border <- readOGR("temp/shapes", "actual_good_border")


h <- fortify(counties)

j <- fortify(border)

map <- ggplot() +
  geom_polygon(data = h, aes(x = long, y = lat, group = group), fill = "#EEEEEE", color = "black") +
  geom_path(data = j, aes(x = long, y = lat, group = group), size = 1) +
  coord_equal(xlim = c(-88.2, -87.75), ylim = c(42.8, 43.3)) +
  geom_point(data = pps, aes(x = long, y = lat), color = "red", size = 1) +
  facet_wrap( ~ year) +
  theme_bc(legend.position = "bottom",
           axis.ticks = element_blank(),
           axis.text = element_blank(),
           panel.background = element_rect(fill = alpha("#0077be", 0.5)),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           base_family = "LM Roman 10") +
  labs(x = NULL, y = NULL,
       caption = "Source: Wisconsin Elections Commission.\nNotes: Thin lines show county borders. Thick lines show Milwaukee City border.")
map
saveRDS(map, "temp/map.rds")
