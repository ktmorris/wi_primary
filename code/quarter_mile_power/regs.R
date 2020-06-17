reg_data <- readRDS("./temp/reg_data_no_age_buff.rds")

roll <- readRDS("./temp/match_data.rds") %>% 
  select(-age) %>% 
  filter(distance_border < 0.125)

roll <- roll[complete.cases(roll), ] %>% 
  mutate(id = row_number())

#######################
cities <- readRDS("./temp/mke_voters.rds") %>% 
  select(LALVOTERID, Residence_Addresses_City, County)
cities <- cities[!duplicated(cities$LALVOTERID), ]

roll <- left_join(roll, cities)
rm(cities)
###########################

reg_data <- left_join(reg_data, roll, by = "id")

reg_data$group <- as.character(reg_data$group)

inter <- as.data.table(filter(reg_data))

inter <- inter[ , count := .N, by = .(group, mke)] %>% 
  mutate(weights = 1 / count)

inter <- as.data.table(inter)[, .(weights = sum(weights)),
                              by = list(id, group, County, primary_20,
                                        primary_18, primary_16, white, black,
                                        latino, asian, income, college, dem,
                                        rep, male)] %>% 
  mutate(mke = id == group)


#############################

m1 <- lm(primary_20 ~ mke + County, inter, weights = weights)

m2 <- lm(primary_20 ~ mke +
           primary_18 + primary_16 + white + black +
           latino + asian + income + college + dem +
           rep + male + County, inter, weights = weights)

m3 <- lm(primary_20 ~ mke * black + County, data = inter, weights = weights)

m4 <- lm(primary_20 ~ mke * black +
           primary_18 + primary_16 + white + 
           latino + asian + income + college + dem +
           rep + male + County, inter, weights = weights)

stargazer(m1, m2, m3, m4,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Lives in MKE", "Black", "Black $\\times$ Lives in MKE"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:reg-table} Turnout in 2020 Primary",
          table.placement = "H",
          omit.stat = c("f", "ser", "aic"),
          omit = c("age", "primary_18", "primary_16", "white",
                   "latino", "asian", "income", "college", "dem",
                   "rep", "male", "County"),
          table.layout = "-cmd#-t-a-s-n",
          out.header = F,
          notes = "TO REPLACE",
          se = list(coef(summary(m1, cluster = c("group")))[, 2],
                    coef(summary(m2, cluster = c("group")))[, 2],
                    coef(summary(m3, cluster = c("group")))[, 2],
                    coef(summary(m3, cluster = c("group")))[, 2]),
          add.lines=list(c("Includes Other Matched Covariates" , "", "X", "", "X"),
                         c("Includes County Fixed Effects" , "X", "X", "X", "X")))
