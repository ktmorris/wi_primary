reg_data <- readRDS("./temp/reg_data_no_age.rds")

roll <- readRDS("./temp/match_data.rds") %>% 
  select(-age)

roll <- roll[complete.cases(roll), ] %>% 
  mutate(id = row_number())

reg_data <- left_join(reg_data, roll, by = "id")

reg_data$group <- as.character(reg_data$group)
#############################################
pot <- readRDS("./temp/mke_voters.rds")

pot <- filter(pot, LALVOTERID %in% reg_data$LALVOTERID) %>% 
  mutate(GEOID = paste0("55", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0")))

reg_data <- left_join(reg_data, select(pot, LALVOTERID, GEOID, Residence_Addresses_City, County))


covid <- fread("./raw_data/ts_0421.csv")

reg_data <- left_join(reg_data, select(covid, GEOID, DEATHS, POSITIVE, NEGATIVE)) %>% 
  mutate_at(vars(DEATHS, POSITIVE, NEGATIVE), ~ ifelse(. == -999, 2, .)) %>%
  mutate(rate2 = ifelse(POSITIVE == -999 | NEGATIVE == -999, NA, POSITIVE / (POSITIVE + NEGATIVE)))
###################################################

inter <- as.data.table(filter(reg_data, distance <= 0.5))

inter <- inter[ , count := .N, by = .(group, mke)] %>% 
  mutate(weights = 1 / count)

inter <- as.data.table(inter)[, .(weights = sum(weights)),
                              by = list(id, group, County, primary_20,
                                        primary_18, primary_16, white, black,
                                        latino, asian, income, college, dem,
                                        rep, male, rate2)] %>% 
  mutate(mke = id == group,
         black_mke = mke * black)


ll <- inter %>% 
  group_by(mke) %>% 
  summarize_at(vars(starts_with("primary_")), ~ weighted.mean(., weights))

ll <- pivot_longer(ll, -mke, names_to = "year", values_to = "to") %>% 
  mutate(year = as.integer(paste0("20", substring(year, nchar(year) - 1))))

ggplot(ll, aes(x = year, y = to, linetype = mke)) + geom_line()

#############################

m1 <- lm(primary_20 ~ mke + County, inter, weights = weights)

m2 <- lm(primary_20 ~ mke +
           primary_18 + primary_16 + white + black +
           latino + asian + income + college + dem +
           rep + male + County, inter, weights = weights)

m3 <- lm(primary_20 ~ mke + black + black_mke + County, data = inter, weights = weights)

m4 <- lm(primary_20 ~ mke + black + black_mke +
           primary_18 + primary_16 + white + 
           latino + asian + income + college + dem +
           rep + male + County, inter, weights = weights)

m5 <- lm(primary_20 ~ mke + black + black_mke +
           primary_18 + primary_16 + white + 
           latino + asian + income + college + dem +
           rep + male + County + rate2, inter, weights = weights)

stargazer(m1, m2, m3, m4, m5,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Lives in Milwaukee", "Black", "Black $\\times$ Lives in Milwaukee",
                               "Positive Test Rate"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:reg-table} Turnout in 2020 Primary",
          table.placement = "H",
          omit.stat = c("f", "ser", "aic"),
          keep = c("mke", "black", "black_mke", "rate2", "Constant"),
          table.layout = "-cmd#-t-a-s-n",
          out = "./temp/reg_table.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = list(coef(summary(m1, cluster = c("group")))[, 2],
                    coef(summary(m2, cluster = c("group")))[, 2],
                    coef(summary(m3, cluster = c("group")))[, 2],
                    coef(summary(m3, cluster = c("group")))[, 2]),
          add.lines=list(c("Includes Other Matched Covariates" , "", "X", "", "X", "X"),
                         c("Includes County Fixed Effects" , "X", "X", "X", "X", "X")))
###############
cints <- rbindlist(lapply(c((1 / 35.2),
                            seq(0.25, 2, 0.25),
                            seq(2.5, 5, 0.5),
                            c(6:10)), function(m){
  print(m)
  
  inter <- as.data.table(filter(reg_data, distance <= m))
  
  inter <- inter[ , count := .N, by = .(group, mke)] %>% 
    mutate(weights = 1 / count)
  
  inter <- as.data.table(inter)[, .(weights = sum(weights),
                                    primary_20 = mean(primary_20),
                                    black = mean(black)), by = list(id, group, County)] %>% 
    mutate(mke = id == group)
  
  if(length(unique(inter$County)) > 1){
    mod <- lm(primary_20 ~ mke * black + County, inter, weights = weights)
  } else{
    mod <- lm(primary_20 ~ mke * black, inter, weights = weights)
  }
  
  
  ci <- confint(mod)
  
  nr <- nrow(ci)
  
  j <- data.table(distance = m,
                  estimate = mod[["coefficients"]][["mkeTRUE"]],
                  lower = ci[2,1],
                  upper = ci[2,2],
                  type = "overall")
  
  k <- data.table(distance = m,
                  estimate = mod[["coefficients"]][["mkeTRUE:black"]],
                  lower = ci[nr,1],
                  upper = ci[nr,2],
                  type = "black_add")
  
  return(bind_rows(j, k))
}))

saveRDS(cints, "./temp/cints_no_age_interaction.rds")

##############################################
cints <- readRDS("./temp/cints_no_age_interaction.rds") %>% 
  mutate(type = ifelse(type == "overall", "Effect for\nNon-Black Voters",
                       "Additional Effect for\nBlack Voters"))

cints$type <- relevel(as.factor(cints$type), ref = "Effect for\nNon-Black Voters")

plot <- ggplot(filter(cints)) +
  geom_errorbar(aes(x = distance,
                    ymin = lower, ymax = upper), size = 0.2) +
  geom_line(aes(x = distance, y = estimate, linetype = type), size = 0.2) + 
  geom_point(aes(x = distance, y = estimate, shape = type), size = 1.5) + 
  theme_bw() +
  labs(y = "Estimated Coefficient",
       x = "Maximum Distance Between Control and Treated Voters (Miles)",
       caption = "Notes: 95% confidence bars shown.\n\"Effect for Non-Black Voters\" refers to the variable \"Lives in Milwaukee,\"
while \"Additional Effect for Black Voters\" refers to \"Black × Lives in Milwaukee.\"
The overall effect for Black voters is therefore the sum of both estimates.",
       shape = "Group",
       linetype = "Group") +
  scale_y_continuous(labels = scales::percent, breaks = seq(-.15, .1, 0.05)) +
  scale_x_continuous(breaks = seq(0, 12, 2)) +
  theme(text = element_text(family = "LM Roman 10"),
        plot.caption = element_text(hjust = 0),
        legend.key.size = unit(2, 'lines')) +
  scale_color_manual(values = c("gray", "black")) +
  geom_hline(yintercept = 0, linetype = "dashed")

plot
saveRDS(plot, "./temp/coef_plot.rds")
