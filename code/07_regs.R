reg_data <- readRDS("./temp/reg_data.rds")
roll <- readRDS("./temp/match_data.rds")

roll <- roll[complete.cases(roll), ] %>% 
  mutate(id = row_number())

###########################
ll <- reg_data %>% 
  group_by(mke) %>% 
  summarize_at(vars(starts_with("primary_")), ~ weighted.mean(., weights))

ll <- pivot_longer(ll, -mke, names_to = "year", values_to = "to") %>% 
  mutate(year = as.integer(paste0("20", substring(year, nchar(year) - 1))))

ggplot(ll, aes(x = year, y = to, linetype = mke)) + geom_line()

#############################

m1 <- lm(primary_20 ~ mke, reg_data, weights = weights)

m2 <- lm(primary_20 ~ mke +
           age + primary_18 + primary_16 + white + black +
           latino + asian + income + college + dem +
           rep + male, reg_data, weights = weights)

m3 <- lm(primary_20 ~ mke * black, data = reg_data, weights = weights)

m4 <- lm(primary_20 ~ mke * black +
           age + primary_18 + primary_16 + white + 
           latino + asian + income + college + dem +
           rep + male, reg_data, weights = weights)

stargazer(m1, m2, m3, m4,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Lives in MKE", "Black", "Black × Lives in MKE"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:trip-diff} Turnout, 2010 --- 2018",
          table.placement = "H",
          omit.stat = c("f", "ser", "aic"),
          omit = c("age", "primary_18", "primary_16", "white",
                   "latino", "asian", "income", "college", "dem",
                   "rep", "male"),
          table.layout = "-cmd#-t-a-s-n",
          out = "./temp/test.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = list(coef(summary(m1, cluster = c("group_id")))[, 2],
                    coef(summary(m2, cluster = c("group")))[, 2],
                    coef(summary(m3, cluster = c("group")))[, 2],
                    coef(summary(m3, cluster = c("group")))[, 2]),
          add.lines=list(c("Includes Other Matched Covariates" , "", "X", "", "X")))
###############

reg_data <- left_join(reg_data, roll %>% 
                        select(id, black), by = c("group" = "id"))

cints <- rbindlist(lapply(c(1:40), function(m){
  m <- m / 4
  
  inter <- filter(reg_data, distance <= m) %>% 
    group_by(group) %>% 
    mutate(weights = ifelse((sum(weights) == 1) | mke, weights,
                            1 / (n() - 1)))
  
  mod <- lm(primary_20 ~ mke * black.y, inter, weights = weights)
  
  ci <- confint(mod)
  
  j <- data.table(distance = m,
                  estimate = mod[["coefficients"]][["mkeTRUE"]],
                  lower = ci[2,1],
                  upper = ci[2,2],
                  type = "overall")
  
  k <- data.table(distance = m,
                  estimate = mod[["coefficients"]][["mkeTRUE:black.yTRUE"]],
                  lower = ci[4,1],
                  upper = ci[4,2],
                  type = "black_add")
  
  return(bind_rows(j, k))
}))

cints <- mutate(cints, distance_jig = ifelse(type == "black_add", distance + 0.05, distance))

ggplot(cints) +
  geom_errorbar(aes(x = distance_jig,
                    ymin = lower, ymax = upper, color = type), width = .2) +
  geom_line(aes(x = distance_jig, y = estimate, color = type)) + 
  geom_point(aes(x = distance_jig, y = estimate, color = type)) + 
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(y = "Estimated Effect of Living in Milwaukee",
       x = "Maximum Distance Between Control and Treated Voters (Miles)",
       caption = "95% confidence bands shown.") +
  scale_y_continuous(labels = scales::percent)
