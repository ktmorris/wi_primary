on_nyu <- F

if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
  
  setwd("/scratch/km3815/nyc_displ")
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}


match_data <- readRDS("./temp/match_data.rds") %>% 
  select(-age) %>% 
  filter(distance_border < 0.125)

match_data <- match_data[complete.cases(match_data), ]

buffer <- readOGR("./temp/shapes",
                  "quarter_buffer")

buffer <- spTransform(buffer, "+init=epsg:4326")


ggplot() +
  geom_path(data = fortify(buffer), aes(x = long, y = lat, group = group)) +
  geom_point(data = match_data, aes(x = long, y = lat, color = mke)) +
  coord_equal() +
  theme_bw()

# match_data2 <- match_data[complete.cases(match_data), ] %>% 
#   group_by(mke) %>% 
#   sample_frac(0.1) %>% 
#   ungroup()
# 
# genout <- GenMatch(Tr = match_data2$mke,
#                    X = match_data2 %>%
#                      select(-LALVOTERID,
#                             -primary_20,
#                             -mke,
#                             -distance_border), M = 1, pop.size = 150)
# saveRDS(genout, "./temp/wi_genout_no_age_1p_buff.rds")
genout <- readRDS("./temp/wi_genout_no_age_1p_buff.rds")
##################

ids <- match_data %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

X <- match_data %>% 
  select(-LALVOTERID,
         -primary_20,
         -mke,
         -distance_border)

mout <- Matchby(Tr = match_data$mke, X = X,
                by = c(X$primary_18,
                       X$primary_16,
                       X$dem,
                       X$rep), estimand = "ATT", Weight.matrix = genout, M = 2)

save(mout, file = "./temp/mout_wi_no_age_buff.RData")

load("./temp/mout_wi_no_age_buff.RData")
#####################

roll <- readRDS("./temp/match_data.rds") %>% 
  select(-age) %>% 
  filter(distance_border < 0.125)

roll <- roll[complete.cases(roll), ] %>% 
  mutate(id = row_number())

varnames <- c("primary_18", "primary_16", "white", "black",
              "latino", "asian", "income", "college", "dem",
              "rep", "male")

# # this takes hours
balance <- MatchBalance(mke ~ primary_18 + primary_16 + white +
                          white + black + latino + asian + income + college +
                          dem + rep + male,
                        data = match_data, match.out = mout)
# saveRDS(balance, "./temp/balance_table_full_match_no_age_buff.rds")
# 
balance <- readRDS("./temp/balance_table_full_match_no_age_buff.rds")

TrMean <- c()
PreMean <- c()
PreQQmed <- c()
PreQQmean <- c()
PreQQmax <- c()
PostMean <- c()
PostQQmed <- c()
PostQQmean <- c()
PostQQmax <- c()

for(i in c(1:length(balance$BeforeMatching))){
  TrMean <- unlist(c(TrMean, balance$BeforeMatching[[i]][3][1]))
  PreMean <- unlist(c(PreMean, balance$BeforeMatching[[i]][4][1]))
  PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[i]]$qqsummary[2]))
  PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[i]]$qqsummary[1]))
  PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[i]]$qqsummary[3]))
  
  PostMean <- unlist(c(PostMean, balance$AfterMatching[[i]][4][1]))
  PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[i]]$qqsummary[2]))
  PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[i]]$qqsummary[1]))
  PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[i]]$qqsummary[3]))
}



df <- data.frame("TrMean" = TrMean,
                 "TrMean2" = TrMean,
                 "PreMean" = PreMean,
                 "PreQQmed" = PreQQmed,
                 "PreQQmean" = PreQQmean,
                 "PreQQmax" = PreQQmax,
                 "PostMean" = PostMean,
                 "PostQQmed" = PostQQmed,
                 "PostQQmean" = PostQQmean,
                 "PostQQmax" = PostQQmax,
                 "names" = varnames) %>%
  mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
         change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
         change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
         change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), ~ comma(round(., 2), accuracy = .01)) %>%
  mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), ~ round(. * 100, 2)) %>% 
  filter(names != "voted_primary")


####

df <- full_join(df, 
                fread("./raw_data/var_orders.csv"),
                by = c("names" = "variable")) %>%
  arrange(order) %>%
  select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
  filter(!is.na(TrMean))


df <- df %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(name == "Income", dollar(round(as.numeric(gsub(",", "", .)))), .)) %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(substring(name, 1, 1) == "%", percent(as.numeric(.), accuracy = .1), .))

colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")

saveRDS(df, "./temp/balance_table_full_no_age_buff.rds")

#########################
j <- knitr::kable(df, booktabs = T, caption = "(\\#tab:full-bal) Balance Table", linesep = "") %>% 
  add_header_above(c(" " = 1, "Means: Unmatched Data" = 2, "Means: Matched Data" = 2, "Percent Improvement" = 4), align = "c") %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position"))
j


################################

cleanup(c("roll", "mout"))

matches <- data.table(treated = mout$index.treated,
                      control = mout$index.control,
                      weight = mout$weights)


matches <- left_join(matches,
                     select(roll, treated_lat = lat, treated_lon = long, id, treated_16 = primary_16),
                     by = c("treated" = "id"))

matches <- left_join(matches,
                     select(roll, control_lat = lat, control_lon = long, id, control_16 = primary_16),
                     by = c("control" = "id"))

matches$dist <- (distHaversine(cbind(matches$control_lon, matches$control_lat),
                               cbind(matches$treated_lon, matches$treated_lat)) / 1000) * 0.621371

distance <- weighted.mean(matches$dist, matches$weight)

#################

ids <- data.table(id = c(matches$treated, matches$control),
                  group = rep(matches$treated, 2),
                  weights = rep(matches$weight, 2),
                  distance = rep(matches$dist, 2))

saveRDS(ids, "./temp/reg_data_no_age_buff.rds")

