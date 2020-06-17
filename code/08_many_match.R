match_data <- select(readRDS("./temp/match_data.rds"), -age)

match_data <- match_data[complete.cases(match_data), ]

genout <- readRDS("./temp/wi_genout_no_age_1p.rds")


for(i in c((1 / 35.2),
           seq(0.25, 2, 0.25),
           seq(2.5, 5, 0.5),
           c(6:10))){
  i = i / 2
  
  t <- filter(match_data, distance_border < i)
  
  X <- t %>%
    select(-LALVOTERID,
           -primary_20,
           -mke,
           -distance_border)
  
  mout <- Matchby(Tr = t$mke, X = X,
                  by = c(X$primary_18,
                         X$primary_16,
                         X$dem,
                         X$rep), estimand = "ATT", Weight.matrix = genout, M = 2)
  
  save(mout, file = paste0("./temp/mout_wi_", i, ".RData"))
}

