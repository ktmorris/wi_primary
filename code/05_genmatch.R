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


match_data <- select(readRDS("./temp/match_data.rds"), -age)

match_data <- match_data[complete.cases(match_data), ] %>% 
  group_by(mke) %>% 
  sample_frac(0.01) %>% 
  ungroup()

genout <- GenMatch(Tr = match_data$mke,
                   X = match_data %>% 
                     select(-LALVOTERID,
                            -primary_20,
                            -mke,
                            -distance_border), M = 1, pop.size = 150)
saveRDS(genout, "./temp/wi_genout_no_age_1p.rds")