library(Greg)
library(stargazer)
library(kableExtra)
library(extrafont)
library(geosphere)
library(Matching)
library(raster)
library(rgeos)
library(SearchTrees)
library(readxl)
library(sp)
library(scales)
library(rdd)
library(tidycensus)
library(rgdal)
library(splitstackshape)
library(tidyverse)
library(data.table)

save <- c("db", "cleanup", "theme_bc", "save")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
}
