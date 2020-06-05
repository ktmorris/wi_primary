d <- fread("C:/Users/morrisk/Documents/Book1.csv", header = F)

d$V1 <- gsub(" Wards | Ward ", "@", d$V1)

d <- cSplit(d, "V1", sep = "@", drop = F, type.convert = F)
d <- cSplit(d, "V1_2", sep = "$", drop = F, type.convert = F, direction = "long")

temp <- filter(d, grepl("-", V1_2))

temp$n <- paste0(temp$V1_1, temp$V1_2)

temp <- cSplit(temp, "V1_2", sep = "-", drop = F, type.convert = F, direction = "long")

temp <- rbindlist(lapply(unique(temp$n), function(s){
  print(s)
  t <- filter(temp, n == s)
  t <- expandRows(t, as.integer(t$V1_2[2]) - as.integer(t$V1_2[1]) + 1, count.is.col = F, drop = F)
  t <- filter(t, row_number() <= (nrow(t) / 2))
  t$V1_2 <- c(as.integer(t$V1_2[1]):(as.integer(t$V1_2[1]) + nrow(t)-1))
  return(t)
}))


d <- filter(d, !(grepl("-", V1_2)))

d1 <- rbind(d, temp %>% select(-n))

fwrite(d1, "madison.csv")
