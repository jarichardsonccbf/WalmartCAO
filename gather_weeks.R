#Get 7, 8 on one file
wk7 <- read.csv("all7cases.csv")
wk8 <- read.csv("all8cases.csv")

full <- rbind(wk7, wk8)
write.csv(full, "allweeks.csv")
