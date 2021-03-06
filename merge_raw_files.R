# merge's raw files before conversion to cases. Used for comparing to MM data

library(tidyverse)

wk7 <- read.csv("data/WalmartCAOweek7.csv", stringsAsFactors = FALSE)
wk8 <- read.csv("data/WalmartCAOweek8.csv", stringsAsFactors = FALSE)
wk9 <- read.csv("data/WalmartCAOweek9.csv", stringsAsFactors = FALSE)
wk10 <- read.csv("data/WalmartCAOweek10.csv", stringsAsFactors = FALSE)
wk10 <- wk10 %>% 
  select(-c(X..of.Change))
wk11 <- read.csv("data/WalmartCAOweek11.csv", stringsAsFactors = FALSE)
wk11 <- wk11 %>% 
  select(-c(X..of.Change))
wk12 <- read.csv("data/WalmartCAOweek12.csv", stringsAsFactors = FALSE)
wk12 <- wk12 %>% 
  select(-c(X..of.Change))
wk13 <- read.csv("data/WalmartCAOweek13.csv", stringsAsFactors = FALSE)
wk13 <- wk13 %>% 
  select(-c(X..of.Change))
wk14 <- read.csv("data/WalmartCAOweek14.csv", stringsAsFactors = FALSE)
wk14 <- wk14 %>% 
  select(-c(X..of.Change))
wk15 <- read.csv("data/WalmartCAOweek15.csv", stringsAsFactors = FALSE)
wk15 <- wk15 %>% 
  select(-c(X..of.Change))
wk16 <- read.csv("data/WalmartCAOweek16.csv", stringsAsFactors = FALSE)
wk16 <- wk16 %>% 
  select(-c(X..of.Change))
wk17 <- read.csv("data/WalmartCAOweek17.csv", stringsAsFactors = FALSE)
wk17 <- wk17 %>% 
  select(-c(X..of.Change))
wk18 <- read.csv("data/WalmartCAOweek18.csv", stringsAsFactors = FALSE)

full <- rbind(wk7, wk8, wk9, wk10, wk11, wk12, wk13, wk14, wk15, wk16, wk17, wk18)

write.csv(full, "data/allweeksraw.csv")
