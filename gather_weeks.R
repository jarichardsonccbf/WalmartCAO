# package loading
library(tidyverse)

# Get all weeks on one file
wk07 <- read.csv("outputs/wk07.cases.csv")
wk08 <- read.csv("outputs/wk08.cases.csv")
wk09 <- read.csv("outputs/wk09.cases.csv")
wk10 <- read.csv("outputs/wk10.cases.csv")
wk10 <- wk10 %>% 
  select(-c(X..of.Change))
wk11 <- read.csv("outputs/wk11.cases.csv")
wk11 <- wk11 %>% 
  select(-c(X..of.Change))
wk12 <- read.csv("outputs/wk12.cases.csv")
wk12 <- wk12 %>% 
  select(-c(X..of.Change))
wk13 <- read.csv("outputs/wk13.cases.csv")
wk13 <- wk13 %>% 
  select(-c(X..of.Change))
wk14 <- read.csv("outputs/wk14.cases.csv")
wk14 <- wk14 %>% 
  select(-c(X..of.Change))
wk15 <- read.csv("outputs/wk15.cases.csv")
wk15 <- wk15 %>% 
  select(-c(X..of.Change))
wk16 <- read.csv("outputs/wk16.cases.csv")
wk16 <- wk16 %>% 
  select(-c(X..of.Change))
wk17 <- read.csv("outputs/wk17.cases.csv")
wk17 <- wk17 %>% 
  select(-c(X..of.Change))

full <- rbind(wk07, wk08, wk09, wk10, wk11, wk12, wk13, wk14, wk15, wk16, wk17)
write.csv(full, "outputs/allweeks.csv")

# get data tidied up, can't do simple gather, must subset dataframes then rbind
full <- full %>%
  mutate(sort.order = Weekly.AM.Order.Cases) %>% 
  select(-c(X, Weekly.Unit.Sales, Weekly.Units.On.Hand, Weekly.AM.Order.Units, Weekly.GRS.Order.Units))

one <- full %>% 
  select(-c(Weekly.GRS.Order.Cases, Weekly.Cases.On.Hand, Weekly.Cases.Sales)) %>% 
  mutate(count_type = "Weekly.AM.Order.Cases") %>% 
  rename(case_count = Weekly.AM.Order.Cases)

two <- full %>% 
  select(-c(Weekly.AM.Order.Cases, Weekly.Cases.On.Hand, Weekly.Cases.Sales)) %>% 
  mutate(count_type = "Weekly.GRS.Order.Cases") %>% 
rename(case_count = Weekly.GRS.Order.Cases)

three <- full %>% 
  select(-c(Weekly.AM.Order.Cases, Weekly.GRS.Order.Cases, Weekly.Cases.Sales)) %>% 
  mutate(count_type = "Weekly.Cases.On.Hand") %>% 
rename(case_count = Weekly.Cases.On.Hand)

four <- full %>% 
  select(-c(Weekly.AM.Order.Cases, Weekly.GRS.Order.Cases, Weekly.Cases.On.Hand))%>% 
  mutate(count_type = "Weekly.Cases.Sales") %>% 
rename(case_count = Weekly.Cases.Sales)

tidyfull <- rbind(one, two, three, four)

write.csv(tidyfull, "outputs/tidyallweeks.csv")
