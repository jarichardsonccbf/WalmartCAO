#package loading
library(tidyverse)

#Get 7, 8, 9, 10 on one file
wk07 <- read.csv("outputs/wk07.cases.csv")
wk08 <- read.csv("outputs/wk08.cases.csv")
wk09 <- read.csv("outputs/wk09.cases.csv")
wk10 <- read.csv("outputs/wk10.cases.csv")
wk10 <- wk10 %>% 
  select(-c(X..of.Change))
wk11 <- read.csv("outputs/wk11.cases.csv")
wk11 <- wk11 %>% 
  select(-c(X..of.Change))

full <- rbind(wk07, wk08, wk09, wk10, wk11)
write.csv(full, "outputs/allweeks.csv")

#get data tidied up, can't do simple gather, must subset dataframes then rbind
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