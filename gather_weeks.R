#package loading
library(tidyverse)
library(plotly)
library(Metrics)

#Get 7, 8 on one file
wk7 <- read.csv("outputs/wk7.cases.csv")
wk8 <- read.csv("outputs/wk8.cases.csv")

full <- rbind(wk7, wk8)
write.csv(full, "outputs/allweeks.csv")

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
