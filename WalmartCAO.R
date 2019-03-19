#package loading
library(tidyverse)

#data preprocessing----
CAO.units.cases <- read.csv("CAOWeek7units.csv", stringsAsFactors = FALSE)

CAOwk7 <- read.csv("WalmartCAOweek7.csv", stringsAsFactors = FALSE)

itmnmbr <- read.csv("itmnmbr.upc.csv", stringsAsFactors = FALSE)

#calculate units per case, just to have ready if needed
CAO.units.cases <- CAO.units.cases %>% 
  mutate(Units.per.case = Units.Ordered..SUM./Cases.Ordered..SUM.)

#percentage of perfect matches----

#number of perfect matches
perfect_match <- CAOwk7 %>% 
  filter(Case.Difference == 0) %>% 
  count(n()) %>% 
  select(n)

#Get %age of perfect matches
perfect_match / nrow(CAOwk7) * 100
#19.56291% of orders match

#Does %age change if we remove instances of WUS, WUoH, and WAMOU at 0
zeroes.out <- CAOwk7 %>% 
  filter(Weekly.Unit.Sales != 0 & Weekly.Units.On.Hand != 0 & Weekly.AM.Order.Units > 0)

perfect_match_sub <- zeroes.out %>% 
  filter(Case.Difference == 0) %>% 
  count(n()) %>% 
  select(n)

perfect_match_sub / nrow(zeroes.out) * 100
#8.702771% of orders match

#Do values differ for our stores?

#number of perfect matches
perfect_match_CF <- CAOwk7 %>% 
  filter(OWNER == "CCBF") %>% 
  filter(Case.Difference == 0) %>% 
  count(n()) %>% 
  select(n)

#Get %age of perfect matches
perfect_match_CF / nrow(CAOwk7 %>% 
                          filter(OWNER == "CCBF")) * 100
#18.540% for us vs 19.56291% whole

#Does %age change if we remove instances of WUS, WUoH, and WAMOU at 0
zeroes.out.CF <- CAOwk7 %>%
  filter(OWNER == "CCBF") %>% 
  filter(Weekly.Unit.Sales != 0 & Weekly.Units.On.Hand != 0 & Weekly.AM.Order.Units > 0)

perfect_match_sub.CF <- zeroes.out.CF %>% 
  filter(Case.Difference == 0) %>% 
  count(n()) %>% 
  select(n)

perfect_match_sub.CF / nrow(zeroes.out.CF) * 100
#6.66% vs 8.702771% for whole

#RMSE----
RMSE = function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

RMSE(CAOwk7$Weekly.AM.Order.Units, CAOwk7$Weekly.GRS.Order.Units)
#37.51782

#Mean absolute difference----
mean(abs(CAOwk7$Case.Difference))
hist(abs(CAOwk7$Case.Difference), breaks = 400)

