#Get 7, 8 on one file with the 24 case cap
wk7 <- read.csv("all7cases.csv")
wk8 <- read.csv("all8cases.csv")

full <- rbind(wk7, wk8)
write.csv(full, "allweeks.csv")

#Get 7, 8 on one file with no cap
#package loading
library(tidyverse)
library(plotly)
library(Metrics)

#this one for doing CASES ordered

#data preprocessing----
itmnmbr <- read.csv("itmnmbr.upc.csv", stringsAsFactors = FALSE)
itmnmbr <- itmnmbr %>% 
  rename(Item.Nbr = Item.Number)

units.case <- read.csv("CAOWeek7units.csv", stringsAsFactors = FALSE)

units.per.case <- units.case %>% 
  mutate(Units.per.case = Units.Ordered..SUM./Cases.Ordered..SUM.) %>% 
  select(Pack, Units.per.case) %>% 
  distinct()

wk7 <- read.csv("WalmartCAOweek7.csv", stringsAsFactors = FALSE)

#convert units to cases
wk7 <- wk7 %>% 
  left_join(units.per.case, "Pack") %>% 
  mutate(Weekly.AM.Order.Cases = Weekly.AM.Order.Units/Units.per.case) %>%
  mutate(Weekly.GRS.Order.Cases = Weekly.GRS.Order.Units/Units.per.case) %>% na.omit()

#just in stocks
wk7.instocks <- wk7 %>% 
  filter(!(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0))

wk8 <- read.csv("WalmartCAOweek7.csv", stringsAsFactors = FALSE)

#convert units to cases
wk8 <- wk8 %>% 
  left_join(units.per.case, "Pack") %>% 
  mutate(Weekly.AM.Order.Cases = Weekly.AM.Order.Units/Units.per.case) %>%
  mutate(Weekly.GRS.Order.Cases = Weekly.GRS.Order.Units/Units.per.case) %>% na.omit()

#just in stocks
wk8.instocks <- wk8 %>% 
  filter(!(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0))

fullnocap <- rbind(wk7.instocks, wk8.instocks)
write.csv(fullnocap, "allweeks.nocap.csv")
