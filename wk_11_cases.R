#package loading
library(tidyverse)
library(plotly)
library(Metrics)

#data preprocessing----
itmnmbr <- read.csv("data/itmnmbr.upc.csv", stringsAsFactors = FALSE)
itmnmbr <- itmnmbr %>% 
  rename(Item.Nbr = Item.Number)

units.case <- read.csv("data/CAOWeek7units.csv", stringsAsFactors = FALSE)

units.per.case <- units.case %>% 
  mutate(Units.per.case = Units.Ordered..SUM./Cases.Ordered..SUM.) %>% 
  select(Pack, Units.per.case) %>% 
  distinct()

wk11 <- read.csv("data/WalmartCAOweek11.csv", stringsAsFactors = FALSE)

#convert units to cases
wk11pack <- wk11 %>% 
  left_join(units.per.case, "Pack") %>% 
  drop_na()

#java monster, monster rehab, and single gold peaks don't have info on units/case, Mary Williams supplied the number 6 and 12, do these mnaually. 
java.mon <- wk11 %>% 
  filter(Pack == "11C4 - 11OZ4PKCN") %>% 
  mutate(Units.per.case = 6)

mon.rehab <- wk11 %>% 
  filter(Pack == "15.5C4 - 15.5OZ 4PK") %>% 
  mutate(Units.per.case = 6)

single.g.peak <- wk11 %>% 
  filter(Pack == "18.5P1 - 18.5OZ PET SINGLE") %>% 
  mutate(Units.per.case = 12)

#bind the manual to the automatic ones
wk11 <- rbind(wk11pack, java.mon, mon.rehab, single.g.peak) %>% 
  mutate(Weekly.AM.Order.Cases = Weekly.AM.Order.Units/Units.per.case) %>%
  mutate(Weekly.GRS.Order.Cases = Weekly.GRS.Order.Units/Units.per.case) %>% 
  mutate(Weekly.Cases.On.Hand = Weekly.Units.On.Hand/Units.per.case) %>% 
  mutate(Weekly.Cases.Sales = Weekly.Unit.Sales/Units.per.case) 

#just carried by store
wk11.instocks <- wk11 %>% 
  filter(!(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0))

#ccbf instocks
wk11.ccbf.instocks <- wk11 %>% 
  filter(OWNER == "CCBF") %>% 
  filter(!(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0))

wk11.instocks
wk11.ccbf.instocks

write.csv(wk11.instocks, "outputs/wk11.cases.csv")

#regression of AM order Cases by CAO order Cases first for our stores then by all stores. Using only instock items----
#go to Tableau for zooming in.
wk11.instocks %>% 
  ggplot(aes(x = Weekly.AM.Order.Cases, y = Weekly.GRS.Order.Cases)) +
  geom_jitter() +
  xlab("AM Order Cases") +
  ylab("GRS Order Cases") 

wk11.ccbf.instocks %>% 
  ggplot(aes(x = Weekly.AM.Order.Cases, y = Weekly.GRS.Order.Cases)) +
  geom_jitter() +
  xlab("AM Order Cases") +
  ylab("GRS Order Cases")

#CCBF GRS orders for products we don't carry----
no.carry <- wk11 %>%
  filter(OWNER == "CCBF") %>% 
  filter(Weekly.Unit.Sales == 0 & Weekly.Cases.On.Hand == 0 & Weekly.AM.Order.Cases == 0) %>% 
  select(Store, City, Pack, Brand, Weekly.GRS.Order.Cases, Case.Difference)

no.carry

write.csv(no.carry, "outputs/CCBF.wk11.no.carry.csv")
#manual inspection of previous sales data reveals these items are not carried by these stores

#Calculate errors
source("functions/functions.R")

CalculateErrors(wk11.instocks)
CalculateErrors(wk11.ccbf.instocks)

#Calculate errors for products
product_metrics11 <- wk11.ccbf.instocks %>% 
  unite(PRODUCT, Brand, Pack) %>% 
  group_by(PRODUCT) %>% 
  summarise(
    count = n(),
    ME = mean(Case.Difference),
    RMSE = sqrt(mean((Weekly.AM.Order.Cases - Weekly.GRS.Order.Cases)^2)),
    RAE = rae(Weekly.AM.Order.Cases, Weekly.GRS.Order.Cases),
    RSE = rse(Weekly.AM.Order.Cases, Weekly.GRS.Order.Cases),
    MAE = mean(abs(Case.Difference)),
    CoD = summary(lm(Weekly.GRS.Order.Cases ~ Weekly.AM.Order.Cases))$r.squared
  ) %>% 
  arrange(desc(MAE))

write.csv(product_metrics11 %>% 
            select(PRODUCT, count, ME, MAE), "outputs/wk11.cases.product.error.csv")