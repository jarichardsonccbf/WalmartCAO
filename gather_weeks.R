# https://ccna-tableau.ko.com/#/site/Walmart/views/WalmartCAOWeeklyBreakdownResults_0/ExcelDownloadDataView?:iid=1

# From microsoft page use S180274@alwaysko.com 
# From sso.connect.coca-cola use jarichardson@cocacolaflorida.com
# Search "Excel download data view"
 
# Download then add columns:
# WM Week (as YYYYWW)
# OWNER

# Change Store Nbr to Store
# Beverage Category to Bev Cat
# Retail Package Group to Pack
# Beverage Product Description to Brand
# Weekly GRS Ordered Units to Weekly GRS Order Units
# Remove "Avg." from days of supply

source("functions/functions.R")

# Calulcate Cases
wk07 <- CaseCount("data/WalmartCAOweek7.csv", "outputs/wk07.cases.csv")
wk08 <- CaseCount("data/WalmartCAOweek8.csv", "outputs/wk08.cases.csv")
wk09 <- CaseCount("data/WalmartCAOweek9.csv", "outputs/wk09.cases.csv")
wk10 <- CaseCount("data/WalmartCAOweek10.csv", "outputs/wk10.cases.csv")
wk11 <- CaseCount("data/WalmartCAOweek11.csv", "outputs/wk11.cases.csv")
wk12 <- CaseCount("data/WalmartCAOweek12.csv", "outputs/wk12.cases.csv")
wk13 <- CaseCount("data/WalmartCAOweek13.csv", "outputs/wk13.cases.csv")
wk14 <- CaseCount("data/WalmartCAOweek14.csv", "outputs/wk14.cases.csv")
wk15 <- CaseCount("data/WalmartCAOweek15.csv", "outputs/wk15.cases.csv")
wk16 <- CaseCount("data/WalmartCAOweek16.csv", "outputs/wk16.cases.csv")
wk17 <- CaseCount("data/WalmartCAOweek17.csv", "outputs/wk17.cases.csv")
wk18 <- CaseCount("data/WalmartCAOweek18.csv", "outputs/wk18.cases.csv")
wk24 <- CaseCount("data/WalmartCAOweek24.csv", "outputs/wk24.cases.csv")
wk25 <- CaseCount("data/WalmartCAOweek25.csv", "outputs/wk25.cases.csv")
wk26 <- CaseCount("data/WalmartCAOweek26.csv", "outputs/wk26.cases.csv")
wk27 <- CaseCount("data/WalmartCAOweek27.csv", "outputs/wk27.cases.csv")
wk28 <- CaseCount("data/WalmartCAOweek28.csv", "outputs/wk28.cases.csv")
wk29 <- CaseCount("data/WalmartCAOweek29.csv", "outputs/wk29.cases.csv")
wk30 <- CaseCount("data/WalmartCAOweek30.csv", "outputs/wk30.cases.csv")
wk31 <- CaseCount("data/WalmartCAOweek31.csv", "outputs/wk31.cases.csv")
wk32 <- CaseCount("data/WalmartCAOweek32.csv", "outputs/wk32.cases.csv")
wk33 <- CaseCount("data/WalmartCAOweek33.csv", "outputs/wk33.cases.csv")
wk34 <- CaseCount("data/WalmartCAOweek34.csv", "outputs/wk34.cases.csv")
wk35 <- CaseCount("data/WalmartCAOweek35.csv", "outputs/wk35.cases.csv")
wk36 <- CaseCount("data/WalmartCAOweek36.csv", "outputs/wk36.cases.csv")
wk37 <- CaseCount("data/WalmartCAOweek37.csv", "outputs/wk37.cases.csv")

# rbind all weeks

dfs = sapply(.GlobalEnv, is.data.frame) 

full <- do.call(rbind, mget(names(dfs)[dfs])) %>% 
  mutate(Week = paste("Week", str_sub(WM.Week, -2))) %>% 
  select(-c(WM.Week))

# get data tidied up, can't do simple gather, must subset dataframes then rbind
full <- full %>%
  mutate(sort.order = Weekly.AM.Order.Cases)

am.orders <- full %>% 
  select(-c(Weekly.GRS.Order.Cases, Weekly.Cases.On.Hand, Weekly.Cases.Sales)) %>% 
  mutate(count_type = "Weekly.AM.Order.Cases") %>% 
  rename(case_count = Weekly.AM.Order.Cases)

grs.orders <- full %>% 
  select(-c(Weekly.AM.Order.Cases, Weekly.Cases.On.Hand, Weekly.Cases.Sales)) %>% 
  mutate(count_type = "Weekly.GRS.Order.Cases") %>% 
  rename(case_count = Weekly.GRS.Order.Cases)

weekly.cases <- full %>% 
  select(-c(Weekly.AM.Order.Cases, Weekly.GRS.Order.Cases, Weekly.Cases.Sales)) %>% 
  mutate(count_type = "Weekly.Cases.On.Hand") %>% 
  rename(case_count = Weekly.Cases.On.Hand)

weekly.sales <- full %>% 
  select(-c(Weekly.AM.Order.Cases, Weekly.GRS.Order.Cases, Weekly.Cases.On.Hand))%>% 
  mutate(count_type = "Weekly.Cases.Sales") %>% 
  rename(case_count = Weekly.Cases.Sales)

tidyfull <- rbind(am.orders, grs.orders, weekly.cases, weekly.sales)

write.csv(tidyfull, "outputs/tableautest.csv")

