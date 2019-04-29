# Get Mary the one-pager tables

# load libraries
library(tidyverse)
library(lubridate)

# import 18 data
wk7_13_yr18 <- read.csv("data/walmart2018noncao_filter out CAO.csv", stringsAsFactors = FALSE)

# import 19 data
wk7_13_yr19 <- read.csv("data/walmart2019noncao_filter out CAO.csv", stringsAsFactors = FALSE)

# get '18 CAO stores
yr18_cao_stores <- wk7_13_yr18 %>% 
  filter(Customer == "WALMART #0538 SUPERCENTER" | 
         Customer == "WALMART #0767 SUPERCENTER" |
         Customer == "WALMART #1081 SUPERCENTER" |
         Customer == "WALMART #1297 SUPERCENTER" |
         Customer == "WALMART #3877 SUPERCENTER") %>% 
  select(Date, Dead.Net.Revenue)

# remove '18 CAO stores for rest
yr18_NONcao_stores <- wk7_13_yr18 %>% 
  filter(Customer != "WALMART #0538 SUPERCENTER") %>% 
  filter(Customer != "WALMART #0767 SUPERCENTER") %>%
  filter(Customer != "WALMART #1081 SUPERCENTER") %>%
  filter(Customer != "WALMART #1297 SUPERCENTER") %>%
  filter(Customer != "WALMART #3877 SUPERCENTER") %>% 
  select(Date, Dead.Net.Revenue)

# remove '19 CAO stores
yr19_cao_stores <- wk7_13_yr19 %>% 
  filter(Customer == "WALMART #0538 SUPERCENTER" | 
           Customer == "WALMART #0767 SUPERCENTER" |
           Customer == "WALMART #1081 SUPERCENTER" |
           Customer == "WALMART #1297 SUPERCENTER" |
           Customer == "WALMART #3877 SUPERCENTER") %>% 
  select(Date, Dead.Net.Revenue)

# remove CAO stores for '19 rest
yr19_NONcao_stores <- wk7_13_yr19 %>% 
  filter(Customer != "WALMART #0538 SUPERCENTER") %>% 
  filter(Customer != "WALMART #0767 SUPERCENTER") %>%
  filter(Customer != "WALMART #1081 SUPERCENTER") %>%
  filter(Customer != "WALMART #1297 SUPERCENTER") %>%
  filter(Customer != "WALMART #3877 SUPERCENTER") %>% 
  select(Date, Dead.Net.Revenue)

# Assign weeks
yr18_cao_stores$Date <- as.Date(yr18_cao_stores$Date)
yr18_cao_stores$Date <- yr18_cao_stores$Date - days(5)
yr18_cao_stores$Week <- week(yr18_cao_stores$Date)
yr18_cao_stores$Date <- yr18_cao_stores$Date + days(5)
yr18_cao_stores$Week <- yr18_cao_stores$Week - 3
arrange(yr18_cao_stores, Date)  

yr18_NONcao_stores$Date <- as.Date(yr18_NONcao_stores$Date)
yr18_NONcao_stores$Date <- yr18_NONcao_stores$Date - days(5)
yr18_NONcao_stores$Week <- week(yr18_NONcao_stores$Date)
yr18_NONcao_stores$Date <- yr18_NONcao_stores$Date + days(5)
yr18_NONcao_stores$Week <- yr18_NONcao_stores$Week - 3
arrange(yr18_NONcao_stores, Date) 

yr19_cao_stores$Date <- as.Date(yr19_cao_stores$Date)
yr19_cao_stores$Date <- yr19_cao_stores$Date - days(4)
yr19_cao_stores$Week <- week(yr19_cao_stores$Date)
yr19_cao_stores$Date <- yr19_cao_stores$Date + days(4)
yr19_cao_stores$Week <- yr19_cao_stores$Week - 3
arrange(yr19_cao_stores, Date)  

yr19_NONcao_stores$Date <- as.Date(yr19_NONcao_stores$Date)
yr19_NONcao_stores$Date <- yr19_NONcao_stores$Date - days(4)
yr19_NONcao_stores$Week <- week(yr19_NONcao_stores$Date)
yr19_NONcao_stores$Date <- yr19_NONcao_stores$Date + days(4)
yr19_NONcao_stores$Week <- yr19_NONcao_stores$Week - 3
arrange(yr19_NONcao_stores, Date)  

yr18_cao_stores %>% 
  group_by(Week) %>% 
  summarise(
    DNR = sum(parse_number(Dead.Net.Revenue))
    ) %>% 
  rename(LY = DNR)
