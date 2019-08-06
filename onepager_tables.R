# Get Mary the one-pager tables

# load libraries
library(tidyverse)
library(lubridate)

# import 18 data
yr18 <- read.csv("data/walmart2018.csv", stringsAsFactors = FALSE)

# import 19 data
yr19 <- read.csv("data/walmart2019.csv", stringsAsFactors = FALSE)

# get '18 CAO stores
yr18_cao_stores <- yr18 %>% 
  filter(Customer == "WALMART #0538 SUPERCENTER" | 
         Customer == "WALMART #0767 SUPERCENTER" |
         Customer == "WALMART #1081 SUPERCENTER" |
         Customer == "WALMART #1297 SUPERCENTER" |
         Customer == "WALMART #3877 SUPERCENTER") %>% 
  select(Date, Dead.Net.Revenue)

# remove '18 CAO stores for rest
yr18_NONcao_stores <- yr18 %>% 
  filter(Customer != "WALMART #0538 SUPERCENTER") %>% 
  filter(Customer != "WALMART #0767 SUPERCENTER") %>%
  filter(Customer != "WALMART #1081 SUPERCENTER") %>%
  filter(Customer != "WALMART #1297 SUPERCENTER") %>%
  filter(Customer != "WALMART #3877 SUPERCENTER") %>% 
  select(Date, Dead.Net.Revenue)

# remove '19 CAO stores
yr19_cao_stores <- yr19 %>% 
  filter(Customer == "WALMART #0538 SUPERCENTER" | 
           Customer == "WALMART #0767 SUPERCENTER" |
           Customer == "WALMART #1081 SUPERCENTER" |
           Customer == "WALMART #1297 SUPERCENTER" |
           Customer == "WALMART #3877 SUPERCENTER") %>% 
  select(Date, Dead.Net.Revenue)

# remove CAO stores for '19 rest
yr19_NONcao_stores <- yr19 %>% 
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

#weird data point, omit
yr18_NONcao_stores <- head(arrange(yr18_NONcao_stores, Date),-1)

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

CAO_stores_sales <- cbind(
yr18_cao_stores %>% 
  group_by(Week) %>% 
  summarise(
    DNR = sum(parse_number(Dead.Net.Revenue))
    ) %>% 
  rename(LY_DNR = DNR),

yr19_cao_stores %>% 
  group_by(Week) %>% 
  summarise(
    DNR = sum(parse_number(Dead.Net.Revenue))
    ) %>% 
  rename(TY_DNR = DNR)
)

NON_CAO_stores_sales <- cbind(
  yr18_NONcao_stores %>% 
    group_by(Week) %>% 
    summarise(
      DNR = sum(parse_number(Dead.Net.Revenue))
    ) %>% 
    rename(LY_DNR = DNR),
  
  yr19_NONcao_stores %>% 
    group_by(Week) %>% 
    summarise(
      DNR = sum(parse_number(Dead.Net.Revenue))
    ) %>% 
    rename(TY_DNR = DNR)
)

CAO_stores_sales
NON_CAO_stores_sales

write.table(CAO_stores_sales, "deliverables/cao_v_non_cao_sales.csv", col.names=TRUE, sep=",")
write.table(NON_CAO_stores_sales, "deliverables/cao_v_non_cao_sales.csv", col.names=FALSE, sep=",", append=TRUE)
