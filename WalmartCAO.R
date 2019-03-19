#package loading
library(tidyverse)

#data preprocessing----
CAO.units.cases <- read.csv("CAOWeek7units.csv", stringsAsFactors = FALSE)

CAOwk7 <- read.csv("WalmartCAOweek7.csv", stringsAsFactors = FALSE)

itmnmbr <- read.csv("itmnmbr.upc.csv", stringsAsFactors = FALSE)

#calculate units per case, just to have ready if needed
CAO.units.cases <- CAO.units.cases %>% 
  mutate(Units.per.case = Units.Ordered..SUM./Cases.Ordered..SUM.)

#number of perfect matches
perfect_match <- CAOwk7 %>% 
  filter(Case.Difference == 0) %>% 
  count(n()) %>% 
  select(n)

#Get %age of perfect matches
perfect_match / nrow(CAOwk7) * 100

#Does %age change if we remove instances of 