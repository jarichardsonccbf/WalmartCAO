#This script to determine if the 0,0,0 pattern indicates no sales

#package loading
library(tidyverse)

priororders <- read.csv("walmart5stores.csv", stringsAsFactors = FALSE)

are.these.sold <- wk7.CCBF %>% 
  filter(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0) %>% 
  select(Store, City, Beverage.Category, Retail.Package.Group, Beverage.Product.Description)

priororders
