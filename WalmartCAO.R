#package loading
library(tidyverse)

#data preprocessing----
CAO.units.cases <- read.csv("CAOWeek7units.csv", stringsAsFactors = FALSE)

CAOwk7 <- read.csv("WalmartCAOweek7.csv", stringsAsFactors = FALSE)

itmnmbr <- read.csv("itmnmbr.upc.csv", stringsAsFactors = FALSE)

#calculate units per case, just to have ready if needed
CAO.units.cases <- CAO.units.cases %>% 
  mutate(Units.per.case = Units.Ordered..SUM./Cases.Ordered..SUM.)

#df subsets

#just CCBF
CAOwk7.CCBF <- CAOwk7 %>% 
  filter(OWNER == "CCBF")

#just in stocks
CAOwk7.instocks <- CAOwk7 %>% 
  filter(Weekly.Unit.Sales != 0 & Weekly.Units.On.Hand != 0 & Weekly.AM.Order.Units > 0)

#ccbf instocks
CAOwk7.ccbf.instocks <- CAOwk7 %>% 
  filter(OWNER == "CCBF") %>% 
  filter(Weekly.Unit.Sales != 0 & Weekly.Units.On.Hand != 0 & Weekly.AM.Order.Units > 0)

#Function to calculate interesting metrics for each
CAO <- function(df) {
  ppm = df %>% 
    filter(Case.Difference == 0) %>% 
    count(n()) %>% 
    select(n) / nrow(df) * 100
  
  rms = sqrt(mean((df$Weekly.AM.Order.Units - df$Weekly.GRS.Order.Units)^2))
  
  mae = mean(abs(df$Case.Difference))
  
  list(Percentage.Perfect = ppm, RMSE = rms, MAE = mae)
}

CAO(CAOwk7)
CAO(CAOwk7.CCBF)
CAO(CAOwk7.instocks)
CAO(CAOwk7.ccbf.instocks)
