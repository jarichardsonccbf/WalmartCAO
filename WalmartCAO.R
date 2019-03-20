#package loading
library(tidyverse)

#data preprocessing----
CAO.units.cases <- read.csv("CAOWeek7units.csv", stringsAsFactors = FALSE)

CAOwk7 <- read.csv("WalmartCAOweek7.csv", stringsAsFactors = FALSE)

itmnmbr <- read.csv("itmnmbr.upc.csv", stringsAsFactors = FALSE)
itmnmbr <- itmnmbr %>% 
  rename(Item.Nbr = Item.Number)

CAOwk7 <- CAOwk7 %>% 
  left_join(itmnmbr, "Item.Nbr")

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

CAOwk7
CAOwk7.CCBF
CAOwk7.instocks
CAOwk7.ccbf.instocks

#Function to calculate interesting metrics for each----
CAO <- function(df) {
  ppm = df %>% 
    filter(Case.Difference == 0) %>% 
    count(n()) %>% 
    select(n) / nrow(df) * 100
  
  d5 = df %>% 
    filter(Case.Difference <= 5 & Case.Difference >= -5) %>% 
    count(n()) %>% 
    select(n) / nrow(df) * 100
  
  d10 = df %>% 
    filter(Case.Difference <= 10 & Case.Difference >= -10) %>% 
    count(n()) %>% 
    select(n) / nrow(df) * 100
  
  rms = sqrt(mean((df$Weekly.AM.Order.Units - df$Weekly.GRS.Order.Units)^2))
  
  mae = mean(abs(df$Case.Difference))
  
  list(Percentage.Perfect = ppm, diff5 = d5, diff10 = d10, RMSE = rms, MAE = mae)
}

CAO(CAOwk7)
CAO(CAOwk7.CCBF)
CAO(CAOwk7.instocks)
CAO(CAOwk7.ccbf.instocks)

#product highlights----
highlow <- function(df, cate, N = 10, n = 10) { #cate is category (column to group by) N is top N values, n is bottom n values
  topN <- df %>% 
    group_by_(cate) %>% 
    summarise(avg = mean(Case.Difference)) %>% 
    arrange(desc(avg)) %>% 
    top_n(N)%>% 
    mutate(rank = "top")
    
  bottomn <- df %>% 
    group_by_(cate) %>% 
    summarise(avg = mean(Case.Difference)) %>% 
    arrange(desc(avg)) %>% 
    top_n(-n) %>% 
    mutate(rank = "bottom")
  
  return(rbind(topN, bottomn))
}

CAOwk7.brand <- highlow(CAOwk7, 'Brand', 10, 10) %>% 
  mutate(label = "All")
CAOwk7.CCBF.brand <- highlow(CAOwk7.CCBF, 'Brand') %>% 
  mutate(label = "CCBF")
CAOwk7.instocks.brand <- highlow(CAOwk7.instocks, 'Brand') %>% 
  mutate(label = "All.instock")
CAOwk7.ccbf.instocks.brand <- highlow(CAOwk7.ccbf.instocks, 'Brand') %>% 
  mutate(label = "CCBF.instock")
write.csv(rbind(CAOwk7.brand, CAOwk7.CCBF.brand, CAOwk7.instocks.brand, CAOwk7.ccbf.instocks.brand), "topbrands.csv", sep = ",")


CAOwk7.pack <- highlow(CAOwk7, 'Pack')
CAOwk7.CCBF.pack <- highlow(CAOwk7.CCBF, 'Pack')
CAOwk7.instock.pack <- highlow(CAOwk7.instocks, 'Pack')
CAOwk7.ccbf.instock.pack <- highlow(CAOwk7.ccbf.instocks, 'Pack')

CAOwk7.upc <- highlow(CAOwk7, 'UPC.Number')
CAOwk7.CCBF.upc <- highlow(CAOwk7.CCBF, 'UPC.Number')
CAOwk7.instock.upc <- highlow(CAOwk7.instocks, 'UPC.Number')
CAOwk7.ccbf.instock.upc <- highlow(CAOwk7.ccbf.instocks, 'UPC.Number')

CAOplot <- function(df, xax, yax){
df %>% 
  ggplot(aes(x = xax, y = yax, color = xax)) +
  geom_boxplot() +
  theme(legend.position="none")
}

CAOplot(CAOwk7, CAOwk7$OWNER, CAOwk7$Case.Difference)
CAOplot(CAOwk7.CCBF, as.factor(CAOwk7.CCBF$Store), CAOwk7.CCBF$Case.Difference)
CAOplot(CAOwk7.instocks, CAOwk7.instocks$OWNER, CAOwk7.instocks$Case.Difference)
CAOplot(CAOwk7.ccbf.instocks, as.factor(CAOwk7.ccbf.instocks$Store), CAOwk7.ccbf.instocks$Case.Difference) 
