#package loading
library(tidyverse)
library(plotly)

#data preprocessing----
wk7.units.cases <- read.csv("CAOWeek7units.csv", stringsAsFactors = FALSE)

wk7 <- read.csv("WalmartCAOweek7.csv", stringsAsFactors = FALSE)

itmnmbr <- read.csv("itmnmbr.upc.csv", stringsAsFactors = FALSE)
itmnmbr <- itmnmbr %>% 
  rename(Item.Nbr = Item.Number)

wk7 <- wk7 %>% 
  left_join(itmnmbr, "Item.Nbr")

#calculate units per case, just to have ready if needed
wk7.units.cases <- wk7.units.cases %>% 
  mutate(Units.per.case = Units.Ordered..SUM./Cases.Ordered..SUM.)

#df subsets

#just CCBF
wk7.CCBF <- wk7 %>% 
  filter(OWNER == "CCBF")

#just in stocks
wk7.instocks <- wk7 %>% 
  filter(Weekly.Unit.Sales != 0 & Weekly.Units.On.Hand != 0 & Weekly.AM.Order.Units > 0)

#ccbf instocks
wk7.ccbf.instocks <- wk7 %>% 
  filter(OWNER == "CCBF") %>% 
  filter(Weekly.Unit.Sales != 0 & Weekly.Units.On.Hand != 0 & Weekly.AM.Order.Units > 0)

wk7
wk7.CCBF
wk7.instocks
wk7.ccbf.instocks

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

CAO(wk7)
CAO(wk7.CCBF)
CAO(wk7.instocks)
CAO(wk7.ccbf.instocks)

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

#highest and lowest brand differences
wk7.brand <- highlow(wk7, 'Brand', 10, 10) %>% 
  mutate(label = "All")
wk7.CCBF.brand <- highlow(wk7.CCBF, 'Brand') %>% 
  mutate(label = "CCBF")
wk7.instocks.brand <- highlow(wk7.instocks, 'Brand') %>% 
  mutate(label = "All.instock")
wk7.ccbf.instocks.brand <- highlow(wk7.ccbf.instocks, 'Brand') %>% 
  mutate(label = "CCBF.instock")
#write.csv(rbind(wk7.brand, wk7.CCBF.brand, wk7.instocks.brand, wk7.ccbf.instocks.brand), "topbrands.csv", sep = ",")


wk7.pack <- highlow(wk7, 'Pack')
wk7.CCBF.pack <- highlow(wk7.CCBF, 'Pack')
wk7.instock.pack <- highlow(wk7.instocks, 'Pack')
wk7.ccbf.instock.pack <- highlow(wk7.ccbf.instocks, 'Pack')

wk7.upc <- highlow(wk7, 'UPC.Number')
wk7.CCBF.upc <- highlow(wk7.CCBF, 'UPC.Number')
wk7.instock.upc <- highlow(wk7.instocks, 'UPC.Number')
wk7.ccbf.instock.upc <- highlow(wk7.ccbf.instocks, 'UPC.Number')


CAOplot <- function(df, xax, yax){
df %>% 
  ggplot(aes(x = xax, y = yax, color = xax)) +
  geom_boxplot(outlier.size = 2) +
  theme(legend.position="none")
}

CAOplot(wk7, wk7$OWNER, wk7$Case.Difference)

CAOplot(wk7.CCBF, as.factor(wk7.CCBF$Store), wk7.CCBF$Case.Difference) + xlab("Store number") + ylab("Case Differences")

#what is the big one in 767?
wk7.CCBF %>% 
  filter(Store == "767") %>% 
  filter(Case.Difference == max(Case.Difference)) %>% 
  select(Beverage.Product.Description, Pack)

#small one in 538?
wk7.CCBF %>% 
  filter(Store == "538") %>% 
  filter(Case.Difference == min(Case.Difference)) %>% 
  select(Beverage.Product.Description, Pack)

#big in 1081?
wk7.CCBF %>% 
  filter(Store == "1081") %>% 
  filter(Case.Difference == max(Case.Difference)) %>% 
  select(Beverage.Product.Description, Pack)

CAOplot(wk7.instocks, wk7.instocks$OWNER, wk7.instocks$Case.Difference)

CAOplot(wk7.ccbf.instocks, as.factor(wk7.ccbf.instocks$Store), wk7.ccbf.instocks$Case.Difference) 

#Do results vary by city?
wk7.CCBF %>% 
  ggplot(aes(x = City, y = Case.Difference, color = City)) +
  geom_boxplot(outlier.size = 2) +
  theme(legend.position="none") +
  ylab("Case Difference")

wk7.ccbf.instocks %>% 
  ggplot(aes(x = City, y = Case.Difference, color = City)) +
  geom_boxplot() +
  theme(legend.position="none")

#Cases per product
avg.cases <- wk7.ccbf.instocks %>% 
  group_by(Item.Nbr) %>% 
  summarise(meanAM = mean(Weekly.AM.Order.Units), 
            sd = sd(Weekly.AM.Order.Units),
            meanCD = mean(abs(Case.Difference)),
            meanCAO = mean(Weekly.GRS.Order.Units),
            sdCAO = sd(Weekly.GRS.Order.Units)) %>% 
  arrange(desc(meanAM)) %>% 
  mutate(rank = c(152:1))

itmnmbr.avg.case.join <- itmnmbr %>% 
  select(Item.Nbr, Retail.Package.Group, Beverage.Product.Description)

avg.cases$sd[avg.cases$sd=="NaN"] <- 0

avg.cases <- avg.cases %>% 
  left_join(itmnmbr.avg.case.join, key = "Item.Nbr") %>% 
  unite(product, Beverage.Product.Description, Retail.Package.Group)

avg.cases

avg.cases.plot <- avg.cases %>% 
  ggplot(aes(y = meanAM, x = rank, color = meanCD, text = paste("Product:", product))) + 
  geom_point() +
  geom_errorbar(aes(ymin=meanAM-sd, ymax=meanAM+sd), width=.1) +
  coord_flip() +
  scale_y_continuous(breaks=seq(0, 400, 10)) +
  scale_color_gradient2() +
  theme(panel.background = element_rect(color = NA, fill = "dark gray")) +
  xlab("Products ordered AM sales units") +
  ylab("Average sales by product (5 stores)")

ggplotly(avg.cases.plot)

avg.cases.plot2 <- avg.cases %>%
  ggplot() +
  geom_point(aes(y = meanAM, x = rank, color = meanCD, text = paste("Product:", product))) +
  geom_point(aes(y = meanCAO, x = rank, text = paste("Product:", product)))+
  coord_flip() +
  scale_y_continuous(breaks=seq(0, 400, 10)) +
  scale_color_gradient2() +
  theme(panel.background = element_rect(color = NA, fill = "dark gray")) +    xlab("Products ordered AM sales units") +
  ylab("Average sales by product (black points average GRS order)")

ggplotly(avg.cases.plot2)

#Are these mean zeroes because stores don'tg carry them?

storeplot <- function(strnmbr) {

i = wk7.ccbf.instocks %>% 
    filter(Store == strnmbr) %>% 
    nrow()  
  
wk7.ccbf.instocks %>% 
    unite(product, Brand, Pack) %>% 
    filter(Store == strnmbr) %>% 
    arrange(desc(Weekly.AM.Order.Units)) %>%
    mutate(rank = c(i:1)) %>% 
    ggplot() +
    geom_point(aes(y = Weekly.AM.Order.Units, x = rank, color = abs(Case.Difference), text = paste("Product:", product))) +
    geom_point(aes(y = Weekly.GRS.Order.Units, x = rank, text = paste("Product:", product)))+
    coord_flip() +
    scale_y_continuous(breaks=seq(0, 400, 10)) +
    scale_color_gradient2() +
    theme(panel.background = element_rect(color = NA, fill = "dark gray")) +    xlab("Products ordered AM sales units") +
    ylab("Average sales by product (black points average GRS order)")
}

ggplotly(storeplot(538))
ggplotly(storeplot(767))
ggplotly(storeplot(1081))
ggplotly(storeplot(1297))
ggplotly(storeplot(3877))

wk7 %>% 
  ggplot(aes(x = Weekly.AM.Order.Units, y = Weekly.GRS.Order.Units)) +
  geom_jitter() #+
  xlim(0,50) +
  ylim(0,50)
