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

wk8 <- read.csv("WalmartCAOweek8.csv", stringsAsFactors = FALSE)

#convert units to cases
wk8 <- wk8 %>% 
  left_join(units.per.case, "Pack") %>% 
  mutate(Weekly.AM.Order.Units = Weekly.AM.Order.Units/Units.per.case) %>%
  mutate(Weekly.GRS.Order.Units = Weekly.GRS.Order.Units/Units.per.case) %>% na.omit()

#just in stocks
wk8.instocks <- wk8 %>% 
  filter(!(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0))

#ccbf instocks
wk8.ccbf.instocks <- wk8 %>% 
  filter(OWNER == "CCBF") %>% 
  filter(!(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0))

wk8.instocks
wk8.ccbf.instocks

#restrict each to top 93.5% of orders
#all
wk8.instocks %>% 
  ggplot(aes(x=Weekly.AM.Order.Units)) + geom_histogram(bins = 600) +
  xlab("AM order units")

quantile(wk8.instocks$Weekly.AM.Order.Units, 0.935)

wk8.instocks$Weekly.AM.Order.Units[wk8.instocks$Weekly.AM.Order.Units>24] <- 24

wk8.instocks$Weekly.AM.Order.Units[wk8.instocks$Weekly.AM.Order.Units< (-5)]

wk8.instocks$Weekly.AM.Order.Units[wk8.instocks$Weekly.AM.Order.Units< (-10)] <- -10


wk8.instocks$Weekly.GRS.Order.Units[wk8.instocks$Weekly.GRS.Order.Units>24] <- 24

wk8.instocks %>% 
  ggplot(aes(x=Weekly.AM.Order.Units)) + geom_histogram(bins = 35) +
  xlab("AM order units") + 
  coord_cartesian(ylim=c(0,3000),xlim=c(0,25))

wk8.instocks %>% 
  ggplot(aes(x=Weekly.GRS.Order.Units)) + geom_histogram(bins = 25) +
  xlab("GRS order units") +
  coord_cartesian(ylim=c(0,3000))

table(wk8.instocks$Weekly.GRS.Order.Units)

#ccbf
wk7.ccbf.instocks %>% 
  ggplot(aes(x=Weekly.AM.Order.Units)) + geom_histogram(bins = 600) +
  xlab("AM order units")

quantile(wk7.ccbf.instocks$Weekly.AM.Order.Units, 0.935)

wk7.ccbf.instocks$Weekly.AM.Order.Units[wk7.ccbf.instocks$Weekly.AM.Order.Units>29] <-29
wk7.ccbf.instocks$Weekly.GRS.Order.Units[wk7.ccbf.instocks$Weekly.GRS.Order.Units>29] <- 29

wk7.ccbf.instocks %>% 
  ggplot(aes(x=Weekly.AM.Order.Units)) + geom_histogram(bins = 100) +
  xlab("AM order units")

wk7.ccbf.instocks %>% 
  ggplot(aes(x=Weekly.GRS.Order.Units)) + geom_histogram(bins = 100) +
  xlab("GRS order units")

# write.csv(wk7.ccbf.instocks, "ccbf7cases.csv")
#write.csv(wk8.instocks, "all8cases.csv")

#function to visualize difference between levels of various treatments----
CAOplot <- function(df, xax, yax){
  df %>% 
    ggplot(aes(x = xax, y = yax, color = xax)) +
    geom_boxplot(outlier.size = 2) +
    theme(legend.position="none")
}

#applications of above function----
#diff in bottlers?
CAOplot(wk7.instocks, wk7.instocks$OWNER, wk7.instocks$Case.Difference)

#diff in store numbers in ccbf territory?
CAOplot(wk7.ccbf.instocks, as.factor(wk7.ccbf.instocks$Store), wk7.ccbf.instocks$Case.Difference) + xlab("Store number") + ylab("Case Differences")

#what is the big one in 767?
wk7.ccbf.instocks %>% 
  filter(Store == "767") %>% 
  filter(Case.Difference == max(Case.Difference)) %>% 
  select(Brand, Pack)
#This is a consistently high order based on MM data

#small one in 538?
wk7.ccbf.instocks %>% 
  filter(Store == "538") %>% 
  filter(Case.Difference == min(Case.Difference)) %>% 
  select(Brand, Pack)

#big in 1081?
wk7.ccbf.instocks %>% 
  filter(Store == "1081") %>% 
  filter(Case.Difference == max(Case.Difference)) %>% 
  select(Brand, Pack)

#Do results vary by city in ccbf territory?
CAOplot(wk7.ccbf.instocks, as.factor(wk7.ccbf.instocks$City), wk7.ccbf.instocks$Case.Difference) + xlab("Store number") + ylab("Case Differences")

#Case differences per product per product----
avg.cases <- wk7.ccbf.instocks %>% 
  group_by(Item.Nbr) %>% 
  summarise(meanAM = mean(Weekly.AM.Order.Units), 
            sd = sd(Weekly.AM.Order.Units),
            meanCD = mean(abs(Case.Difference)),
            meanCAO = mean(Weekly.GRS.Order.Units),
            sdCAO = sd(Weekly.GRS.Order.Units)) %>% 
  arrange(desc(meanAM)) %>% 
  mutate(rank = c(148:1))

avg.cases

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
  xlab("Rank by average unit sales") +
  ylab("Average sales by product (5 stores)")

#Average order (with SD) colored by average case difference
ggplotly(avg.cases.plot)
#shows that products with higher average order units have more variability in case difference

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
#shows actual difference. Black points to the left of the line are predictions that are UNDERordered, to the right OVERordered.

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

#the above plot by store
ggplotly(storeplot(538))
ggplotly(storeplot(767))
ggplotly(storeplot(1081))
ggplotly(storeplot(1297))
ggplotly(storeplot(3877))

#regression of AM order units by CAO order units first for our stores then by all stores. Using only instock items----
#go to Tableau for zooming in.
wk8.instocks %>% 
  ggplot(aes(x = Weekly.AM.Order.Units, y = Weekly.GRS.Order.Units)) +
  geom_jitter() +
  xlab("AM Order Units") +
  ylab("GRS Order Units") 

wk7.ccbf.instocks %>% 
  ggplot(aes(x = Weekly.AM.Order.Units, y = Weekly.GRS.Order.Units)) +
  geom_jitter() +
  xlab("AM Order Units") +
  ylab("GRS Order Units")

#CCBF GRS orders out of stocks----
no.carry <- wk8 %>%
  filter(OWNER == "CCBF") %>% 
  filter(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0) %>% 
  select(Store, City, Pack, Brand, Weekly.GRS.Order.Units, Case.Difference)

no.carry

#write.csv(no.carry, "no.carry.csv")
#manual inspection of previous sales data reveals these items are not carried by these stores

#Function to calculate interesting metrics for each dataset----
CAO <- function(df) {
  Percent.Perfect.Match = df %>% 
    filter(Case.Difference == 0) %>% 
    count() / nrow(df) * 100
  
  Percent.Perfect.Match <- rename(Percent.Perfect.Match, Percent.Perfect.Match = n)
  
  ME = mean(df$Case.Difference)
  
  RMSE = sqrt(mean((df$Weekly.AM.Order.Units - df$Weekly.GRS.Order.Units)^2))
  
  RAE = rae(df$Weekly.AM.Order.Units, df$Weekly.GRS.Order.Units)
  
  RSE = rse(df$Weekly.AM.Order.Units, df$Weekly.GRS.Order.Units)
  
  MAE = mean(abs(df$Case.Difference))
  
  CoD = summary(lm(df$Weekly.GRS.Order.Units ~ df$Weekly.AM.Order.Units))$r.squared
  
  vec = cbind(Percent.Perfect.Match, ME, MAE, RMSE, RAE, RSE, CoD)
  
  return(vec)
}

#CAO(wk8)
#CAO(wk7.ccbf)
CAO(wk8.instocks)
CAO(wk8.ccbf.instocks)

#Function to calculate interesting metrics for each product----
product_metrics8 <- wk8.ccbf.instocks %>% 
  unite(PRODUCT, Brand, Pack) %>% 
  group_by(PRODUCT) %>% 
  summarise(
      count = n(),
    ME = mean(Case.Difference),
    RMSE = sqrt(mean((Weekly.AM.Order.Units - Weekly.GRS.Order.Units)^2)),
    RAE = rae(Weekly.AM.Order.Units, Weekly.GRS.Order.Units),
    RSE = rse(Weekly.AM.Order.Units, Weekly.GRS.Order.Units),
    MAE = mean(abs(Case.Difference)),
    CoD = summary(lm(Weekly.GRS.Order.Units ~ Weekly.AM.Order.Units))$r.squared
  ) %>% 
  arrange(desc(MAE))

write.csv(product_metrics8 %>% 
            select(PRODUCT, count, ME, MAE), "week8casesproduct.csv")
#NaNs produced from zeroes in GRS units.
#Why inf for some?----
