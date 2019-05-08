#package loading
library(tidyverse)

#data preprocessing----

#import Margin Minder data, cut out the crap, and rename material number
march.orders <- read.csv("data/marchMMorders.csv", stringsAsFactors = FALSE)
march.orders <- march.orders %>% 
  select(-c(Dead.Net.Revenue, Customer.host.code)) %>% 
  rename(MaterialNo = Material.number)

#import UPC data, get unique values as some products double up.
UPC <- read.csv("data/UPC.csv", stringsAsFactors = FALSE)
UPC <- UPC %>%
  rename(MaterialNo = Product.ID) %>% 
  unique()

#add UPC into MM data then get rid of the UPC df to clear space
march.orders <- march.orders %>%
  left_join(UPC, "MaterialNo") %>% 
  select(-c(Each.UPC))
 
rm(UPC)

#import Walmart's item number and product description, cut out the crap and rename for joining purposes.
walmartitms <- read.csv("data/itmnmbr.upc.csv", stringsAsFactors = FALSE)
walmartitms <- walmartitms %>% 
  select(c(Item.Number,UPC.Number)) %>%
  rename(Case.UPC = UPC.Number)

#our UPC one digit too long, cut the last one off by converting to string, then back to numeric for joining
march.orders$Case.UPC <- str_sub(march.orders$Case.UPC, 1, str_length(march.orders$Case.UPC)-1)

march.orders$Case.UPC <- as.numeric(march.orders$Case.UPC)

#add in Walmart item number to MM data
march.orders <- march.orders %>% 
  left_join(walmartitms, "Case.UPC")

#get rid of body armor since not in grs yet.
march.orders <- march.orders %>% 
  filter(!(stringr::str_detect(Product, 'BODYARMOR')))

#save march.mm.data
#DONOTOVERWRITE!!!!write.csv(march.orders, "outputs/march.mm.data.csv")!!!

march.orders <- read.csv("outputs/march.mm.data.csv", stringsAsFactors = FALSE)

#Missing products from Walmart GRS provided
missing.prod <- march.orders[is.na(march.orders$Item.Number),]
missing.prod <- missing.prod %>% 
  select(Product, Material.Description) %>% 
  unique()
# write.csv(missing.prod, "deliverables/missing.products.csv")

#import walmart GRS data, select CCBF, remove crap, rename for joining
walmartgrs <- read.csv("outputs/allweeks.csv", stringsAsFactors = FALSE)
walmartgrs <- walmartgrs %>% 
  filter(OWNER == "CCBF") %>% 
  select(c(WM.Week, Store, Item.Nbr, Bev.Cat, Pack, Brand)) %>% 
  rename(Item.Number = Item.Nbr)
   
#compare items delivered with this by GRS, are any delivered that are not in GRS?
delivered.items <- march.orders %>% 
  select(Item.Number, Material.Description) %>% 
  unique() %>% 
  rename(Item.Nbr = Item.Number)

wk07 <- read.csv("data/WalmartCAOweek7.csv", stringsAsFactors = FALSE)
wk08 <- read.csv("data/WalmartCAOweek8.csv", stringsAsFactors = FALSE)
wk09 <- read.csv("data/WalmartCAOweek9.csv", stringsAsFactors = FALSE)
wk10 <- read.csv("data/WalmartCAOweek10.csv", stringsAsFactors = FALSE)
wk10 <- wk10 %>% 
  select(-c(X..of.Change))
wk11 <- read.csv("data/WalmartCAOweek11.csv", stringsAsFactors = FALSE)
wk11 <- wk11 %>% 
  select(-c(X..of.Change))
wk12 <- read.csv("data/WalmartCAOweek12.csv")
wk12 <- wk12 %>% 
  select(-c(X..of.Change))
wk13 <- read.csv("data/WalmartCAOweek13.csv")
wk13 <- wk13 %>% 
  select(-c(X..of.Change))
wk14 <- read.csv("data/WalmartCAOweek14.csv")
wk14 <- wk14 %>% 
  select(-c(X..of.Change))
wk.all <- rbind(wk07, wk08, wk09, wk10, wk11, wk12, wk13, wk14)
wk.all <- wk.all %>% 
  filter(OWNER == "CCBF") %>% 
  select(Item.Nbr, Pack, Brand) %>% 
  unique()

non.grs.items <- delivered.items %>% 
  left_join(wk.all, "Item.Nbr")

## DONT OVERWRITE!! write.csv(non.grs.items, "deliverables/non.grs.items.csv")!!