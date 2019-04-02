#package loading
library(tidyverse)

#data preprocessing----
march.orders <- read.csv("data/marchMMorders.csv", stringsAsFactors = FALSE)
march.orders <- march.orders %>% 
  select(-c(Dead.Net.Revenue, Customer.host.code)) %>% 
  rename(MaterialNo = Material.number)

UPC <- read.csv("data/UPC.csv", stringsAsFactors = FALSE)
UPC <- UPC %>%
  rename(MaterialNo = Product.ID) %>% 
  unique()

#add UPC into MM data
march.orders <- march.orders %>%
  left_join(UPC, "MaterialNo") %>% 
  select(-c(Each.UPC))
 
rm(UPC)

walmartitms <- read.csv("data/itmnmbr.upc.csv", stringsAsFactors = FALSE)
walmartitms <- walmartitms %>% 
  select(c(Item.Number,UPC.Number)) %>%
  rename(Case.UPC = UPC.Number)

#our UPC one digit too long?
march.orders$Case.UPC <- str_sub(march.orders$Case.UPC, 1, str_length(march.orders$Case.UPC)-1)

march.orders$Case.UPC <- as.numeric(march.orders$Case.UPC)

#add in Walmart item number to MM data
march.orders <- march.orders %>% 
  left_join(walmartitms, "Case.UPC")

march.orders <- march.orders %>% 
  filter(!(stringr::str_detect(Product, 'BODYARMOR')))

#march.mm.data
#DONOTOVERWRITE!!!!write.csv(march.orders, "outputs/march.mm.data.csv")!!!

march.orders <- read.csv("outputs/march.mm.data.csv", stringsAsFactors = FALSE)

#Missing products from GRS (also did not appear in Walmart items spreadsheet)
missingprod <- march.orders[is.na(march.orders$Item.Number),]
missingprod <- missingprod %>% 
  select(Product, Material.Description) %>% 
  unique()
# write.csv(missingprod, "deliverables/missing.products.csv")

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

wk7 <- read.csv("data/WalmartCAOweek7.csv", stringsAsFactors = FALSE)
wk8 <- read.csv("data/WalmartCAOweek8.csv", stringsAsFactors = FALSE)
wk9 <- read.csv("data/WalmartCAOweek9.csv", stringsAsFactors = FALSE)

wk.all <- rbind(wk7, wk8, wk9)
wk.all <- wk.all %>% 
  filter(OWNER == "CCBF") %>% 
  select(Item.Nbr, Pack, Brand) %>% 
  unique()

non.grs.items <- delivered.items %>% 
  left_join(wk.all, "Item.Nbr")

##DONT OVERWRITE!!write.csv(non.grs.items, "deliverables/non.grs.items.csv")!!

