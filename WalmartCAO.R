#package loading
library(tidyverse)

#data preprocessing----
CAO.units.cases <- read.csv("CAOWeek7units.csv", header = TRUE, stringsAsFactors = FALSE)

CAOwk7 <- read.csv("WalmartCAOweek7.csv", header = TRUE, stringsAsFactors = FALSE)

