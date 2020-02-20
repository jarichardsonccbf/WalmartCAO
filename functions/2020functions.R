# package loading
library(tidyverse)
library(Metrics)
library(MMWRweek)

# converts units to cases and cuts out noise
CaseCount2020 <- function(week.name, output.file.location) {
  units.case <- read.csv("data/CAOWeek7units.csv", stringsAsFactors = FALSE)

  units.per.case <- units.case %>%
    mutate(Units.per.case = Units.Ordered..SUM./Cases.Ordered..SUM.) %>%
    select(Pack, Units.per.case) %>%
    distinct()

  wk <- read.csv("data/WalmartCAOweek202001.csv", stringsAsFactors = FALSE)

  wk <- wk %>%
    mutate(week = substring(as.character(WM.Week), 5),
           year = substring(as.character(WM.Week), first = 1, last = 4),
           date = MMWRweek2Date(MMWRyear = as.numeric(year),
                                MMWRweek = as.numeric(week) + 5) -1)

  # convert units to cases

  wk.cases <- wk %>%
    full_join(units.per.case, "Pack") %>%
    drop_na(WM.Week, Units.per.case)

  # java monster, monster rehab, and single gold peaks don't have info on units/case, Mary Williams supplied the     units per case, do these mnaually.

  java.mon <- wk %>%
    filter(Pack == "11C4 - 11OZ4PKCN") %>%
    mutate(Units.per.case = 6)

  mon.rehab4 <- wk %>%
    filter(Pack == "15.5C4 - 15.5OZ 4PK") %>%
    mutate(Units.per.case = 6)

  mon.rehab10 <- wk %>%
    filter(Pack == "15.5C10 - 15.5OZ 10P") %>%
    mutate(Units.per.case = 2)

  single.g.peak <- wk %>%
    filter(Pack == "18.5P1 - 18.5OZ PET SINGLE") %>%
    mutate(Units.per.case = 12)

  # bind the manual to the automatic ones
  wk.pack <- rbind(wk.cases, java.mon, mon.rehab4, mon.rehab10, single.g.peak) %>%
    mutate(Weekly.AM.Order.Cases = Weekly.AM.Order.Units/Units.per.case    ,
           Weekly.GRS.Order.Cases = Weekly.GRS.Order.Units/Units.per.case  ,
           Weekly.Cases.On.Hand = Weekly.Units.On.Hand/Units.per.case      ,
           Weekly.Cases.Sales = Weekly.Unit.Sales/Units.per.case           ,
           case.difference = Weekly.AM.Order.Cases - Weekly.GRS.Order.Cases)

  # filter CCBF and carried by store
  wk.ccbf.instocks <- wk.pack %>%
    filter(OWNER == "CCBF") %>%
    filter(!(Weekly.Unit.Sales == 0 & Weekly.Units.On.Hand == 0 & Weekly.AM.Order.Units == 0)) %>%
    select(WM.Week, OWNER, Store, City, State, Brand, Pack, Days.of.Supply, Weekly.AM.Order.Cases, Weekly.GRS.Order.Cases, Weekly.Cases.On.Hand, Weekly.Cases.Sales, case.difference, week, year, date)

  write.csv(wk.ccbf.instocks, output.file.location)

  return(wk.ccbf.instocks)
}

# Function to calculate interesting metrics for each dataset----
CalculateErrors <- function(df) {
  Percent.Perfect.Match = df %>%
    filter(case.difference == 0) %>%
    count() / nrow(df) * 100

  Percent.Perfect.Match <- rename(Percent.Perfect.Match, Percent.Perfect.Match = n)

  ME = mean(df$case.difference)

  RMSE = sqrt(mean((df$Weekly.AM.Order.Cases - df$Weekly.GRS.Order.Cases)^2))

  RAE = rae(df$Weekly.AM.Order.Cases, df$Weekly.GRS.Order.Cases)

  RSE = rse(df$Weekly.AM.Order.Cases, df$Weekly.GRS.Order.Cases)

  MAE = mean(abs(df$case.difference))

  CoD = summary(lm(df$Weekly.GRS.Order.Cases ~ df$Weekly.AM.Order.Cases))$r.squared

  vec = cbind(Percent.Perfect.Match, ME, MAE, RMSE, RAE, RSE, CoD)

  return(vec)
}
