library(tidyverse)
library(RJDBC)
library(keyring)
library(lubridate)
library(plotly)

# import and establish hana connection ----

go.live.dates <- read.csv("golives.csv")

options(java.parameters = "-Xmx8048m")
# memory.limit(size=10000000000024)

# classPath="C:/Program Files/sap/hdbclient/ngdbc.jar"
# For ngdbc.jar use        # jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver",
# For HANA Studio jar use  # jdbcDriver <- JDBC(driverClass="com.sap.ndb.studio.jdbc.JDBCConnection",

jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver",
                   classPath="C:/Program Files (x86)/sap/hdbclient/ngdbc.jar")

jdbcConnection <- dbConnect(jdbcDriver,
                            "jdbc:sap://vlpbid001.cokeonena.com:30015/",
                            "fl014036",
                            "jasoN3#w")


# Fetch all results

sql <- 'SELECT VISITTYPE,
                      ACCOUNT,
                      EXECUTIONSTART,
                      EXECUTIONEND,
                      STATUS,
                      SOURCE,
                      ACCOUNT_NAME
         FROM "_SYS_BIC"."cona-reporting.field-sales/Q_CA_R_SpringVisit"
         WHERE
         STATUS = ? and
         SOURCE = ? and
         PLANNEDSTART >= ? and
         PLANNEDEND <= ?'


# VISITTYPE
param2 <- 'FINAL'
param3 <- 'INT'
param4 <- '2019-03-12 00:00:00'
param5 <- '2020-02-07 00:00:00'

spring.visit1 <- dbGetQuery(jdbcConnection, sql, param2, param3, param4, param5)

dbDisconnect(jdbcConnection)

rm(jdbcConnection, jdbcDriver, param2, param3, param4, param5, sql)


# 10/21 go live ----

outlets <- go.live.dates %>%
  filter(DC == "Tampa" |
           DC == "St Petersburg" |
           DC == "Spring Hill") %>%
  droplevels() %>%
  select(outlet, Go.Live, DC, Store.Type) %>%
  rename(ACCOUNT = outlet) %>%
  mutate(ACCOUNT = as.character(ACCOUNT),
         ACCOUNT = paste("0", ACCOUNT, sep = ""))

spring.visit.tpa <- spring.visit1 %>%
  right_join(outlets, by = "ACCOUNT") %>%
  mutate(date = date(EXECUTIONSTART)) %>%
  filter(VISITTYPE == "ZR") %>%
  mutate(Actual.Time..Minutes. = ((ymd_hms(EXECUTIONEND) - ymd_hms(EXECUTIONSTART)) / 60)) %>%
  filter(Actual.Time..Minutes. <= 600 &
           Actual.Time..Minutes. >= 60) %>%
  select(-c(STATUS, SOURCE, EXECUTIONSTART, EXECUTIONEND)) %>%
  mutate(Go.Live = as.Date(Go.Live, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(CAO = as.numeric(date - Go.Live)) %>%
  filter(CAO > 10 | CAO < 0) %>%
  filter(CAO > -98) %>%
  mutate(CAO = case_when(
         date < Go.Live ~ "PRE",
         date >= Go.Live ~ "POST"))

counts.tpa <- spring.visit.tpa %>%
  distinct(DC, date, CAO) %>%
  count(DC, CAO)

counts.tpa

# 11/18 go live ----

# Fetch all results

outlets <- go.live.dates %>%
  filter(DC == "Orlando" |
           DC == "Brevard" |
           DC == "Lakeland" |
           DC == "Jacksonville" |
           DC == "Sebring" |
           DC == "Daytona" |
           DC == "Ft Pierce" |
           DC == "Ocala") %>%
  droplevels() %>%
  select(outlet, Go.Live, DC, Store.Type) %>%
  rename(ACCOUNT = outlet) %>%
  mutate(ACCOUNT = as.character(ACCOUNT),
         ACCOUNT = paste("0", ACCOUNT, sep = ""))

spring.visit.orl <- spring.visit1 %>%
  right_join(outlets, by = "ACCOUNT") %>%
  mutate(date = date(EXECUTIONSTART)) %>%
  filter(VISITTYPE == "ZR") %>%
  mutate(Actual.Time..Minutes. = ((ymd_hms(EXECUTIONEND) - ymd_hms(EXECUTIONSTART)) / 60)) %>%
  filter(Actual.Time..Minutes. <= 600 &
           Actual.Time..Minutes. >= 60) %>%
  select(-c(STATUS, SOURCE, EXECUTIONSTART, EXECUTIONEND)) %>%
  mutate(Go.Live = as.Date(Go.Live, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(CAO = as.numeric(date - Go.Live)) %>%
  group_split(DC)

spring_selector <- function(tib, PRE) {

  dc.rbinder <- tib %>%
    filter(CAO > 10 | CAO < 0) %>%
    filter(CAO > PRE) %>% # decrease to get more PRE dates
    mutate(CAO = case_when(
      date < Go.Live ~ "PRE",
      date >= Go.Live ~ "POST"))

  dc.counts <- dc.rbinder %>%
    count(DC, CAO)

  newlist <- list(dc.rbinder, dc.counts)

  return(newlist)

}

BREVARD <- spring_selector(spring.visit.orl[[1]], -64)

DAYTONA <- spring_selector(spring.visit.orl[[2]], -73)

FT.PIERCE <- spring_selector(spring.visit.orl[[3]], -71)

JACKSONVILLE <- spring_selector(spring.visit.orl[[4]], -72)

ORLANDO <- spring_selector(spring.visit.orl[[5]], -73)

OCALA <- spring_selector(spring.visit.orl[[6]], -70)

ORLANDO <- spring_selector(spring.visit.orl[[7]], -68)

SEBRING <- spring_selector(spring.visit.orl[[8]], -65)

# GAINESVILLE ----

# Fetch all results

outlets <- go.live.dates %>%
  filter(DC == "Gainesville") %>%
  droplevels() %>%
  select(outlet, Go.Live, DC, Store.Type) %>%
  rename(ACCOUNT = outlet) %>%
  mutate(ACCOUNT = as.character(ACCOUNT),
         ACCOUNT = paste("0", ACCOUNT, sep = ""))

spring.visit.gvl <- spring.visit1 %>%
  right_join(outlets, by = "ACCOUNT") %>%
  mutate(date = date(EXECUTIONSTART)) %>%
  filter(VISITTYPE == "ZR") %>%
  mutate(Actual.Time..Minutes. = ((ymd_hms(EXECUTIONEND) - ymd_hms(EXECUTIONSTART)) / 60)) %>%
  filter(Actual.Time..Minutes. <= 600 &
           Actual.Time..Minutes. >= 60) %>%
  select(-c(STATUS, SOURCE, EXECUTIONSTART, EXECUTIONEND)) %>%
  mutate(Go.Live = as.Date(Go.Live, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(CAO = as.numeric(date - Go.Live)) %>%
  filter(CAO > 10 | CAO < 0) %>%
  filter(CAO < 62) %>%
  mutate(CAO = case_when(
    date < Go.Live ~ "PRE",
    date >= Go.Live ~ "POST"))

counts.gvl <- spring.visit.gvl %>%
  distinct(DC, date, CAO) %>%
  count(DC, CAO)

counts.gvl

# Summaries ----

# bind locations

spring.visit.all <- rbind(spring.visit.gvl, spring.visit.tpa, BREVARD[[1]], DAYTONA[[1]], FT.PIERCE[[1]], JACKSONVILLE[[1]], OCALA[[1]], ORLANDO[[1]], SEBRING[[1]])

rm(go.live.dates, spring.visit1, outlets, spring.visit.gvl, spring.visit.tpa)

visit.count.all <- rbind(counts.gvl, counts.tpa, BREVARD[[2]], DAYTONA[[2]], FT.PIERCE[[2]], JACKSONVILLE[[2]], OCALA[[2]], ORLANDO[[2]], SEBRING[[2]])

rm(counts.gvl, counts.tpa, BREVARD, DAYTONA, FT.PIERCE, JACKSONVILLE, OCALA, ORLANDO, SEBRING, spring.visit.orl, spring_selector)

# use for a cutoff point

# a <- spring.visit.all %>%
#   arrange(as.numeric(Actual.Time..Minutes.)) %>%
#   mutate(order = 1:n()) %>%
#   ggplot(aes(x = order, y = as.numeric(Actual.Time..Minutes.))) +
#   geom_point()
#
# ggplotly(a)
#
# spring.visit.all <- spring.visit.all %>%
#   mutate(Actual.Time..Minutes. = as.numeric(Actual.Time..Minutes.)) %>%
#   filter(Actual.Time..Minutes. <= 600 &
#          Actual.Time..Minutes. >= 60)
#
# rm(a)

spring.visit.avg <- spring.visit.all %>%
  group_by(ACCOUNT, Store.Type, CAO, DC) %>%
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T))

# write.csv(spring.visit.avg, "deliverables/store_averages.csv", row.names = FALSE)
