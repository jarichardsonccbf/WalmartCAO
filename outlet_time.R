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
                   classPath="C:/Program Files/sap/hdbclient/ngdbc.jar")

jdbcConnection <- dbConnect(jdbcDriver,
                            "jdbc:sap://vlpbid001.cokeonena.com:30015/",
                            "fl014036",
                            "jasoN3#w")


# TAMPA ----

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
param4 <- '2019-06-15 00:00:00'
param5 <- '2019-12-07 00:00:00'

spring.visit1 <- dbGetQuery(jdbcConnection, sql, param2, param3, param4, param5)

outlets <- go.live.dates %>% 
  filter(DC == "Tampa") %>% 
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
  select(-c(STATUS, SOURCE, EXECUTIONSTART, EXECUTIONEND)) %>% 
  mutate(Go.Live = as.Date(Go.Live, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y")) %>% 
  mutate(CAO = as.numeric(date - Go.Live)) %>% 
  filter(CAO < -93 | CAO > 10) %>% 
  mutate(CAO = case_when(
         date < Go.Live ~ "pre",
         date >= Go.Live ~ "post"))

spring.visit.tpa %>% 
  distinct(date, CAO) %>% 
  count(CAO)


# GAINESVILLE ----

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
param4 <- '2019-03-09 00:00:00'
param5 <- '2019-06-31 00:00:00'

spring.visit <- dbGetQuery(jdbcConnection, sql, param2, param3, param4, param5)

dbDisconnect(jdbcConnection)

rm(jdbcConnection, jdbcDriver, param2, param3, param4, param5, sql)

outlets <- go.live.dates %>% 
  filter(DC == "Gainesville") %>% 
  droplevels() %>% 
  select(outlet, Go.Live, DC, Store.Type) %>% 
  rename(ACCOUNT = outlet) %>% 
  mutate(ACCOUNT = as.character(ACCOUNT),
         ACCOUNT = paste("0", ACCOUNT, sep = ""))

spring.visit.gvl <- spring.visit %>% 
  right_join(outlets, by = "ACCOUNT") %>% 
  mutate(date = date(EXECUTIONSTART)) %>% 
  filter(VISITTYPE == "ZR") %>% 
  mutate(Actual.Time..Minutes. = ((ymd_hms(EXECUTIONEND) - ymd_hms(EXECUTIONSTART)) / 60)) %>% 
  select(-c(STATUS, SOURCE, EXECUTIONSTART, EXECUTIONEND)) %>% 
  mutate(Go.Live = as.Date(Go.Live, format = "%m/%d/%Y"),
         date = as.Date(date, format = "%m/%d/%Y")) %>% 
  mutate(CAO = as.numeric(date - Go.Live)) %>%
  filter(date < "2019-04-14" | date > "2019-04-21") %>% 
  filter(CAO > -37 & CAO < 29) %>% 
  mutate(CAO = case_when(
    date < Go.Live ~ "pre",
    date >= Go.Live ~ "post"))

spring.visit.gvl %>% 
  distinct(date, CAO) %>% 
  count(CAO)


# Summaries ----

#bind locations

spring.visit.all <- rbind(spring.visit.gvl, spring.visit.tpa)

a <- spring.visit.all %>% 
  arrange(as.numeric(Actual.Time..Minutes.)) %>% 
  mutate(order = 1:n()) %>% 
  ggplot(aes(x = order, y = as.numeric(Actual.Time..Minutes.))) + 
  geom_point()

ggplotly(a)
  
spring.visit.all <- spring.visit.all %>% 
  mutate(Actual.Time..Minutes. = as.numeric(Actual.Time..Minutes.)) %>% 
  filter(Actual.Time..Minutes. <= 600 &
         Actual.Time..Minutes. >= 60) 

spring.visit.avg <- spring.visit.all %>% 
  group_by(ACCOUNT, Store.Type, CAO, DC) %>% 
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T))

write.csv(spring.visit.avg, "deliverables/store_averages.csv", row.names = FALSE)

spring.visit.all %>% 
  group_by(Store.Type, CAO) %>% 
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T)) %>% 
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>% 
  ggplot(aes(x = Store.Type, y = as.numeric(`Average minutes in outlet`), fill = CAO)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = `Average minutes in outlet` - SD, ymax = `Average minutes in outlet` + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  ylim(0, 500) +
  xlab("Store Type") +
  ylab("Average Time in Outlet (minutes)") +
  theme_bw()

spring.visit.all %>% 
  group_by(DC, Store.Type, CAO) %>% 
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T)) %>% 
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>% 
  ggplot(aes(x = Store.Type, y = as.numeric(`Average minutes in outlet`), fill = CAO)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = `Average minutes in outlet` - SD, ymax = `Average minutes in outlet` + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  ylim(0, 500) +
  xlab("Store Type") +
  ylab("Average Time in Outlet (minutes)") +
  facet_wrap(~ DC) +
  theme_bw() 

spring.visit.all %>% 
  group_by(ACCOUNT, CAO) %>% 
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T)) %>% 
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>% 
  ggplot(aes(x = ACCOUNT, y = as.numeric(`Average minutes in outlet`), fill = CAO)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = `Average minutes in outlet` - SD, ymax = `Average minutes in outlet` + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  ylim(0, 600) +
  xlab("Store Type") +
  ylab("Average Time in Outlet (minutes)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
