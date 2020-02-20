# performance metrics matching time frame of TIO data

source("outlet_time.R")

library(readxl)

gvl.pre <- spring.visit.avg %>%
  filter(CAO == "PRE") %>%
  filter(DC == "Gainesville")

gvl.post <- spring.visit.avg %>%
  filter(CAO == "POST") %>%
  filter(DC == "Gainesville")

orl.pre <- spring.visit.avg %>%
  filter(CAO == "PRE") %>%
  filter(DC == "Orlando" |
           DC == "Brevard" |
           DC == "Lakeland" |
           DC == "Jacksonville" |
           DC == "Sebring" |
           DC == "Daytona" |
           DC == "Ft Pierce" |
           DC == "Ocala")

orl.post <- spring.visit.avg %>%
  filter(CAO == "POST") %>%
  filter(DC == "Orlando" |
           DC == "Brevard" |
           DC == "Lakeland" |
           DC == "Jacksonville" |
           DC == "Sebring" |
           DC == "Daytona" |
           DC == "Ft Pierce" |
           DC == "Ocala")

tpa.pre <- spring.visit.avg %>%
  filter(CAO == "PRE") %>%
  filter(DC == "Tampa" |
           DC == "St Petersburg" |
           DC == "Spring Hill")

tpa.post <- spring.visit.avg %>%
  filter(CAO == "POST") %>%
  filter(DC == "Tampa" |
           DC == "St Petersburg" |
           DC == "Spring Hill")

MM_uploader <- function(dataframe.mm, dataframe.time) {
  read_excel(dataframe.mm, skip = 9) %>%
    filter(Customer != "Totals") %>%
    mutate(ACCOUNT = paste(0, `Customer host code`, sep = "")) %>%
    select(ACCOUNT, Volume, `Dead Net Revenue`, `Dead Net Gross Profit`) %>%
    right_join(dataframe.time, "ACCOUNT")
}

gvl.pre <- MM_uploader("data/Gvl_pre.xlsx", gvl.pre)
gvl.post <- MM_uploader("data/Gvl_post.xlsx", gvl.post)
tpa.pre <- MM_uploader("data/Tpa_pre.xlsx", tpa.pre)
tpa.post <- MM_uploader("data/Tpa_post.xlsx", tpa.post)
orl.pre <- MM_uploader("data/Orl_pre.xlsx", orl.pre)
orl.post <- MM_uploader("data/Orl_post.xlsx", orl.post)

spring.visit.perf <- rbind(gvl.pre, gvl.post, tpa.pre, tpa.post, orl.pre, orl.post)

rm(gvl.post, gvl.pre, orl.post, orl.pre, tpa.post, tpa.pre)
