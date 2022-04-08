library(tidyverse)
library(readxl)

stop.sample <- read_csv("Question1/CleanData/stop_activity_sample.csv")
stop.sample$AFTER_CHANGE <- as.factor(stop.sample$AFTER_CHANGE)
acs <- read.csv("Question1/Data/King_County_ACS_2019_tract.csv")
acs.key <- read_excel("Question1/Data/ACS_Variables_Selected.xlsx")

View(acs.key)

stop.sample.agerace <- merge(stop.sample, select(acs, c("GEOID", "B01002_001E", "B02001_002E")), by = "GEOID")

summary(lm(PSNGR_BOARDINGS~AFTER_CHANGE + GEOID + B01002_001E + B02001_002E, stop.sample.agerace))

summary(lm(PSNGR_BOARDINGS~AFTER_CHANGE, stop.sample))

        