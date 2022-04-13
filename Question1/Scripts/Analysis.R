library(tidyverse)
library(readxl)

stop.activity <- read_csv("Question1/CleanData/stop_activity_clean.csv")
acs <- read.csv("Question1/CleanData/acs_clean.csv")

stop.activity.merged <- merge(stop.activity, select(acs, c("GEOID", "B01002_001E", "B02001_002E")), by = "GEOID")

summary(lm(boardings~AFTER_CHANGE + B01002_001E + B02001_002E, stop.activity.merged))

summary(lm(boardings~AFTER_CHANGE, stop.acitivity.merged))

colnames(stop.sample)

View(stop.activity)

View(acs)


colnames(stop.activity.merged)
