library(tidyverse)
library(readxl)
library(coefplot)

stop.activity <- read_csv("Question1/CleanData/stop_activity_clean.csv")
acs <- read.csv("Question1/CleanData/acs_clean.csv")

acs$NativeHawaiian.and.Pacific.Islander

stop.activity.race <- merge(stop.activity, select(acs, c("GEOID", "White", "AfricanAmerican", "Asian", 
                                                         "NativeAmerican.or.Alaskan.Native", 
                                                         "NativeHawaiian.and.Pacific.Islander", 
                                                         "Other.Race", "Two.or.more.races")), by = "GEOID")

fit.race <- lm(boardings~AFTER_CHANGE + White + AfricanAmerican + Asian +
             NativeAmerican.or.Alaskan.Native + NativeHawaiian.and.Pacific.Islander +
             Other.Race + Two.or.more.races, stop.activity.race)

summary(fit.race)