library(tidyverse)
library(readxl)

stop.activity <- read.csv("Question1/Data/stop_activity_granular_2020-09-01_2020-10-31-002.csv")
apc <- read.csv("Question1/Data/apc_detailed_09-01-2020_10-31-2020.csv")
alltrips <- read.csv("Question1/Data/alltrips_2020-09_to_2020-10.csv")
acs <- read.csv("Question1/Data/King_County_ACS_2019_tract.csv")
acs.key <- read_excel("Question1/Data/ACS_Variables_Selected.xlsx")

stop.activity$OPERATION_DATE <- as.Date(stop.activity$OPERATION_DATE, format = "%Y-%m-%d")
stop.activity <- dplyr::filter(stop.activity, OPERATION_DATE >= as.Date("2020-09-17") & OPERATION_DATE <= as.Date("2020-10-15"))
stop.activity <- select(stop.activity, c("OPERATION_DATE", 
                                         "STOP_ID", "STOP_NM", 
                                         "PSNGR_BOARDINGS", "PSNGR_ALIGHTINGS",
                                         "TRIP_ID"))
apc$OPERATION_DATE <- as.Date(apc$OPERATION_DATE, format = "%m/%d/%Y")
apc <- dplyr::filter(apc, OPERATION_DATE >= as.Date("2020-09-17") & OPERATION_DATE <= as.Date("2020-10-15"))
stop.activity$AFTER_CHANGE <- ifelse(stop.activity$OPERATION_DATE > as.Date("2020-10-01"), 1, 0)
apc$AFTER_CHANGE <- ifelse(apc$OPERATION_DATE > as.Date("2020-10-01"), 1, 0)
  
write_csv(stop.activity, "Question1/CleanData/stop_activity_clean.csv")
write_csv(apc, "Question1/CleanData/apc_clean.csv")

