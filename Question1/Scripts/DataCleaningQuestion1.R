library(tidyverse)
library(readxl)

stop.activity <- read_csv("Question1/Data/stop_activity_granular_2020-09-01_2020-10-31-002.csv")
apc <- read_csv("Question1/Data/apc_detailed_09-01-2020_10-31-2020.csv")
alltrips <- read_csv("Question1/Data/alltrips_2020-09_to_2020-10.csv")
acs <- read_csv("Question1/Data/King_County_ACS_2019_tract.csv")
acs.key <- read_excel("Question1/Data/ACS_Variables_Selected.xlsx")

stop.activity$OPERATION_DATE <- as.Date(stop.activity$OPERATION_DATE, format = "%Y-%m-%d")
stop.activity <- dplyr::filter(stop.activity, OPERATION_DATE >= as.Date("2020-09-17") & OPERATION_DATE <= as.Date("2020-10-15"))

library(sf)
library(rgeos)
library(transformr)

shapename <- read_sf('Question1/Data/KCMStopsData/kcm_stops.shp')
stops <- cbind(shapename, st_transform(x = shapename, crs = 4326) %>% st_coordinates)
kctracts <- tigris::tracts(state = 53, county = 033, cb = FALSE, year = 2020)
stops.sf <- st_as_sf(data.frame(x = stops$X, y= stops$Y, STOP_ID = stops$STOP_ID), coords = c('x', 'y'), crs = st_crs(kctracts))
stop.in.tract <- st_join(stops.sf, select(kctracts, GEOID), join = st_within)
stop.activity <- merge(stop.activity, stop.in.tract, by = "STOP_ID")
stop.activity$AFTER_CHANGE <- as.factor(ifelse(stop.activity$OPERATION_DATE > as.Date("2020-10-01"), 1, 0))
apc$AFTER_CHANGE <- as.factor(ifelse(apc$OPERATION_DATE > as.Date("2020-10-01"), 1, 0))
apc$OPERATION_DATE <- as.Date(apc$OPERATION_DATE, format = "%m/%d/%Y")
apc <- dplyr::filter(apc, OPERATION_DATE >= as.Date("2020-09-17") & OPERATION_DATE <= as.Date("2020-10-15"))

stop.activity <- stop.activity %>%
  select(c("PSNGR_BOARDINGS", "GEOID", "AFTER_CHANGE", "OPERATION_DATE", "STOP_ID")) %>% 
  group_by(STOP_ID, OPERATION_DATE, GEOID, AFTER_CHANGE) %>% 
  summarise(boardings = sum(PSNGR_BOARDINGS))
  
write_csv(stop.activity, "Question1/CleanData/stop_activity_clean.csv")
write_csv(apc, "Question1/CleanData/apc_clean.csv")

acs.key <- acs.key[-25, ]

acs <- acs %>%
  rename("TRACT" = "NAME") %>% 
  select(-contains("M"))

colnames(acs) <- c("X", "GEOID", "TRACT", acs.key$Description)
colnames(acs) <- gsub("Estimate..", "", colnames(acs))
colnames(acs) <- gsub("Total..", "", colnames(acs))

write_csv(acs, "Question1/CleanData/acs_clean.csv")
