library(tidyverse)
library(sf)
library(rgeos)
library(gganimate)
library(transformr)

shapename <- read_sf('Question1/Data/KCMStopsData/kcm_stops.shp')
stop.activity <- read_csv("Question1/CleanData/stop_activity_clean.csv")
stops <- cbind(shapename, st_transform(x = shapename, crs = 4326) %>% st_coordinates)
kctracts <- tigris::tracts(state = 53, county = 033, cb = FALSE, year = 2020)
stops.sf <- st_as_sf(data.frame(x = stops$X, y= stops$Y, STOP_ID = stops$STOP_ID), coords = c('x', 'y'), crs = st_crs(kctracts))
stop.in.tract <- st_join(stops.sf, select(kctracts, GEOID), join = st_within)
stop.census <- merge(stop.activity, stop_in_tract, by = "STOP_ID")
stop.sample <- sample_n(stop.census, 200000)
write_csv(stop.sample, "Question1/CleanData/stop_activity_sample.csv")

#ggplot() +
#  geom_sf(data = kctracts, alpha = 0.5) +
#  geom_sf(stop.census.sample, mapping = aes(color = PSNGR_BOARDINGS), alpha = 0.05, size = 0.01)




