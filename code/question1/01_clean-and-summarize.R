#' ---
#' title: Clean and summarize stops data for Question 1
#' authors: Joe Ornstein & Suhan Kacholia
#' date: 2022-04-27
#' version: 0.2
#' ---


library(tidyverse)
library(lubridate)

trips <- read_csv('data/raw/question1/apc_detailed_09-01-2020_10-31-2020.csv')

stops <- read_csv('data/raw/question1/stop_activity_granular_2020-09-01_2020-10-31-001.csv')

covid <- read_csv('data/clean/covid.csv') %>%
  select(date = Result_Date,
         covid_cases = Confirmed_Cases) %>%
  mutate(date = as.Date(date,
                        format = '%m/%d/%Y'))

weather <- read_csv('data/clean/weather.csv')

boardings_ts <- trips %>%
  mutate(date = as.Date(OPERATION_DATE,
                        format = '%m/%d/%Y'),
         day_of_week = wday(date, label = TRUE)) %>%
  group_by(date, day_of_week) %>%
  summarize(boardings = sum(PSNGR_BOARDINGS),
            trips = n()) %>%
  mutate(boardings_per_trip = boardings / trips)


ggplot(data = boardings_ts,
       mapping = aes(x = date,
                     y = boardings_per_trip)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  theme_minimal() +
  labs(x = 'Date',
       y = 'Average Boardings Per Trip') +
  facet_wrap(~day_of_week) +
  geom_vline(xintercept = as.Date('2020-10-01'),
             linetype = 'dashed')

# to compare apples with apples, let's isolate the TRIP_IDs that ran every week day for the
# two weeks before and after the policy change.

treatment_date <- as.Date('2020-10-01')

trips <- trips %>%
  mutate(date = as.Date(OPERATION_DATE,
                        format = '%m/%d/%Y'),
         day_of_week = wday(date, label = TRUE)) %>%
  filter(date >= as.Date('2020-09-19'), # remove rides before the service change
         date >= treatment_date - weeks(2),
         date <= treatment_date + weeks(2),
         day_of_week %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))


trips_to_keep <- trips %>%
  count(TRIP_ID) %>%
  filter(n == max(n))

# 2460 unique trip IDs; 46,740 unique trips
trips %>%
  filter(TRIP_ID %in% trips_to_keep$TRIP_ID) %>%
  pull(TRIP_ID) %>%
  unique %>%
  length

trips %>%
  filter(TRIP_ID %in% trips_to_keep$TRIP_ID) %>%
  nrow


trips %>%
  filter(TRIP_ID %in% trips_to_keep$TRIP_ID) %>%
  group_by(date) %>%
  summarize(total_boardings = sum(PSNGR_BOARDINGS)) %>%
  ggplot(mapping = aes(x = date,
                       y = total_boardings)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  theme_minimal() +
  labs(x = 'Date',
       y = 'All Boardings') +
  geom_vline(xintercept = as.Date('2020-10-01'),
             linetype = 'dashed')


covid %>%
  mutate(day_of_week = wday(date, label = TRUE)) %>%
  filter(day_of_week %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri')) %>%
  filter(date < '2021-03-01') %>%
  ggplot(mapping = aes(x=date, y=covid_cases)) +
  geom_point(alpha = 0.3) +
  geom_vline(xintercept = as.Date('2020-10-01'),
             linetype = 'dashed') +
  geom_vline(xintercept = treatment_date - weeks(2)) +
  geom_vline(xintercept = treatment_date  + weeks(2)) +
  theme_minimal() +
  labs(x='Date', y = 'Confirmed Cases')

# get daily average temperature and precipitation
weather <- weather %>%
  mutate(date = as.Date(DATE)) %>%
  group_by(date) %>%
  summarize(avg_temp = mean(na.omit(HourlyDryBulbTemperature)),
            precipitation = sum(na.omit(as.numeric(HourlyPrecipitation))))

ggplot(data = weather,
         mapping = aes(x=date,y=avg_temp)) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = as.Date('2020-10-01'),
             linetype = 'dashed') +
  geom_vline(xintercept = treatment_date - weeks(2)) +
  geom_vline(xintercept = treatment_date  + weeks(2)) +
  theme_minimal() +
  labs(x='Date', y='Average Temperature')

ggplot(data = weather,
         mapping = aes(x=date,y=precipitation)) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = as.Date('2020-10-01'),
             linetype = 'dashed') +
  geom_vline(xintercept = treatment_date - weeks(2)) +
  geom_vline(xintercept = treatment_date  + weeks(2)) +
  theme_minimal() +
  labs(x='Date', y='Daily Precipitation')

# merge with cleaned stops and trips data
trips <- trips %>%
  left_join(covid, by = 'date') %>%
  left_join(weather, by = 'date')


stops <- stops %>%
  select(date = OPERATION_DATE,
         trip_id = TRIP_ID,
         stop_id = STOP_ID,
         psngr_boardings = PSNGR_BOARDINGS) %>%
  mutate(date = as.Date(date),
         day_of_week = wday(date, label = TRUE)) %>%
  filter(date >= as.Date('2020-09-19'), # remove rides before the service change
         date >= treatment_date - weeks(2),
         date <= treatment_date + weeks(2),
         day_of_week %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))


## Write cleaned data to csv --------------------

trips %>%
  select(trip_id = TRIP_ID,
         date,
         day_of_week,
         psngr_boardings = PSNGR_BOARDINGS,
         covid_cases,
         avg_temp, precipitation) %>%
  write_csv('data/clean/trips.csv')

# keep trips with at least 17 data points
trips_to_keep <- d %>%
  count(trip_id) %>%
  filter(n >= 17)

stops %>%
  filter(trip_id %in% trips_to_keep$TRIP_ID) %>%
  write_csv('data/raw/stops.csv')
