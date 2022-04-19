#' ---
#' title: Clean and summarize stops data for Question 1
#' authors: Joe Ornstein & Suhan Kacholia
#' date: 20222-04-14
#' version: 0.1
#' ---


library(tidyverse)
library(lubridate)


d <- read_csv('data/raw/question1/apc_detailed_09-01-2020_10-31-2020.csv')

covid <- read_csv('data/clean/covid.csv') %>%
  select(date = Result_Date,
         cases = Confirmed_Cases) %>%
  mutate(date = as.Date(date,
                        format = '%m/%d/%Y'))

boardings <- d %>%
  mutate(date = as.Date(OPERATION_DATE,
                        format = '%m/%d/%Y'),
         day_of_week = wday(date, label = TRUE)) %>%
  group_by(date, day_of_week) %>%
  summarize(boardings = sum(PSNGR_BOARDINGS),
            trips = n()) %>%
  mutate(boardings_per_trip = boardings / trips)


ggplot(data = boardings,
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

boardings <- d %>%
  mutate(date = as.Date(OPERATION_DATE,
                        format = '%m/%d/%Y'),
         day_of_week = wday(date, label = TRUE)) %>%
  filter(date >= as.Date('2020-09-19'), # remove rides before the service change
         date >= treatment_date - weeks(2),
         date <= treatment_date + weeks(2),
         day_of_week %in% c('Mon', 'Tue', 'Wed', 'Thu', 'Fri'))


trips_to_keep <- boardings %>%
  count(TRIP_ID) %>%
  filter(n == max(n))

# TODO: Consider keeping trips that are missing 1 or two days for the modeling stage

trips <- boardings %>%
  filter(TRIP_ID %in% trips_to_keep$TRIP_ID)

length(unique(trips$TRIP_ID)) # 2460 unique trip IDs; 46,740 unique trips

trips %>%
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
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_point(alpha = 0.3) +
  geom_vline(xintercept = as.Date('2020-10-01'),
             linetype = 'dashed') +
  geom_vline(xintercept = treatment_date - weeks(2)) +
  geom_vline(xintercept = treatment_date  + weeks(2)) +
  theme_minimal() +
  labs(x='Date', y = 'Confirmed Cases')


## Write cleaned data to csv --------------------

boardings %>%
  select(trip_id = TRIP_ID,
         date,
         day_of_week,
         psngr_boardings = PSNGR_BOARDINGS) %>%
  write_csv('data/clean/apc.csv')
