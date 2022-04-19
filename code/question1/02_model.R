#' ---
#' title: Estimate treatment effect of fare reinstatement, two-way fixed effects
#' authors: Joe Ornstein & Suhan Kacholia
#' date: 20222-04-19
#' version: 0.1
#' ---


library(tidyverse)
library(fixest)
library(modelsummary)


# load cleaned dataset
d <- read_csv('data/clean/apc.csv')


trips_to_keep <- d %>%
  count(trip_id) %>%
  filter(n >= 17)

trips <- d %>%
  filter(trip_id %in% trips_to_keep$trip_id)


length(unique(trips$trip_id)) # 5,354 unique trip IDs; 97,775 unique trips


treatment_date <- as.Date('2020-10-01')
trips$treated <- as.numeric(trips$date >= treatment_date)

twfe <- feols(psngr_boardings ~ treated | trip_id + day_of_week,
              data = trips)

summary(twfe) # standard errors clustered at the trip level

# there were roughly 0.8 fewer boardings per trip after the treatment (that's robust to just keeping the trips with all 21 days)

ggplot(data = trips) +
  geom_histogram(mapping = aes(x=psngr_boardings),
                 color = 'black') +
  theme_minimal() +
  labs(x = 'Boardings Per Trip')

mean(trips$psngr_boardings)


