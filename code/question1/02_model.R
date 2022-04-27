#' ---
#' title: Estimate treatment effect of fare reinstatement, two-way fixed effects
#' authors: Joe Ornstein & Suhan Kacholia
#' date: 2022-04-27
#' version: 0.2
#' ---


library(tidyverse)
library(fixest)
library(modelsummary)
library(marginaleffects)


## 1. load cleaned datasets ------------------------

d <- read_csv('data/clean/trips.csv')


trips_to_keep <- d %>%
  count(trip_id) %>%
  filter(n >= 17)

trips <- d %>%
  filter(trip_id %in% trips_to_keep$trip_id)


length(unique(trips$trip_id)) # 5,354 unique trip IDs; 97,775 unique trips


treatment_date <- as.Date('2020-10-01')
trips$treated <- as.numeric(trips$date >= treatment_date)

stops <- read_csv('data/raw/stops.csv')




## 2. Estimate average treatment effect (ATE) ------------------------------

twfe <- feols(psngr_boardings ~ treated + covid_cases +
                avg_temp + precipitation | trip_id + day_of_week,
              data = trips)

summary(twfe) # standard errors clustered at the trip level

twfe_poisson <- fepois(psngr_boardings ~ treated + covid_cases +
                         avg_temp + precipitation | trip_id + day_of_week,
                       data = trips)

summary(twfe_poisson)
summary(marginaleffects(twfe_poisson))

# note for my own edification: the Average Marginal Effect is computed like this
# twfe_poisson %>%
#   marginaleffects %>%
#   filter(term == 'treated') %>%
#   pull(dydx) %>%
#   na.omit %>%
#   mean

# there were roughly 0.6 fewer boardings per trip after the treatment (that's robust to just keeping the trips with all 21 days)
# that's a roughly 4.5% decrease in ridership

ggplot(data = trips) +
  geom_histogram(mapping = aes(x=psngr_boardings),
                 color = 'black') +
  theme_minimal() +
  labs(x = 'Boardings Per Trip')

mean(trips$psngr_boardings)

## 3. Estimate heterogeneous treatment effects -------------------------------




