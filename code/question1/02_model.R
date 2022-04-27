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

tracts <- read_csv('data/clean/tracts.csv')

tracts$treated <- as.numeric(tracts$date >= treatment_date)


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

## 3. Estimate heterogeneous treatment effects at the tract level ------------------

gid <- '53033028403'

ggplot(data = filter(tracts, GEOID == gid),
       mapping = aes(x=date, y=normalized_boardings)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  geom_vline(xintercept = as.Date('2020-10-01'), linetype = 'dashed') +
  labs(x='Date', y = 'Boardings Relative to September 21')

m3 <- feols(normalized_boardings ~ treated + covid_cases +
                avg_temp + precipitation | day_of_week,
              data = tracts %>% filter(GEOID == gid))
summary(m3)