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

m1 <- feols(psngr_boardings ~ treated + covid_cases +
              avg_temp + precipitation | trip_id + day_of_week,
            data = trips)

summary(m1) # standard errors clustered at the trip level

m2 <- fepois(psngr_boardings ~ treated + covid_cases +
               avg_temp + precipitation | trip_id + day_of_week,
             data = trips)

summary(m2)
summary(marginaleffects(m2))

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

## 3. Estimate heterogeneous treatment effects by demographics at first stop --------------------

trips_with_acs <- trips %>%
  filter(!is.na(median_income)) %>%
  mutate(pct_white = white/population*100)



# estimate average treatment effect at three quantiles of median income
m3 <- list(
  'Poorest Third' = feols(psngr_boardings ~ treated + covid_cases +
                            avg_temp + precipitation | trip_id + day_of_week,
                          data = filter(trips_with_acs,
                                        median_income < quantile(trips_with_acs$median_income,
                                                                 0.33333))),
  'Middle Third' = feols(psngr_boardings ~ treated + covid_cases +
                           avg_temp + precipitation | trip_id + day_of_week,
                         data = filter(trips_with_acs,
                                       median_income >= quantile(trips_with_acs$median_income,
                                                                 0.33333),
                                       median_income < quantile(trips_with_acs$median_income,
                                                                0.66666))),
  'Richest Third' = feols(psngr_boardings ~ treated + covid_cases +
                            avg_temp + precipitation | trip_id + day_of_week,
                          data = filter(trips_with_acs,
                                        median_income >= quantile(trips_with_acs$median_income,
                                                                  0.66666)))
)

modelsummary(m3)
modelplot(m3, coef_map = c('treated'='ATE'))


# estimate average treatment effect at three quantiles of pct white
m4 <- list(
  'Least White' = feols(psngr_boardings ~ treated + covid_cases +
                          avg_temp + precipitation | trip_id + day_of_week,
                        data = filter(trips_with_acs,
                                      pct_white < quantile(trips_with_acs$pct_white,
                                                           0.33333))),
  'Kinda White' = feols(psngr_boardings ~ treated + covid_cases +
                          avg_temp + precipitation | trip_id + day_of_week,
                        data = filter(trips_with_acs,
                                      pct_white >= quantile(trips_with_acs$pct_white,
                                                            0.33333),
                                      pct_white < quantile(trips_with_acs$pct_white,
                                                           0.66666))),
  'Very White' = feols(psngr_boardings ~ treated + covid_cases +
                         avg_temp + precipitation | trip_id + day_of_week,
                       data = filter(trips_with_acs,
                                     pct_white >= quantile(trips_with_acs$pct_white,
                                                           0.66666)))
)

modelsummary(m4)
modelplot(m4, coef_map = c('treated'='ATE'))


# estimate average treatment effect at three quantiles of median_age
m5 <- list(
  'Youngest' = feols(psngr_boardings ~ treated + covid_cases +
                          avg_temp + precipitation | trip_id + day_of_week,
                        data = filter(trips_with_acs,
                                      median_age < quantile(trips_with_acs$median_age,
                                                           0.33333))),
  'Middlest' = feols(psngr_boardings ~ treated + covid_cases +
                          avg_temp + precipitation | trip_id + day_of_week,
                        data = filter(trips_with_acs,
                                      median_age >= quantile(trips_with_acs$median_age,
                                                            0.33333),
                                      median_age < quantile(trips_with_acs$median_age,
                                                           0.66666))),
  'Oldest' = feols(psngr_boardings ~ treated + covid_cases +
                         avg_temp + precipitation | trip_id + day_of_week,
                       data = filter(trips_with_acs,
                                     median_age >= quantile(trips_with_acs$median_age,
                                                           0.66666)))
)

modelsummary(m5)
modelplot(m5, coef_map = c('treated'='ATE'))