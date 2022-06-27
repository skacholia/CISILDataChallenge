#' ---
#' title: Estimate how incentives affect use and enrollment
#' authors: Joe Ornstein & Suhan Kacholia
#' date: 2022-06-26
#' version: 0.1
#' ---

library(tidyverse)
library(ebal)

load('data/clean/question2.RData')

d |>
  group_by(initial_load) |>
  summarize(num_cards = n(),
            pct_white = sum(race_desc == 'White') / n(),
            pct_english = sum(language_spoken == 'English') / n(),
            average_age = mean(age))

# note the imbalance.

# There are too few observations in the other categories, so we'll compare
# the $10 level with the subsidized annual pass

## Question 2a: do the riders with subsidized passes use transit more? -----

# Entropy Balancing:
# conditioning on age, race, language,
# num_weeks, issue date, and
# tract-level % white, median age, and median income

d2 <- d |>
  filter(initial_load %in% c('10', 'Subsidized Annual Pass')) |>
  mutate(treated = as.numeric(initial_load == 'Subsidized Annual Pass')) |>
  filter(!is.na(tract_median_income),
         !is.na(weekly_boardings))

eb.out <- ebalance(Treatment = d2$treated,
                   # dummy encode the factors
                   X = model.matrix(~ 1 + age + race_desc +
                                      language_simplified +
                                      num_weeks + issue_date_numeric +
                                      tract_white + tract_median_age +
                                      tract_median_income,
                                    d2)[,-1])

# merge the weights vector
control_weights <- d2 |>
  filter(treated == 0) |>
  mutate(weight = eb.out$w) |>
  select(card_id, weight)

d2 <- d2 |>
  left_join(control_weights, by = 'card_id') |>
  # treated weights are all 1
  mutate(weight = replace_na(weight, 1))

d2 |>
  group_by(treated) |>
  summarize(average_age = mean(age),
            weighted_average_age = weighted.mean(age, weight),
            pct_white = mean(race_desc == 'White'),
            weighted_pct_white = weighted.mean(race_desc == 'White',
                                               weight))

d2 |>
  group_by(treated) |>
  summarize(pct_english = mean(language_simplified == 'English'),
            weighted_pct_english = weighted.mean(language_simplified == 'English',
                                                 weight),
            avg_num_weeks = mean(num_weeks),
            weighted_avg_num_weeks = weighted.mean(num_weeks,
                                                   weight))
# tract level variables
d2 |>
  group_by(treated) |>
  summarize(tract_median_age = mean(tract_median_age),
            tract_median_inc = mean(tract_median_income))

d2 |>
  group_by(treated) |>
  summarize(weighted_median_age = weighted.mean(tract_median_age, weight),
            weighted_median_inc = weighted.mean(tract_median_income,
                                                weight))

# now compare transit boardings
d2 |>
  group_by(treated) |>
  summarize(mean_boardings = mean(weekly_boardings),
            weighted_mean_boardings = weighted.mean(weekly_boardings,
                                                    weight))

ggplot(data = d2) +
  geom_histogram(mapping = aes(x=weekly_boardings),
                 color = 'black') +
  facet_wrap(~initial_load) +
  theme_minimal() +
  labs(x='Weekly Boardings', y=NULL)

ggplot(data = d2) +
  geom_histogram(mapping = aes(x=issue_date),
                 color = 'black') +
  facet_wrap(~initial_load) +
  theme_minimal() +
  labs(x='Issue Date', y=NULL)

ggplot(data = d2) +
  geom_histogram(mapping = aes(x=tract_median_income),
                 color = 'black') +
  facet_wrap(~initial_load) +
  theme_minimal() +
  labs(x='Tract-Level Median Income', y=NULL)

ggplot(data = d2) +
  geom_histogram(mapping = aes(x=age),
                 color = 'black') +
  facet_wrap(~initial_load) +
  theme_minimal() +
  labs(x='Age', y=NULL)

model_ebal <- lm(weekly_boardings ~ treated,
                 data = d2,
                 weights = weight)

summary(model_ebal)
