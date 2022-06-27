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
  summarize(mean(all_boardings),
            num_cards = n(),
            pct_white = sum(race_desc == 'White') / n(),
            pct_english = sum(language_spoken == 'English') / n(),
            average_age = mean(age))

# note the imbalance.

# condition on language and race
d |>
  filter(is.na(initial_load) | initial_load == 10) |>
  filter(race_desc == 'White') |>
  filter(language_spoken == 'English') |>
  group_by(initial_load) |>
  summarize(boardings = mean(all_boardings),
            num_cards = n(),
            average_age = mean(age),
            median_issue_date = median(issue_date))

## Entropy balancing, conditioning on age, race, language, and issue date -----
d2 <- d |>
  filter(initial_load %in% c(10, 15)) |>
  mutate(treated = as.numeric(initial_load == 15))
  #filter(is.na(initial_load) | initial_load == 10) |>
  #mutate(treated = as.numeric(!is.na(initial_load)))

eb.out <- ebalance(Treatment = d2$treated,
                   # need to dummy encode the factors
                   X = model.matrix(~ 1 + age + race_desc + language_simplified,
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
                                                 weight))

# now compare transit boardings
d2 |>
  group_by(treated) |>
  summarize(mean_boardings = mean(weekly_boardings),
            weighted_mean_boardings = weighted.mean(weekly_boardings,
                                                    weight))

ggplot(data = d2) +
  geom_histogram(mapping = aes(x=all_boardings)) +
  facet_wrap(~treated)
