#' ---
#' title: Clean and summarize stops data for Question 2
#' authors: Joe Ornstein & Suhan Kacholia
#' date: 2022-06-26
#' version: 0.1
#' ---

## Question 2: How does the level and duration of incentive
## (i.e., fare subsidy) affect riders’ long-term use of and
## enrollment in low-income transit programs?

library(tidyverse)
library(janitor)

registry <- read_csv("data/raw/question2/LIFT_registry.csv") |>
  clean_names() |>
  mutate(expiration_date = as.Date(expiration_date, '%m/%d/%Y'),
         issue_date = as.Date(date_issued_to_card_holder)) |>
  mutate(duration = as.numeric(expiration_date - issue_date)) |>
  mutate(user_id = str_replace_all(card_id,
                                   '-.*', ''))

boardings <- read_csv("data/raw/question2/LIFT_boardings.csv") |>
  clean_names() |>
  # replace NAs with zero
  mutate(
    across(community_transit:sound_transit, ~replace_na(.x, 0))
  ) |>
  # sum boardings across all systems
  mutate(all_boardings = rowSums(across(where(is.numeric)))) |>
  # and across all weeks
  group_by(card_id) |>
  summarize(all_boardings = sum(all_boardings))

sales <- read.csv("data/raw/question2/LIFT_sales.csv") |>
  clean_names()

# join use and enrollment data with registry
d <- registry |>
  left_join(boardings, by = 'card_id') |>
  # drop ~5,000 with missing values
  filter(!is.na(all_boardings),
         !is.na(language_spoken)) |>
  mutate(language_simplified = case_when(
    language_spoken == 'English' ~ 'English',
    language_spoken == 'Spanish' ~ 'Spanish',
    language_spoken == 'Chinese' ~ 'Chinese',
    TRUE ~ 'Other'))



# save cleaned data to file
save(d, file = 'data/clean/question2.RData')
