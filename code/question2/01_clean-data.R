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

registry <- read_csv("data/raw/question2/LIFT_registry_2022-04-01.csv") |>
  clean_names() |>
  mutate(expiration_date = as.Date(expiration, '%m/%d/%Y'),
         issue_date = as.Date(date_issued)) |>
  mutate(duration = as.numeric(expiration_date - issue_date)) |>
  mutate(user_id = str_replace_all(card_id,
                                   '-.*', '')) |>
  # study period is October 2020 to present (after the fare suspension ended)
  filter(issue_date > '2020-10-01')

boardings1 <- read_csv("data/raw/question2/LIFT_boardings.csv") |>
  clean_names() |>
  # remove boardings before October 2020 (fare suspension ended that month)
  filter(week > '2020-10-01')

boardings2 <- read_csv('data/raw/question2/LIFT_boardings_2021-11-01_to_2022-03-06.csv') |>
  clean_names()

# bind the two datasets and clean up
boardings <- bind_rows(boardings1, boardings2) |>
  # replace NAs with zero
  mutate(
    across(community_transit:sound_transit, ~replace_na(.x, 0))
  ) |>
  # sum boardings across all systems
  mutate(all_boardings = rowSums(across(where(is.numeric)))) |>
  # and across all weeks
  group_by(card_id) |>
  summarize(all_boardings = sum(all_boardings),
            num_weeks = n()) |>
  mutate(weekly_boardings = all_boardings / num_weeks)

sales <- read.csv("data/raw/question2/LIFT_sales.csv") |>
  clean_names()

# load tract-level selected variables from ACS
acs <- read_csv("data/raw/King_County_ACS_2019_tract.csv") %>%
  select(tract_fips = GEOID,
         tract_median_age = B01002_001E,
         tract_population = B01003_001E,
         tract_white = B02001_002E,
         tract_median_income = B06011_001E)


# join use, enrollment, and tract-level data with registry
d <- registry |>
  left_join(boardings, by = 'card_id') |>
  # drop ~5,000 with missing values
  filter(!is.na(all_boardings),
         !is.na(language_spoken)) |>
  mutate(language_simplified = case_when(
    language_spoken == 'English' ~ 'English',
    language_spoken == 'Spanish' ~ 'Spanish',
    language_spoken == 'Chinese' ~ 'Chinese',
    TRUE ~ 'Other')) |>
  # make these factors
  mutate(race_desc = factor(race_desc),
         language_simplified = factor(language_simplified)) |>
  # convert issue date to numeric for matching
  mutate(issue_date_numeric = as.numeric(issue_date - min(issue_date))) |>
  # join with tract data
  mutate(
    tract_fips = fips |>
      str_sub(1, 11) |>
      as.numeric()
    ) |>
  left_join(acs, by = 'tract_fips')



# save cleaned data to file
save(d, file = 'data/clean/question2.RData')
