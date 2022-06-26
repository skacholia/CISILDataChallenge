library(tidyverse)

registry <- read_csv("Question2/Data/LIFT_registry.csv")
boardings <- read.csv("Question2/Data/LIFT_boardings.csv")
sales <- read.csv("Question2/Data/LIFT_sales.csv")
boardings_new <- read.csv("Question2/Data/LIFT_boardings_2021-11-01_to_2022-03-06.csv")
sales_new <- read.csv("Question2/Data/LIFT_sales_2021-11-01_to_2022-03-06.csv")
registry_new <- read.csv("Question2/Data/LIFT_registry_2022-03-22.csv")
registry <- rename(registry, Expiration = ExpirationDate, DateIssued = DateIssuedToCardHolder)

boardings <- rbind(boardings, boardings_new)
sales <- rbind(sales, sales_new)
registry <- rbind(registry, registry_new)

sales$week <- as.Date(sales$week)
boardings$week <- as.Date(boardings$week)

