# DATA BASICS ----

library(tidyverse)

## Data Types ----
?c
a <- c(1, 2, 3)
a %>% class()
b <- c("low", "medium", "high")
b %>% class()

## Data Structure ---

ab_tbl <- tibble(a, b)

ab_tbl


read_rds("00_data/bike_sales/data_wrangled/ad/bike_orderlines.rds")
