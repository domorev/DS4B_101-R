# TIDY DATA EXAMPLE ----

library(tidyverse)
library(readxl)


bikeshop_revenue_wide_tbl <- 
    read_excel("00_data/bike_sales/data_wrangled/bikeshop_revenue_formatted_wide.xlsx")

# Wide Format ----

bikeshop_revenue_wide_tbl


# Long Format ----

bikeshop_revenue_wide_tbl %>%
    select(-Total) %>%
    pivot_longer(cols=Mountain:Road, 
                 names_to = "category_1", 
                 values_to = "sales") -> 
    bikeshop_revenue_long_tbl

bikeshop_revenue_long_tbl


# Analyze

model <- lm(sales ~ ., data = bikeshop_revenue_long_tbl)

model
