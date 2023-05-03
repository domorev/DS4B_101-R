# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----


library(tidyverse)
library(readxl)

bikes_tbl           <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")


bikes_tbl
orderlines_tbl
bike_orderlines_tbl %>% glimpse()

# 1.0 Selecting Columns with select() ----

## Basic Select ----

bike_orderlines_tbl %>% 
    select(order_date, order_id, order_line)

bike_orderlines_tbl %>% 
    select(1:3)

bike_orderlines_tbl %>% 
    select(starts_with("order"))

## Reduce Columns ----

bike_orderlines_tbl %>% 
    select(order_date, total_price, category_1, category_2)

## Rearrange Columns ----

bike_orderlines_tbl %>% 
    select(bikeshop_name:state, everything())

bike_orderlines_tbl %>% 
    select(contains("price"))

bike_orderlines_tbl %>% 
    select(ends_with("price"))

## pull() ----

bike_orderlines_tbl %>% 
    pull(total_price) %>% 
    mean()

bike_orderlines_tbl %>% 
    pull(model)

## select_if()

?select_if

bike_orderlines_tbl %>% 
    select_if(is.character)

bike_orderlines_tbl %>% 
    select_if(is.numeric)

bike_orderlines_tbl %>% 
    select_if(~ !is.numeric(.))

bike_orderlines_tbl %>% 
    select_if(~ !is.numeric(.))

# 2.0 Arranging with arrange() and desc() ----


bikes_tbl %>% 
    select(model, price) %>% 
    arrange(desc(price))


# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > mean(price))

bikes_tbl %>% 
    select(model, price) %>% 
    filter((price > 5000) | (price < 1000)) %>% 
    arrange(desc(price))

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > 6000, model %>% str_detect("Supersix"))

bike_orderlines_tbl %>% 
    filter(category_2 %in% c("Over Mountain", "Trail"))

bike_orderlines_tbl %>% 
    filter(category_2 == "Over Mountain")

bike_orderlines_tbl %>% 
    filter(category_2 != "Over Mountain")

bike_orderlines_tbl %>% 
    filter (!(category_2 %in% c("Over Mountain", "Trail", "Endurance Road")))

## 3.2 slice(): filtering with row number(s) ----


bikes_tbl %>% 
    arrange(desc(price)) %>% 
    slice(1:5) # top 5 models

bikes_tbl %>% 
    arrange(price) %>% 
    slice(1:5) # bottom 5 models

bikes_tbl %>% 
    arrange(desc(price)) %>% 
    slice((nrow(.)-4):nrow(.)) # top 5 models

## 3.3 distinct() ----

bike_orderlines_tbl %>% 
    distinct(category_1)

bike_orderlines_tbl %>% 
    distinct(category_1, category_2)

bike_orderlines_tbl %>% 
    distinct(bikeshop_name, city, state)

# 4.0 Adding Columns with mutate() ----

bike_orderlines_tbl %>% 
    select(order_date, model, quantity, price) %>% 
    mutate(total_price = quantity * price) -> bike_orderlines_prices_tbl

bike_orderlines_prices_tbl %>% 
    mutate(total_price_log = log(total_price)) %>% 
    mutate(total_price_sqrt = sqrt(total_price))

## Adding Flag ----

bike_orderlines_prices_tbl %>% 
    mutate(is_supersix = model %>% str_to_lower() %>% str_detect("supersix")) %>% 
    filter(is_supersix)

## Binning with ntile() ----

bike_orderlines_prices_tbl %>% 
    mutate(total_price_bin = ntile(total_price, 3)) # ntile() bins continuous variables into bins

## Binning with case_when() ----

## You can specify the threshold for each group in terms of probability

## Numeric to Categorical

bike_orderlines_prices_tbl %>% 
    mutate(total_price_binned = ntile(total_price, 3)) %>% 
    mutate(total_price_binned_2 = case_when(
        total_price > quantile(total_price, 0.75) ~ "High",
        total_price > quantile(total_price, 0.25) ~ "Medium",
        TRUE ~ "Low" # catch-all condition
    ))

## Text to Categorical

bike_orderlines_prices_tbl %>% 
    mutate(bike_type = case_when(
        model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix",
        model %>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll",
        TRUE ~ "Not Supersix or Jekyll"
    ))

# 5.0 Grouping & Summarizing with group_by() and summarize() ----

bike_orderlines_tbl %>% 
    summarise(
        revenue = scales::dollar(sum(total_price))
    )

bike_orderlines_tbl %>% 
    group_by(category_1) %>% 
    summarise(revenue = scales::dollar(sum(total_price))) %>% 
    ungroup()

bike_orderlines_tbl %>% 
    group_by(category_1, category_2) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup() %>% 
    arrange(desc(revenue)) %>% 
    mutate(revenue = scales::dollar(revenue))

## Summary Functions ----

bike_orderlines_tbl %>% 
    group_by(category_1, category_2) %>% 
    summarise(
        count = n(),
        avg = mean(total_price),
        med = median(total_price),
        sd = sd(total_price),
        min = min(total_price),
        max = max(total_price)
    ) %>% 
    ungroup() %>% 
    arrange(desc(count))

## Detect Missing Values with summarise_all()

bike_orderlines_tbl %>% 
    mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)])) ->  # injects NA values to the first 5 rows
    bikes_orderlines_missing_tbl

bikes_orderlines_missing_tbl

bikes_orderlines_missing_tbl %>% 
    summarise_all(~ sum(is.na(.)))

bikes_orderlines_missing_tbl %>% 
    summarise_all(~ sum(is.na(.)/length(.)))

bikes_orderlines_missing_tbl %>% 
    filter(!is.na(total_price))

# 6.0 Renaming columns with rename() and set_names() ----

# 6.1 rename: One column at a time ----

bike_orderlines_tbl %>% 
    select(bikeshop_name, category_1, total_price) %>% 
    group_by(bikeshop_name, category_1) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    arrange(desc(sales)) -> bikeshop_revenue_tbl

bikeshop_revenue_tbl %>% 
    rename(
        `Bikeshop Name` = bikeshop_name,
        `Primary Category` = category_1,
        Sales = sales
    )

# 6.2 set_names: All columns at once ---

bikeshop_revenue_tbl %>% 
    set_names(c("Bikeshop Name", "Primary Category", "Sales"))

bikeshop_revenue_tbl %>% 
    set_names(names(.) %>% str_replace("_", " ") %>% 
                  str_to_title())

# 7.0 Reshaping (Pivoting) Data with pivot_wider() and pivot_longer() ----


# 7.1 pivot_wider(): Long to Wide ----

bikeshop_revenue_tbl %>% 
    pivot_wider(names_from = category_1, values_from = sales) %>% 
    arrange(desc(Mountain)) %>% 
    rename('Bikeshop Name' = bikeshop_name) %>% 
    mutate (Mountain = scales::dollar(Mountain)) %>% 
    mutate (Road = scales::dollar(Road)) -> 
    bike_shop_revenue_formatted_tbl

bike_shop_revenue_formatted_tbl

# 7.2 pivot_longer(): Wide to Long ----

bike_shop_revenue_formatted_tbl %>% 
    pivot_longer(Mountain:Road, names_to = "category_1", values_to = "sales") %>% 
    mutate(sales = sales %>% str_remove_all("\\$|,") %>% as.double()) %>% 
    arrange(desc(sales))


# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----

orderlines_tbl

bikes_tbl

orderlines_tbl %>% 
    left_join(y = bikes_tbl, by = c("product.id" = "bike.id"))

# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----


## 9.1 bind_cols() ----

bike_orderlines_tbl %>% 
    select(-contains("order")) %>% 
    bind_cols(
        bike_orderlines_tbl %>% select(order_id)
    ) %>% 
    relocate(order_id)

## 9.2 bind_rows() ----

train_tbl <- bike_orderlines_tbl %>% 
    slice(1:(nrow(.)/2))
train_tbl

test_tbl <- bike_orderlines_tbl %>% 
    slice((nrow(.)/2 +1):nrow(.))

test_tbl

dim(train_tbl)
dim(test_tbl)

train_tbl %>% bind_rows(test_tbl)


# 10 Separate and Unite ----

bike_orderlines_tbl %>% 
    select(order_date) %>% 
    mutate(order_date = as.character(order_date)) %>% 
    separate(col = order_date, into = c("year", "month", "day"), sep="-", remove = FALSE) %>% 
    mutate(
        year  = as.numeric(year),
        month = as.numeric(month),
        day   = as.numeric(day)
    ) %>% 
    unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>% 
    mutate(order_date_united = as.Date(order_date_united))

