# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Date & lubridate Basics ----

## 1.1. Character vs Date/Datetime ----

bike_orderlines_tbl %>% 
    select(order_date) -> order_date_tbl


order_date_tbl %>% 
    pull(order_date) %>% 
    class()

## 1.2. Date Classes ----

order_date_tbl %>% 
    mutate(order_date_chr = as.character(order_date)) %>% 
    mutate(order_date_chr_2 = order_date_chr %>% str_c(" 00:00:00")) %>% 
    mutate(order_date_date = order_date_chr %>% ymd()) %>% 
    mutate(order_date_dttm = order_date_chr_2 %>% ymd_hms())

## 1.3. lubridate Functions ----

### 1.3.1.Conversion ----

"06/01/18" %>% mdy() %>% class()

"06/06/18 12:30:15" %>% mdy_hms() %>% class()

"January 1, 1985" %>% mdy()

### 1.3.2. Extractor ----

"2011-01-01" %>% ymd() %>% year()

"2011-01-01" %>% ymd() %>% month(label = TRUE, abbr = FALSE) 
"2011-01-01" %>% ymd() %>% month(label = FALSE)

"2011-01-01" %>% ymd() %>% wday(label = TRUE, abbr = FALSE)

"2011-01-01" %>% ymd() %>% day()

### 1.3.3. Helpers ----

OlsonNames(tzdir = NULL)

now(tzone = "Australia/Adelaide")
now() %>% class()

today()

today() %>% class()

### 1.3.4. Periods & Durations - Add/Subract Time to/from a Date ----

#### Periods are time spans take into account 
#### daylight savings time and leap years, etc.

#### 1.3.4.1. Periods ----

today() + days(12) # period

#### 1.3.4.2. Durations ----

##Durations are just physical time spans without time irregularities


today() + ddays(12) # duration

today() %m+% years(5) # %m+% accounts for leap years


### 1.3.5. Intervals - Calculate Time-Based Distance ----

#### Intervals are used to convert 2 timestamps to a duration,
#### they capture 2 slices of time as difference that can be converted
#### to a duration (a physical time difference)

i <- interval(today(), today() + ddays(12))

i / ddays(1) # converts the interval to days

i / dminutes(1) # converts the interval to minutes

i_from_the_start_of_the_year <- interval(
    ymd_hms("2022-01-01 00:00:00"),
    now())

scales::comma(i_from_the_start_of_the_year / dseconds(1))

i_to_the_end_of_the_year <- interval(
    now(),
    ymd_hms("2022-12-31 23:59:59")
    )

scales::comma(i_to_the_end_of_the_year / dseconds(1))

#### Always divide an interval by a duration (not period)!

order_date_tbl %>% 
    mutate(today = today()) %>% 
    mutate(diff_days = interval(order_date, today) / ddays(1)) %>% 
    mutate(diff_weeks = difftime(today, order_date, units = "weeks") %>% as.double()) 


# 2.0. Time-Based Data Grouping ----

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(order_date = ymd(order_date)) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() -> bike_sales_y_tbl


bike_sales_y_tbl

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(order_date = ymd(order_date)) %>% 
    mutate( year = year(order_date),
           month = month(order_date, label = TRUE, abbr = TRUE)
           ) %>% 
    group_by(year, month) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() -> bike_sales_m_tbl

bike_sales_m_tbl

## 2.1. Floor Date ----

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(order_date = ymd(order_date)) %>% 
    mutate(year_month = floor_date(order_date, unit = "month")) %>% 
    group_by(year_month) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() -> bike_sales_ym_tbl

bike_sales_ym_tbl

# 3.0 Measuring Change ----

## lag() - shift a time series by n lags - 
## useful for comparing previous values in a vector

bike_sales_y_tbl %>% 
    mutate(sales_lag_1 = lag(sales, n = 1)) %>% 
    mutate(sales_lag_1 = case_when( # dealing with NA values, but it can also be achieved by tidyr::fill(direction = "up")
        is.na(sales_lag_1) ~ sales,
        TRUE ~ sales_lag_1
    )) %>% 
    mutate(diff_1 = sales - sales_lag_1) %>% 
    mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
    mutate(pct_diff_1_chr = scales::percent(pct_diff_1))

### Always divide differences by your reference point.
### The reference point for comparison here is the previous year (lag 1).


## 3.1. Difference from most recent observation ----

calculate_pct_diff <- function(data) {
    data %>% 
        mutate(sales_lag_1 = lag(sales, n = 1)) %>% 
        mutate(sales_lag_1 = case_when( # dealing with NA values, but it can also be achieved by tidyr::fill(direction = "up")
            is.na(sales_lag_1) ~ sales,
            TRUE ~ sales_lag_1
        )) %>% 
        mutate(diff_1 = sales - sales_lag_1) %>% 
        mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
        mutate(pct_diff_1_chr = scales::percent(pct_diff_1, accuracy = 0.1))
}

bike_sales_m_tbl %>% 
    calculate_pct_diff


## 3.2. Difference from first observation ----

bike_sales_y_tbl %>% 
    mutate(
        sales_2011        = first(sales),
        diff_2011         = sales - sales_2011,
        pct_diff_2011     = diff_2011 / sales_2011,
        pct_diff_2011_chr = scales::percent(pct_diff_2011, accuracy = 0.1)
    )

bike_sales_m_tbl %>% 
    group_by(year) %>% # group_by mutates - vector operations on grouped data are applied to each group independently
    mutate(
        sales_jan        = first(sales),
        diff_jan         = sales - sales_jan,
        pct_diff_jan     = diff_jan/ sales_jan,
        pct_diff_jan_chr = scales::percent(pct_diff_jan, accuracy = 0.1)
    ) %>% 
    ungroup()


# 4.0. Cumulative Calculations ----

bike_sales_y_tbl %>% 
    mutate(cum_sales = cumsum(sales),
           cum_sales_avg = cummean(sales))

bike_sales_y_tbl %>% 
    mutate(cum_sales = cumsum(sales)) %>% 
    mutate(cum_sales_pct = cum_sales / sum(sales)) %>% 
    mutate(cum_sales_pct_chr = scales::percent(cum_sales_pct, accuracy = 0.01))

bike_sales_m_tbl %>% 
    group_by(year) %>% 
    mutate(cum_sales = cumsum(sales)) %>% 
    mutate(cum_sales_pct = cum_sales / sum(sales)) %>% 
    mutate(cum_sales_pct_chr = scales::percent(cum_sales_pct, accuracy = 0.01))
    ungroup() #View()


# 5.0. Rolling Calculations ----

## A rolling mean is a simple way to expose the trend in a time series.
## By taking an average of several values, we can reduce the effect of outliers
## enabling us to visualise a trend.
    
## rollmean() by default has na.pad = FALSE, which returns a shorter vector than
## the length of the data frame. To fix, set na.pad = TRUE
    
## Moving averages are meant to detect trends. Difficult to do if you group.
## You don't want to group moving averages.
    
bike_sales_m_tbl %>% 
        mutate(roll_mean_3 = rollmean(sales, 
                                      k = 3, 
                                      align = "right",
                                      fill = NA)
               ) %>% 
        mutate(roll_mean_6 = rollmean(sales,
                                      k = 6,
                                      align = "right",
                                      fill = NA
                                      )
               )

# 6.0. Filtering Date Ranges ---- 

bike_orderlines_tbl %>% 
        mutate(order_date = ymd(order_date)) %>% 
        filter(order_date %>% between(
            left = ymd("2012-01-01"),
            right = ymd("2013-12-31"))) %>% 
        tail()

    bike_orderlines_tbl %>% 
        mutate(order_date = ymd(order_date)) %>% 
        filter(year(order_date) == 2012)
    
    
    bike_orderlines_tbl %>% 
        mutate(order_date = ymd(order_date)) %>% 
        filter(year(order_date) %in% c(2012, 2013)) %>% 
        tail()
    