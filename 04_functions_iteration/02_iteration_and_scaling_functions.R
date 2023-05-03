# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0. PRIMER ON PURRR ----
## 1.1.Programmatically getting Excel files into R ----

excel_paths_tbl <- fs::dir_info("00_data/bike_sales/data_raw/")

paths_chr <- excel_paths_tbl %>% 
    pull(path)

## 1.2. What Not To Do: Don't Use For Loops ----

excel_list <- list()

for (path in paths_chr){
    excel_list[[path]] <- read_excel(path)
}

## 1.3. What to Do: Use map() ----

?map

### 1.3.1. Method 1: Use Function Name ----
excel_list_map <- paths_chr %>% 
    map(read_excel) %>% 
    set_names(paths_chr)

### 1.3.2. Method 2: Use an Anonymous Function ----

# OR use an anonymous function with ~ and .

paths_chr %>% 
    map(~ read_excel(.))


### 1.3.3. Method 3: Specify a Function() ----

paths_chr %>% 
    map(function(x) read_excel(path = x))



## 1.4. Reading Excel Sheets ----

excel_sheets("00_data/bike_sales/data_raw/bikes.xlsx") %>% 
    map(~ read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx", sheet = .))


# 2.0. MAPPING DATA FRAMES ----

## 2.1. Column-wise Map ----

bike_orderlines_tbl %>% is.list()

bike_orderlines_tbl %>% as.list()

bike_orderlines_tbl[[2]]

bike_orderlines_tbl %>% 
    map(~ class(.)[1])

## 2.2. Map Variants ----

### 2.2.1. List Map ----

?map 

### 2.2.2. Character Map ----


bike_orderlines_tbl %>% 
    map_chr(~ class(.)[1]) #[1] because order_date has two classes POSIXct and POSIXt


### 2.2.3. Data-Frame Map ----


bike_orderlines_name_chr_df <- bike_orderlines_tbl %>% 
    map_df(~ class(.)[1]) %>% pivot_longer(cols = everything(),names_to = "column", values_to = "type")

bike_orderlines_name_chr_df 


# You can calculate percentage of missing values:

bike_orderlines_tbl %>% 
    map_df(~ sum(is.na(.)) / length(.)) %>% 
    pivot_longer(cols = everything(),names_to = "column", values_to = "type")


## 2.3. Row-wise Map ----

excel_tbl <- excel_paths_tbl %>% 
    select(path) %>% 
    mutate(data = path %>% map(read_excel))

excel_list

excel_tbl

# 3.0. NESTED DATA ----


## 3.1. Unnest ----

excel_tbl

excel_tbl$data

excel_tbl$data[[1]]

excel_tbl$data[[2]]

excel_tbl$data[[3]]

excel_tbl_unnested <- excel_tbl %>% 
    mutate(id = names(data)) %>% 
    mutate(ID = rownames(.)) %>% 
    unnest(data) 

excel_tbl_unnested

## 3.2. Nest ----

excel_tbl_nested <- excel_tbl_unnested %>% 
    group_by(ID, path) %>% 
    nest()

excel_tbl_nested$data[[1]] %>% glimpse()

## 3.3. Mapping Nested List Columns ----

x <- rep(NA, 5)

x

is.na(x) %>% all()
!is.na(x) %>% all()

y <- c(1:4, NA_real_)

y

is.na(y) %>% all()
!is.na(y) %>% all()

excel_tbl_nested$data[[3]] %>% 
    select_if (~ !is.na(.) %>% all())


### 3.3.1. Method 1: Creating a Function Outside of purrr::map() ----

#### 3.3.1.1. Create a Function That Can Be Mapped to One Element ----

select_non_na_columns <- function(data){
    data %>% 
        select_if (~ !is.na(.) %>% all())
}

#### 3.3.1.2. Extract an Element and Test the Function ----

excel_tbl_nested$data[[3]] %>% 
    select_non_na_columns()


#### 3.3.1.3. Use mutate() + map() ----

excel_tbl_nested_fixed <- excel_tbl_nested %>% 
    mutate(data_fixed = data %>% map(select_non_na_columns))

excel_tbl_nested_fixed$data[[1]]
excel_tbl_nested_fixed$data_fixed[[1]]


# 4.0. MODELING WITH PURRR ----

## 4.1. Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))


## 4.2. Modeling Primer ----

### 4.2.1. Data Preparation ----

sales_by_m_cross_country_tbl <- rolling_avg_3_tbl %>% 
    filter(category_2 == "Cross Country Race") %>% 
    select(month_end, total_price) %>% 
    mutate(month_end_num = as.numeric(month_end))
    
sales_by_m_cross_country_tbl %>% 
    ggplot(aes(month_end_num, total_price)) +
    geom_point() +
    geom_smooth(method = "loess", span = 0.2, se = FALSE)

### 4.2.2. Making a LOESS model ----

?loess

fit_loess_cross_country <- sales_by_m_cross_country_tbl %>% 
    loess(total_price ~ month_end_num, data = ., span = 0.2)

fit_loess_cross_country

### 4.2.3. Working with Broom ----

fit_loess_cross_country %>% 
    broom::augment() %>% 

### 4.2.4. Visualizing results ----
    
ggplot(aes(month_end_num, total_price)) +
    geom_point() +
    geom_line(aes(y = .fitted), color = "blue")

## 4.3. Step 1: Crate a Function to Return Fitted Results ----

rolling_avg_3_tbl_nested <- rolling_avg_3_tbl %>% 
    group_by(category_1, category_2) %>% 
    nest() %>% 
    ungroup()

rolling_avg_3_tbl_nested$data[[1]]

data <- rolling_avg_3_tbl_nested$data[[1]]

tidy_loess <- function(data, span = 0.2){
    
    data_formatted <- data %>% 
        select(month_end, total_price) %>% 
        mutate(month_end_num = as.numeric(month_end))
    
    fit_loess <- loess(formula = total_price ~ month_end_num, 
                       data = data_formatted, 
                       span = span)
    output_tbl <- fit_loess %>% 
        broom::augment() %>% 
        select(.fitted)
    
    return(output_tbl)
    
}

## 4.4. Step 2: Test the Function on Single Element ----

rolling_avg_3_tbl_nested$data[[3]] %>% tidy_loess()

## 4.5. Step 3: Map the Function to All Categories ----

### 4.5.1. Map Functions ----

loess_tbl_nested <- rolling_avg_3_tbl_nested %>% 
    mutate(fitted = data %>% map(tidy_loess))

loess_tbl_nested$fitted[[1]]

loess_tbl_nested %>% 
    unnest()

### 4.5.2. Visualize Results ----

loess_tbl_nested %>% 
    unnest(cols = c(data, fitted)) %>% 
    ggplot(aes(month_end, total_price, color = category_2)) +
    geom_point() +
    geom_line(aes(y = .fitted), color = "blue", linewidth = 1) +
    # geom_smooth (method = "loess", span = 0.2) +
    facet_wrap(~ category_2, scales = "free_y")

