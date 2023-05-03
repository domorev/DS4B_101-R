# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis ----

# 1.0 Load libraries ----

# Workhorse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)

# 2.0 Importing Files ----

?read_excel()

bikes_tbl <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")

bikeshops_tbl <- read_excel("00_data/bike_sales/data_raw/bikeshops.xlsx")

orderlines_tbl <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")

# 3.0 Examining Data ----

bikes_tbl %>% glimpse()

bikeshops_tbl %>% glimpse()

orderlines_tbl %>% glimpse()

orderlines_tbl

# 4.0 Joining Data ----

bike_orderlines_joined_tbl <- orderlines_tbl %>% 
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----

bike_orderlines_joined_tbl %>% 
    separate(description,
             into = c("category.1", "category.2", "frame.material"),
             sep = " - ",
             remove = TRUE) %>% 
    separate(location,
            into = c("city", "state"),
            sep = ", ",
            remove = FALSE) %>% 
    # price extended = quantity * unit price
    mutate(total.price = price * quantity) %>% 
    select(-'...1', -location) %>% 
    select(-ends_with(".id")) %>% 
    bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% #add the column back
    select(contains("date"), 
           contains("id"), 
           contains("order"),
           quantity, 
           contains("price"), 
           everything()) %>% 
    rename(order_date = order.date) %>% 
    set_names(names(.) %>% str_replace_all("\\.","_")) -> bike_orderlines_wrangled_tbl 
 
bike_orderlines_wrangled_tbl %>% glimpse()


# 6.0 Business Insights ----

# 6.1 Sales by Year ----

# Step 1 - Manipulate

bike_orderlines_wrangled_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    # mutate(sales_text = scales::dollar(sales,
    #                                   prefix = "",
    #                                   suffix = " €",
    #                                   big.mark = ".",
    #                                   decimal.mark = ","))
    mutate(sales_text = scales::dollar(sales)) -> sales_by_year_tbl

sales_by_year_tbl

# library(timetk)
# 
# bike_orderlines_wrangled_tbl %>% 
#     summarise_by_time(.date_var = order_date,
#                       .by = "quarter",
#                       sales = sum(total_price)) 

# Step 2 - Visualize

sales_by_year_tbl %>% 
    ggplot(aes(x = year, y = sales)) +
    geom_col(fill="#2C3E50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(title = "Revenue by Year",
         subtitle = "Upwards Trend",
         x = "",
         y = "Revenue"
         )
# scales::show_col(palette_light())

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

bike_orderlines_wrangled_tbl %>% 
    select(order_date, total_price, category_2) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year, category_2) %>% 
    summarise(sales = sum(total_price)) %>% 
    ungroup() %>% 
    mutate(sales_text = scales::dollar(sales)) -> sales_by_year_cat_2_tbl

sales_by_year_cat_2_tbl

# Step 2 - Visualize

sales_by_year_cat_2_tbl %>% 
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    geom_col() +
    geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
    facet_wrap(~category_2, ncol = 3, scales = "free_y") + #free_y shows trend, no scales shows magnitude
    theme_tq() +
    scale_fill_tq() + # applies the fill from theme_tq
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Each product category has an upward trend",
        x = "",
        y = "Revenue",
        fill = "Product Secondary Category"
    )

# 7.0 Writing Files ----

fs::dir_create("00_data/bike_sales/data_wrangled/ad") 
file_name <- "00_data/bike_sales/data_wrangled/ad/bike_orderlines"

# 7.1 Excel ----

bike_orderlines_wrangled_tbl %>% 
    write_xlsx(paste0(file_name, ".xlsx"))

# 7.2 CSV ----

bike_orderlines_wrangled_tbl %>% 
    write_csv(paste0(file_name, ".csv"))

# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>% 
    write_rds(paste0(file_name, ".rds"))


