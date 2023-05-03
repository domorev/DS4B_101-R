# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

file_name_prefix <- "_images/02_ggplot2_geometries_-_"

# 1.0. Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

## 1.1. Data Manipulation ----

bike_orderlines_tbl %>% 
    select(order_id, order_line, total_price, quantity) %>% 
    group_by(order_id) %>% 
    summarise(total_quantity = sum(quantity),
              total_price    = sum(total_price)) %>% 
    ungroup() -> 
    order_value_tbl

## 1.2. Scatter Plot ----

order_value_tbl %>% 
    ggplot(aes(x = total_quantity, y = total_price)) +
    geom_point(alpha = 0.5, size = 2) +
    geom_smooth(method = "lm", formula = 'y ~ x',se = FALSE) -> 
    plot_scatter

plot_scatter
ggsave(str_glue(file_name_prefix, "1.2_-_scatter_plot.png"))

# 2.0. Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

## 2.1. Data Manipulation  ----

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(year_month = floor_date(order_date, "month") %>% ymd()) %>% 
    group_by(year_month) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup() -> 
    revenue_by_month_tbl

## 2.2. Line Plot ----

revenue_by_month_tbl %>% 
    ggplot(aes(year_month, revenue)) +
    geom_line(linewidth = 0.5, linetype = 1) +
    geom_smooth(method = "loess", span = 0.2, se = TRUE) ->
    plot_line

plot_line
ggsave(str_glue(file_name_prefix, "2.2_-_line_plot.png"))

# 3.0. Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

## 3.1. Data Manipulation ----

bike_orderlines_tbl %>% 
    select(category_2, total_price) %>% 
    group_by(category_2) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup() %>% 
    mutate(category_2 = category_2 %>% 
               as_factor() %>% 
               fct_reorder(revenue)) -> 
    revenue_by_category_2_tbl

## 3.2. Bar Plot ----

revenue_by_category_2_tbl %>% 
    ggplot(aes(category_2, revenue, fill = revenue)) +
    geom_col(show.legend = FALSE) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
    labs(title = "Revenue by Category", x = "Bike Category", y = "Revenue") +
    coord_flip() +
    scale_fill_gradient(low="#2c3e70", high = "#2c3e50") +
    theme_tq() ->
    plot_bar

plot_bar
ggsave(str_glue(file_name_prefix, "3.2_-_bar_plot.png"))
    
# 4.0. Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable

# Goal: Unit price of bicycles
## 4.1. Histogram  ----

bike_orderlines_tbl %>% 
    distinct(model, price) %>% 
    ggplot(aes(price)) +
    geom_histogram(bins = 25, colour = "#2c3eaa", fill = "#2c3e50") +
    scale_x_continuous(labels = scales::dollar_format(scale = 1/1e3, suffix = "k")) +
    labs(title = "Distribution of Pices") +
    theme_tq() -> 
    plot_histogram

plot_histogram
ggsave(str_glue(file_name_prefix, "4.1_-_histogram.png"))

# Goal: Unit price of bicyclce, segmenting by frame material
## 4.2. Histogram ----

bike_orderlines_tbl %>% 
    distinct(model, price, frame_material) %>% 
    ggplot(aes(price, fill = frame_material)) +
    geom_histogram() +
    facet_wrap(~ frame_material, ncol = 1) +
    scale_fill_tq() +
    theme_tq() -> plot_histogram_segmented

plot_histogram_segmented
ggsave(str_glue(file_name_prefix, "4.2_-_segmented_histogram.png"))
    
## 4.3. Density ----

bike_orderlines_tbl %>% 
    distinct(model, price, frame_material) %>% 
    ggplot(aes(price, fill = frame_material)) +
    geom_density(alpha = 0.5) +
    scale_fill_tq() +
    theme(legend.position = "bottom") ->
    plot_density

plot_density
ggsave(str_glue(file_name_prefix, "4.3_-_density_plot.png"))

# 5.0. Box Plot / Violin Plot ----

# - Great for comparing distributions
# Goal: Unit price of models, segmenting by category 2

## 5.1. Data Manipulation ----

bike_orderlines_tbl %>% 
    select(category_2, model, price) %>% 
    distinct() %>% 
    # distinct(category_2, model, price) %>% 
    mutate(category_2 = category_2 %>% 
               fct_reorder(price)) ->
    unit_price_by_cat_2_tbl
    
## 5.2. Box Plot ----

unit_price_by_cat_2_tbl %>% 
    ggplot(aes(category_2, price)) +
    geom_boxplot() +
    coord_flip() +
    theme_tq() ->
    plot_box

plot_box
ggsave(str_glue(file_name_prefix, "5.2_-_box_plot.png"))

## 5.3. Violin Plot & Jitter Plot ----

unit_price_by_cat_2_tbl %>% 
    ggplot(aes(category_2, price)) +
    geom_boxplot() +
    geom_point(size = 3, colour = "#2c3e50", shape = 18, alpha = 0.75) +
    # geom_jitter(width = 0.15, colour = "#2c3e50") +
    geom_violin(fill = "#2c3e50", alpha = 0.5) +
    coord_flip() +
    theme_tq() -> plot_violin

plot_violin
ggsave(str_glue(file_name_prefix, "5.3_-_violin_plot.png"))
# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

## 6.1. Data Manipulation ----

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup() ->
    revenue_by_year_tbl

## 6.2. Adding text to bar chart ----

revenue_by_year_tbl %>% 
    ggplot(aes(year, revenue)) + 
    geom_col(fill = "#2c3e50") +
    geom_smooth(method = "lm", se = FALSE) +
    geom_text(aes(label = scales::dollar(revenue, scale = 1/1e6, suffix = "M")), 
              vjust = 1.5, 
              colour = "white") +
    geom_label(label = "Demand Above Expectation", 
               vjust = -0.5,
               size = 5,
               fontface = "bold",
               fill = "#1F78B4", # palette_light()
               colour = "white",
               label.r = unit(0.5, "lines"),
               label.padding = unit(0.5, "lines"),
               data = revenue_by_year_tbl %>% 
                   filter(year %in% c(2013))) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                      suffix = "M")) +
    expand_limits(y = 2e7) +
    theme_tq() -> plot_bar_with_text

plot_bar_with_text 
ggsave(str_glue(file_name_prefix, "6.2_-_bar_plot_with_text.png"))


## 6.3. Filtering labels to highlight a point ----

