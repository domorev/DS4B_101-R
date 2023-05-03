# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# ADVANCED BUSINESS PLOTS ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0. Lollipop Chart: Top N Customers ----
# - Great for showing order

# Question: How much purchasing power is in top 5 customers?
# Goal: Visualize top N customers in terms of Revenue, include cumulative percentage

n <- 7

## 1.1. Data Manipulation ----

bike_orderlines_tbl %>% 
    select(bikeshop_name, total_price) %>% 
    mutate(bikeshop_name = as_factor(bikeshop_name) %>% fct_lump (n = n, w = total_price)) %>% 
    group_by(bikeshop_name) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup() %>% 
    
    mutate(bikeshop_name = bikeshop_name %>% fct_reorder(revenue)) %>%
    mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after = 0)) %>% 
    arrange(desc(bikeshop_name)) %>% 
    
    mutate(revenue_text = scales::dollar(revenue, scale = 1e-6, suffix = "M")) %>% 
    
    mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>% 
    mutate(cum_pct_text = scales::percent(cum_pct)) %>% 
    
    mutate(rank = row_number()) %>% 
    mutate(rank = case_when(
        rank == max(rank) ~ NA_integer_, # NA values must match the data type. NA_integer_ adds an NA value to integer data
        TRUE ~ rank
    )) %>% 
    mutate(label_text = str_glue("Rank: {rank}\nRevenue: {revenue_text}\nCumulative Percent: {cum_pct_text}")) %>% 
    mutate(bikeshop_name_text = str_glue("{rank}. {bikeshop_name}")) ->
    top_customer_tbl

top_customer_tbl %>% glimpse()
## 1.2. Data Visualization ----

label_title <- str_glue("Top {n} Customers")
label_subtitle <- str_glue("{year(min(bike_orderlines_tbl$order_date))} - {year(max(bike_orderlines_tbl$order_date))}")
caption_text <- str_glue ("Top 6 customers contribute 51% of revenue")

top_customer_tbl %>% 
    ggplot(aes(revenue, bikeshop_name, color = revenue)) +
    geom_segment(
        aes(
            xend  = 0, 
             yend = bikeshop_name
            ),
        color = palette_light()[2],
        linewidth = 1
        ) +
    geom_point(aes(size = revenue),
               color = palette_light()[2],
               ) +
    geom_label(aes(label = label_text),
               hjust = "inward",
               size = 3,
               color = palette_light()[1],
               label.padding = unit(0.5, "lines")) +
    scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    scale_size_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    labs(
        title = label_title,
        subtitle = label_subtitle,
        x = "Revenue ($M)",
        y = "Customer",
        caption = caption_text,
        size = "Revenue"
        ) +
    theme_tq() +
    theme(#legend.position = "none",
          #title = element_text(face = "bold")
          plot.title = element_text(face = "bold.italic")
          )

# 2.0. Heatmaps ----
# - Great for showing details in 3 dimensions

# Question: Do specific customers have a purchasing prefernce?
# Goal: Visualize heatmap of proportion of sales by Secondary Product Category

## 2.1. Data Manipulation ----

bike_orderlines_tbl %>% 
    select(bikeshop_name, category_1, category_2, quantity) %>% 
    group_by(bikeshop_name, category_1, category_2) %>% 
    summarise(total_qty = sum(quantity)) %>% 
    ungroup() %>% 
    group_by(bikeshop_name) %>% 
    mutate(pct = total_qty / sum(total_qty)) %>% 
    ungroup() %>% 
    mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>% # as.factor() preserves alphabetical order
    mutate(bikeshop_name_num = as.numeric(bikeshop_name)) -> 
    pct_sales_by_customer_tbl


## 2.2. Data Visualization ----

pct_sales_by_customer_tbl %>% 
    ggplot(aes(category_2, bikeshop_name)) +
    geom_tile(aes(fill = pct)) +
    geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
              size = 3,
              color = ifelse(pct_sales_by_customer_tbl$pct >= 0.25, "white", "black") ) +
    facet_wrap(~ category_1, scales = "free_x") +
    scale_fill_gradient(low = "white", high=palette_light()[1]) +
    labs(
        title = "Heatmap of Purchasing Habits",
        x = "Bike Type (Category 2)",
        y = "Customer",
        caption = str_glue("Customers that prefer Road:",
                           "Ann Arbor Speed, Austin Cruisers and Indianapolis Velocipedes",
                           "\nCustomers that prefer Mountain: Ithaca Mountain Climbers, Pittsburgh Mountain Machines and Timpa 29ers") 
    ) +
    theme_tq() + 
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold.italic", hjust = 0.5)
    )