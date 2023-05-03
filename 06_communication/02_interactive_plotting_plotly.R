# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# INTERACTIVE PLOTS ----

# GOAL: DEVELOP INTERACTIVE PLOTS FOR A SALES REPORT


# LIBRARIES & DATA ----

# Main
library(tidyverse)
library(lubridate)

# Visualization
library(tidyquant)
library(plotly)


bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

# 1.0 TOTAL SALES BY MONTH ----

## 1.1 Preparing Time Series Data ----

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(date_rounded = floor_date(order_date, unit = "month")) %>% 
    group_by(date_rounded) %>% 
    summarise(total_sales = sum(total_price)) %>% 
    ungroup() %>% 
    mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}")) ->
    total_sales_m_tbl

total_sales_m_tbl

## floor_date rounds down, ceiling_date rounds up

# Formatting Dates
# - strftime: https://devhints.io/strftime
# Sys.setlocale("LC_TIME", "English") # switches the session to English

?format.POSIXct

"2011-01-01 00:00:00" %>% as_datetime() %>% format("%B %Y")

## 1.2 Interactive Plot ----

# Step 1: Create ggplot with text feature

total_sales_m_tbl %>% 
    ggplot(aes(x = date_rounded, y = total_sales)) +
    geom_point(aes(text = label_text), color = "#2c3e50") +
    geom_smooth(method = "loess", span = 0.2) +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    expand_limits(y = 0) +
    labs(title = "Total Sales",
         y = "Revenue (USD",
         x = "") -> g1

g1

# Step 2: Use ggplotly()

ggplotly(g1, tooltip = "text")


## 1.3 Plot Total Sales Function ----

plot_total_sales <- function (unit = "month", 
                              date_format = "%B %Y", 
                              interactive = TRUE){
    
    # Handle Data
    
    bike_orderlines_tbl %>% 
        select(order_date, total_price) %>% 
        mutate(date_rounded = floor_date(order_date, unit = unit)) %>% 
        group_by(date_rounded) %>% 
        summarise(total_sales = sum(total_price)) %>% 
        ungroup() %>% 
        mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}")) ->
        data_tbl
    
    # Make Plot
    
    data_tbl %>% 
        ggplot(aes(x = date_rounded, y = total_sales)) +
        geom_point(aes(text = label_text), color = "#2c3e50") +
        geom_smooth(method = "loess", span = 0.2) +
        theme_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        expand_limits(y = 0) +
        labs(title = "Total Sales",
             y = "Revenue (USD",
             x = "") -> g1
    
    # Static vs Interactive Logic
    
    if (interactive) {
        return(ggplotly(g1, tooltip = "text"))
    } else {
        return(g1)
    }
    
}

## 1.4 Test Our Function ----

plot_total_sales(unit = "month", date_format = "%B %Y", interactive = TRUE)

plot_total_sales(unit = "week", date_format = "%B %d, %Y")

plot_total_sales(unit = "day", date_format = "%B %d, %Y")

# 2.0 CATEGORY 2 SALES BY MONTH ----


## 2.1 Preparing Time Series Data ----

bike_orderlines_tbl %>% 
    select(order_date, category_1, category_2, total_price) %>% 
    mutate(date_rounded = floor_date(order_date, unit = "month")) %>% 
    group_by(date_rounded, category_1, category_2) %>% 
    summarise(total_sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}")) %>% 
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder2(date_rounded, total_sales)) -> 
    
    category_2_sales_m_tbl

## 2.2 Interactive Plot ----

### 2.2.1. Step 1: Create ggplot ----

category_2_sales_m_tbl %>% 
    ggplot(aes(x = date_rounded, y = total_sales, color = category_2)) +
    
    # Geoms
    geom_point(aes(text = label_text),color = "#2c3e50") +
    geom_smooth(method = 'loess', span = 0.2) +
    facet_wrap(~ category_2, scales = "free_y", ncol = 3) + 
    
    # Formatting
    expand_limits(y = 0) +
    theme_tq() +
    theme(legend.position = 'none',
          strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = 'pt'))) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = 'k')) +
    scale_color_tq() +
    labs(
        title = "Sales by Category 2",
        y = '',
        x = ''
    ) -> g2


### 2.2.2. Step 2: Use ggplotly() ----

ggplotly(g2, tooltip = 'text') # tooltip is linked to text defined in geom_point

## 2.3 Plot Categories Function ----

plot_categories <- function(category_1 = 'All', 
                            category_2 = 'All',
                            unit = 'month',
                            date_format = '%B %Y',
                            ncol = 1,
                            scales = 'free_y',
                            interactive = TRUE) {
    
    # Handle Data
    
    data_tbl <- bike_orderlines_tbl %>% 
        select(order_date, category_1, category_2, total_price) %>% 
        mutate(date_rounded = floor_date(order_date, unit = unit)) %>% 
        group_by(date_rounded, category_1, category_2) %>% 
        summarise(total_sales = sum(total_price)) %>% 
        ungroup() %>% 
        
        mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}")) %>% 
        mutate(category_2 = as_factor(category_2) %>% 
                   fct_reorder2(date_rounded, total_sales))
    
    # Handle Inputs
    
    cat_1_text <- str_to_lower(category_1)
    cat_2_text <- str_to_lower(category_2)
    
    # Create Filter Logic
    
    if (cat_1_text != "all") {
        data_tbl %>% 
            filter(category_1 %>% 
                       str_to_lower() %>% 
                       str_detect(pattern = cat_1_text)) ->
            data_tbl
    }
    if (cat_2_text != "all") {
        data_tbl %>% 
            filter(category_2 %>% 
                       str_to_lower() %>% 
                       str_detect(pattern = cat_2_text)) ->
            data_tbl
    }
    
    # Make Plot
    
    data_tbl %>% ggplot(aes(x = date_rounded, y = total_sales, color = category_2)) +
        
        # Geoms
        geom_point(aes(text = label_text),color = "#2c3e50") +
        geom_smooth(method = 'loess', span = 0.2) +
        facet_wrap(~ category_2, scales = scales, ncol = ncol) + 
        
        # Formatting
        expand_limits(y = 0) +
        theme_tq() +
        theme(legend.position = 'none',
              strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = 'pt'))) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = 'k')) +
        scale_color_tq() +
        labs(
            title = "Sales by Category 2",
            y = '',
            x = ''
        ) -> g2
    
    
    # Static vs Interactive Logic
    
    if (interactive) {
        return(ggplotly(g2, tooltip = "text"))
    } else {
        return(g2)
    }
    
}

plot_categories(category_1 = 'all', 
                category_2 = '(cross|elite|trail)',
                ncol = 2,
                scales = 'free_y',
                unit = 'month',
                date_format = '%d %B %Y')

## 2.4 Test Our Function ----

plot_categories(category_1 = 'all', 
                category_2 = '(cross|elite|trail)',
                ncol = 1,
                scales = 'free_y',
                unit = 'quarter',
                date_format = '%d %B %Y')


# 3.0 SAVE FUNCTIONS ----

file_name = '00_scripts/plot_sales.R'

fs::file_create(file_name)

dump(list = c('plot_total_sales', 'plot_categories'), file_name)




