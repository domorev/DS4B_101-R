# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# FORMATTING GGPLOTS ----

# Libraries & Data ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

file_name_prefix <- "_images/02_ggplot2_geometries_-_"
# plot_scatter
# ggsave(str_glue(file_name_prefix, "1.2_-_scatter_plot.png"))

# Data Manipulation

sales_by_year_category_2_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_2, total_price) %>%
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    group_by(category_2, year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    mutate(category_2 = fct_reorder2(category_2, year, revenue))

sales_by_year_category_2_tbl %>% 
    mutate(category_2_num = as.numeric(category_2)) %>% 
    arrange(category_2_num)



# 1.0. Working with Colours ----

## 1.1. Colour Conversion ----

### 1.1.1. Named Colours ----

colours()

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue)) +
#   geom_col(fill = "slateblue") 
#   geom_col(fill = RColorBrewer::brewer.pal(n = 8, name = "Blues")[8])
    geom_col(fill = viridisLite::viridis(n = 100)[10])

### 1.1.2. To RGB ----

col2rgb("slateblue")
col2rgb("#2c3e50")

### 1.1.3. To HEX ----
 
rgb(44,62,80, maxColorValue = 255)

## 1.2 Colour Palettes ----

### 1.2.3. tidyquant ----

tidyquant::palette_light()

palette_light()[1] %>% col2rgb()

tidyquant::palette_light() %>% scales::show_col()

### 1.2.4. Brewer ----

### Designed primarily for discrete data

RColorBrewer::display.brewer.all()

RColorBrewer::brewer.pal.info # divergent, qualitative, sequential

RColorBrewer::brewer.pal(n = 9, name = "Blues")

RColorBrewer::brewer.pal(n = 8, name = "Blues")[1] %>% col2rgb()

RColorBrewer::brewer.pal(n = 8, name = "Blues")

RColorBrewer::brewer.pal(n = 8, name = "Blues") %>% scales::show_col()

### 1.2.5. Viridis ----

# Designed for continuous data

viridisLite::viridis(n = 20) # Trailing FF is transparency value on the hex codes

viridisLite::viridis(n = 30) %>% scales::show_col()
viridisLite::plasma(n = 30) %>% scales::show_col()
viridis::inferno(n = 30)%>% scales::show_col()

# 2.0. Aesthetic Mappings ----

## 2.1. Colour  -----

### 2.1.2. Used with line and points, outlines of rectangular objects ----

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue)) +
    geom_line(aes(colour = category_2), linewidth = 2) +
    geom_point(colour = "dodgerblue", size = 4)

### 2.1.2. Using colours as aesthetics ----

# wrap colour in aes() if you are mapping it to a particular column

# 2.2 Fill  -----
# - Used with fill of rectangular objects 

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue)) +
    geom_col(aes(fill = category_2))

# 2.3 Size ----
# - Used with points

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue)) +
    geom_line(aes(colour = category_2), linewidth = 1) +
    geom_point(aes(size = revenue))



# 3.0 Faceting ----
# - Great way to tease out variation by category
# Goal: Sales annual sales by category 2

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, colour = category_2)) +
    geom_line(color = "black") +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") +
    expand_limits(y = 0)

# 4.0 Position Adjustments (Stack & Dodge) ----

# Stacked Bars & Side-By-Side Bars

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, fill = category_2)) +
    #   geom_col(position = "stack") # by default
    #   geom_col(position = "dodge") 
    geom_col(position = position_dodge(width = 0.9), colour = "white") 

# Stacked Area

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, fill = category_2)) +
    geom_area(colour = "black")

# 5.0 Scales (Colours, Fills, Axis) ----

# Scales enable customising aesthetics

## 5.1. Plot Starting Points ----

# - Continuous (e.g. Revenue): Changes colour via gradient palette
# - Categorical (e.g. ): Changes colour via discrete palette

### 5.1.1. Plot 1: Faceted Plot, Colour = Continuous Scale ----

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, colour = revenue)) +
    geom_line(aes(colour = revenue), linewidth = 0.5) +
    geom_point(size = 2) +
    facet_wrap(~category_2, scales = "free_y") +
    expand_limits(y = 0) +
    theme_minimal() ->
    g_facet_continuous

g_facet_continuous    

### 5.1.2. Plot 2: Faceted Plot, Colour = Discrete Scale ----

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, colour = category_2)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 2) +
    facet_wrap(~category_2, scales = "free_y") +
    expand_limits(y = 0) +
    theme_minimal() ->
    g_facet_discrete

g_facet_discrete

### 5.1.3. Plot 3: Stacked Area Plot ----

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, fill = category_2)) +
    geom_area(colour = "black") +
    theme_minimal() ->
    g_area_discrete

g_area_discrete

## 5.2. Scale Colours & Fills ----
# - Awesome way to show variation by groups (discrete) and by values (continuous)

### 5.2.1. Colour by Revenue (Continuous Scale) ----

g_facet_continuous +
    # scale_color_continuous(
    #     low  = "cornflowerblue", 
    #     high = "black"
    # )
    scale_color_viridis_c(direction = -1, option = "A", begin = 0.2, end = 0.7)

### 5.2.2. Colour by Category 2 (Discrete Scale) ----

RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info # divergent, qualitative, sequential
RColorBrewer::brewer.pal(n = 9, name = "Blues")

g_facet_discrete +
    scale_color_brewer(palette = "Set3") +
    theme_dark()

g_facet_discrete +
    scale_color_tq(theme = "dark") +
    theme_dark()

g_facet_discrete +
    scale_color_viridis_d(option = "C") +
    theme_dark()

### 5.2.3. Fill by Category 2 ----

 
g_area_discrete +
    scale_fill_brewer(palette = "Set3")

g_area_discrete +
    scale_fill_tq()

g_area_discrete +
    scale_fill_viridis_d()

## 5.3 Axis Scales ----

g_facet_continuous +
    scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))

# 6.0 Labels ----

g_facet_continuous +
    geom_smooth(aes(colour = revenue), method = "lm", se = FALSE) +
    scale_x_continuous(breaks = seq(2011, 2015, by = 2)) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    scale_colour_viridis_c(label = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    theme_dark() +
    
    labs(
        title    = "Bike Sales",
        subtitle = "Sales are trending up",
        caption  = "5-year sales trends\ncomes from our ERP database",
        x        = "Year",
        y        = "Revenue ($M)",
        colour   = "Revenue"
    ) +
    
    theme(
        plot.title    = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0.5)
    )


# 7.0 Themes  ----

g_facet_continuous +
    theme_light() +
    theme(
        axis.text.x = element_text(
            angle   = 45,
            hjust   = 1 # aligns with the tick
        ),
        strip.background = element_rect(
            colour    = "black",
            fill      = "cornflowerblue",
            linewidth = 1
        ),
        strip.text = element_text(
            face   = "bold",
            colour = "white"
        )
    )


# 8.0 Putting It All Together ----

RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info # divergent, qualitative, sequential
RColorBrewer::brewer.pal(n = 9, name = "Blues")

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, fill = category_2)) +
    
    # Geometries 
    geom_area(color = "black") +
    
    # Scales
    scale_fill_brewer(palette = "Blues", direction = -1) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = " M")) +
    
    # Labels
    labs (
        title = "Sales Over Year by Category 2",
        subtitle = "Sales Tredning Upward",
        x = "",
        y = "Revenue ($M)",
        fill = "Second Category",
        caption = "Bike sales trends look strong heading into 2016"
    ) +
    
    # Theme
    theme_light() +
    theme(
        title = element_text(face = "bold", colour = RColorBrewer::brewer.pal(n = 9, name = "Blues")[9])
    )

