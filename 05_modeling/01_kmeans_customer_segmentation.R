# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0. CUSTOMER TRENDS ----
# - GOAL: Mine Customer Purchase History for similarity to other "like" customers
# - TECHNIQUES: K-Means Clustering, UMAP 2D Projection

## 1.1. Get Customer Trends ----

bike_orderlines_tbl %>% 
    select(bikeshop_name, quantity, price, model, category_1, category_2, frame_material) %>% 
    group_by(bikeshop_name, price, model, category_1, category_2, frame_material) %>% 
    summarise(quantity_purchased = sum(quantity)) %>% 
    arrange(model, category_1, category_2) %>% 
    ungroup() %>% 
    group_by(bikeshop_name) %>% 
    mutate(prop_of_total = quantity_purchased/sum(quantity_purchased)) %>% 
    ungroup() ->
    customer_trends_tbl

## 1.2. Convert to User-Item Format (e.g. Customer-Product) ----

customer_trends_tbl %>% 
    select(bikeshop_name, model, prop_of_total) %>% 
    pivot_wider(names_from = model, 
                values_from = prop_of_total, 
                values_fill = 0) ->
    customer_product_tbl

customer_product_tbl

# 2.0 MODELING: K-MEANS CLUSTERING ----

?kmeans

## 2.1. Performing K-Means ----

customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    kmeans(centers = 3, nstart = 100) ->
    kmeans_obj

kmeans_obj$cluster

kmeans_obj$centers

## 2.2. Tidying a K-Means Object ----

kmeans_obj %>% broom::tidy() %>% glimpse() # returns the centres information

kmeans_obj %>% broom::glance() # returns overall summary metrics for the model

kmeans_obj %>% broom::augment(customer_product_tbl) %>% 
    select(bikeshop_name, .cluster)

## 2.3. How many centres (customer groups) to use? ----
 
### 2.3.1. Step 1: Create a Function That Can Be Iterated (Mapped) ----

kmeans_mapper <- function (centers = 3){
    customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        kmeans(centers = centers, nstart = 100)
}

### 2.3.2. Step 2: Test the Function Out on a Single Element (Iteration) ----

3 %>% kmeans_mapper() %>% glance()

### 2.3.3. Step 3: Map the Function to All Elements (Iterate) ----

### Implement purrr row-wise: mutate() + map()

tibble(centers = 1:15) %>% 
    mutate(k_means = centers %>% map(kmeans_mapper)) %>% 
    mutate(glance = k_means %>% map(glance)) ->
    kmeans_mapped_tbl

kmeans_mapped_tbl %>% 
    unnest(glance) %>% 
    select(centers, tot.withinss)

## 2.4. Scree Plot ----

kmeans_mapped_tbl %>% 
    unnest(glance) %>% 
    select(centers, tot.withinss) %>% 
    ggplot(aes(centers, tot.withinss)) +
    geom_point(color = "#2c3e50", size = 4) +
    geom_line(color = "#2c3e50", linewidth = 1) +
    ggrepel::geom_label_repel(aes(label = centers), color = "#2c3e50") +
    theme_tq() +
    labs(
        title = "Scree Plot",
        subtitle = "Measures the distance each of the customer are from the closest K-Means centre",
        caption = "Conclusion: based on the Scree Plot, we will select four clusters to segment the customer base."
    )
    
 
# 3.0 VISUALIZATION: UMAP ----
?umap

## 3.1. Use UMAP to get 2-D Projection ----

customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    umap() ->
    umap_obj

umap_obj$layout %>% as_tibble(.name_repair = "unique") %>% 
    set_names(c("x", "y")) %>% 
    bind_cols(
        customer_product_tbl %>% 
            select(bikeshop_name)
            
    ) %>% 
    relocate(bikeshop_name) ->
    umap_results_tbl

umap_results_tbl

umap_results_tbl %>% 
    ggplot(aes(x, y)) +
    geom_point() +
    geom_label_repel(aes(label = bikeshop_name), size = 3)
    

## 3.2. Use K-Means to Add Cluster Assignments ----

umap_results_tbl

kmeans_4_obj <- kmeans_mapped_tbl %>% 
    pull(k_means) %>% 
    pluck(4) # plucks the forth item - equalt to filter(centers == 4) %>% pull(k_means) %>% pluck(1)

kmeans_4_obj

kmeans_4_obj %>% augment(customer_product_tbl) %>% # augment adds multiple columns to a dataframe
    select(bikeshop_name, .cluster) ->
    kmeans_4_clusters_tbl

umap_results_tbl %>% 
    left_join(kmeans_4_clusters_tbl) ->
    umap_kmeans_4_results_tbl

umap_kmeans_4_results_tbl

## 3.3. Visualize UMAP'ed Projections with Cluster Assignments ----

umap_kmeans_4_results_tbl %>% 
    mutate(label_text = str_glue("Customer: {bikeshop_name}
                                 Cluster: {.cluster}")) %>% 
    ggplot(aes(x, y, color = .cluster)) +
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Customer Segmentation: 2D Projection",
        subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
        caption = "Conclusion: four customer segments identified using two algorithms") +
    theme(legend.position = "none")
    

# 4.0 ANALYZE PURCHASING TRENDS ----

## We need to relate clusters to products within those clusters

customer_trends_tbl %>% 
    pull(price) %>% 
    quantile(probs = c(0, 0.33, 0.67, 1)) # divide into three groups (low, medium, high)

customer_trends_tbl %>% 
    left_join(umap_kmeans_4_results_tbl) %>% 
    # mutate(price_bin = case_when(price <= price |> quantile(0.33) ~ "low", price <= price |> quantile(0.66) ~ "medium", TRUE ~ "high"))
    mutate(price_bin = case_when( #  mutate(price_bin = ntile(price,3)) %>%  mutate(price_bin = recode(price_bin, `1` = "low", `2` = "medium", `3` = "high"))
        price <= 2240 ~ "low",
        price <= 4260 ~ "medium",
        TRUE ~ "high"
    )) %>% 
    # View() # check
    select(.cluster, model, contains("price"), category_1:quantity_purchased) %>% 
    
    # Aggregate quantity purchased by cluster and product attributes
    
    # group_by_at(.vars = vars(.cluster:frame_material)) %>% 
    group_by(across(.cluster:frame_material)) %>% 
    summarise(total_quantity = sum(quantity_purchased)) %>% 
    ungroup() %>% 
    group_by(.cluster) %>% 
    mutate(prop_of_total = total_quantity / sum(total_quantity)) %>% 
    ungroup() ->
    cluster_trends_tbl

cluster_trends_tbl 

get_cluster_trends <- function (cluster = 1){
    cluster_trends_tbl %>% 
        filter(.cluster == cluster) %>% 
        arrange(desc(prop_of_total)) %>% 
        mutate(cum_prop = cumsum(prop_of_total))
}

# Cluster 1 - Low/Medium Price, Road Model Preference
    
get_cluster_trends(1)

cluster_trends_tbl %>% 
    filter(.cluster==1) %>%  
    select(price_bin:frame_material) %>% 
    table()


# Cluster 2 - High-End Price, Road Preference, Carbon Frame

get_cluster_trends(2)

# Cluster 3 - High-End Price, Mountain Preference, Carbon Frame

get_cluster_trends(3)

# Cluster 4 - Low/Medium Price, Mountain Preference, Aluminium Frame

get_cluster_trends(4) %>% View()

cluster_label_tbl <- tibble(
    .cluster = 1:4,
    .cluster_label = c(
        "Low/Medium Price, Road",
        "High-End Price, Road, Carbon Frame",
        "High-End Price, Mountain, Carbon Frame",
        "Low/Medium Price, Mountain, Aluminium Frame"
    )
) %>% 
    mutate(.cluster = as_factor(as.character(.cluster)))


umap_kmeans_4_results_tbl %>% 
    left_join(cluster_label_tbl) %>% 
    mutate(label_text = str_glue("{bikeshop_name} [{.cluster}]")) %>% 
    ggplot(aes(x, y, color = .cluster)) +
    geom_point() +
    geom_label_repel(aes(label = label_text), size = 3) +
    theme_tq() +
    scale_color_tq() +
    scale_color_discrete(name = "Clusters", labels = cluster_label_tbl$.cluster_label) +
    labs(title = "Customer Segmentation: 2D Projection",
         subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
         caption = "Conclusion: four customer segments identified using two algorithms") +
    theme(legend.position = "bottom")
