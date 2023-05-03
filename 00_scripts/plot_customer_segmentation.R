get_customer_segments <-
function(k = 4, seed = 123) {
    
    # 1.0 CUSTOMER TRENDS
    
    bike_orderlines_tbl %>% 
        select(bikeshop_name, price, model, category_1, category_2, 
               frame_material, quantity)  %>% 
        group_by_at(.vars = vars(bikeshop_name:frame_material)) %>% 
        summarise(total_qty = sum(quantity)) %>% 
        ungroup() %>% 
        
        group_by(bikeshop_name) %>% 
        mutate(pct = total_qty / sum(total_qty)) %>% # calculates proportion of sales
        ungroup() ->
        
        customer_trends_tbl
    
    customer_trends_tbl %>% 
        select(bikeshop_name, model, pct) %>% 
        pivot_wider(names_from = model, values_from = pct, values_fill = 0) ->
        
        customer_product_tbl
    
    # 2.0 MODELING: K-MEANS CLUSTERING
    
    set.seed(seed)
    customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        kmeans(centers = k, nstart = 100) -> kmeans_obj
    
    
    kmeans_obj %>% 
        broom::augment(customer_product_tbl) %>% 
        select(bikeshop_name, .cluster) ->
        
        kmeans_tbl
    
    # 3.0 UMAP
    
    # umap.defaults %>% View()
    
    umap.defaults -> umap_config
    
    umap_config$random_state <- seed
    
    customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        as.matrix() %>% 
        umap(config = umap_config) %>% 
        pluck("layout") %>% 
        as_tibble() %>% 
        set_names(c("x","y")) %>% 
        bind_cols(
            customer_product_tbl %>% select(bikeshop_name)
        ) ->
        umap_tbl
     
    # 4.0 COMBINE UMAP & K-MEANS
    
    umap_tbl %>% 
        left_join(kmeans_tbl, by = "bikeshop_name") %>% 
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     Cluster: {.cluster}"))
}
plot_customer_segments <-
function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION
    
    get_customer_segments(k = k, seed = seed) -> combined_tbl
    
    # VISUALIZATION
    
    combined_tbl %>% 
        
        ggplot(aes(x, y, color = .cluster)) +
        
        # Geoms
        
        geom_point(aes(text = label_text)) + 
        
        # Formatting
        
        theme_tq() +
        scale_color_tq() +
        labs(
            title = "Customer Segmentation: 2D Projection",
            subtitle = "UMAP 2D Projection with K-Means Cluster Assignment"
        ) +
        theme(legend.position = 'none') ->
        
        g
     
    # INTERACTIVE VS STATIC
    
    if (interactive) (
        ggplotly(g, tooltip = 'text')
    ) else {
        g + geom_label_repel(aes(label = label_text), size = 2)
    }
    
}
plot_customer_heatmap <-
function(interactive = TRUE) {
    
    # DATA MANIPULATION
    
    bike_orderlines_tbl %>% 
        
        select(bikeshop_name, category_1, category_2, quantity) %>% 
        group_by(bikeshop_name, category_1, category_2) %>% 
        summarise(total_qty = sum(quantity)) %>% 
        ungroup() %>% 
        
        group_by(bikeshop_name) %>% 
        mutate(pct = total_qty / sum(total_qty)) %>% 
        ungroup() %>% 
        
        mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>% 
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     Category: {category_1}
                                     Sub-Category: {category_2}
                                     Qty Purchased: {total_qty}
                                     Percent of Sales: {scales::percent(pct, 0.1)}")) ->
        pct_sales_by_customer_tbl
    
    # VISUALIZATION
    
    pct_sales_by_customer_tbl %>% 
        ggplot(aes(category_2, bikeshop_name)) +
        
        # Geoms
        
        geom_tile(aes(fill = pct)) +
        geom_text(aes(label = scales::percent(pct, 0.1),  text = label_text),
                  size = 3) +
        facet_wrap(~ category_1, scales = "free_x") + # removes missing columns
        
        # Formatting
        
        scale_fill_gradient(low = "white", high = "#2c3e50") +
        theme_tq() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = 'none',
            plot.title = element_text(face = 'bold'),
            strip.text.x = element_text(margin = margin(5,5,5,5, unit = 'pt'))
        ) +
        labs(title = 'Heatmap of Purchasing Habits') -> 
        
        g
        
    # INTERACTIVE VS STATIC
    if (interactive) {
        g <- g +
            labs(x = "", y = "")
            
        return(ggplotly(g, tooltip = 'text'))
    } else {
        g <- g +
            labs(x = "Bike Type (Category 2)", y = "Customer")
        return(g)
    }
    
}
plot_customer_behavior_by_cluster <-
function(top_n_products = 10, 
                                              k = 4, seed = 123, 
                                              interactive = TRUE) {
    
    # DATA MANIPULATION
    
    get_customer_segments(k = k, seed = seed) ->
        
        combined_tbl
    
    bike_orderlines_tbl %>% 
        select(bikeshop_name, model, category_1, category_2, price, quantity) %>% 
        
        group_by_at(.vars = vars(bikeshop_name:price)) %>% 
        summarise(total_qty = sum(quantity)) %>% 
        ungroup() %>% 
        
        group_by(bikeshop_name) %>% 
        arrange(desc(total_qty), .by_group = TRUE) %>% 
        slice(1:top_n_products) %>% 
        ungroup() %>% 
        
        left_join(
            combined_tbl %>% select(bikeshop_name, .cluster), by = "bikeshop_name"
        ) %>% 
        
        mutate(label_text = str_glue("Bike Shop: {bikeshop_name}
                                     Model: {model}
                                     Category 1: {category_1}
                                     Category 2: {category_2}
                                     Price: {scales::dollar(price)}")) ->
        
        top_n_tbl
        
    
    
    # VISUALIZATION
    
    top_n_tbl %>% 
        
        ggplot(aes(category_1, price, color = .cluster)) +
        
        # Geoms
        geom_violin() +
        geom_jitter(width = 0.2, alpha = 0.5, aes(text = label_text)) +
        facet_wrap(~ .cluster, ncol = 2) +
        
        # Formatting
        theme_tq() +
        theme(strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = 'pt'))) +
        scale_color_tq() +
        scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
        labs(
            title = str_glue("Top {top_n_products} Bike Models by Cluster"),
            x = "Category 1",
            y = "Unit Price (Log Scale)"
        ) ->
        g
        
    g
    
    # INTERACTIVE VS STATIC
    
    if (interactive) {
        ggplotly(g, tooltip = "text")
    } else {
        return(g)
    }
}
