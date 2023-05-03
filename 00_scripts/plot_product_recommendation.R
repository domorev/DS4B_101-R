get_bike_features <-
function() {
    
    bike_orderlines_tbl %>% 
        select(price, model, category_1, category_2, frame_material) %>% 
        distinct() %>% 
        mutate(id = row_number()) %>% 
        select(id, everything()) %>% 
        separate_bike_model(keep_model_column = TRUE, append = TRUE) ->
        
        bike_features_tbl
    
    return(bike_features_tbl)
}
plot_bike_features <-
function(interactive = TRUE) {
    
    # DATA MANIPULATION
    
    bike_features_tbl <- get_bike_features()
    
    # VISUALIZATION
    
    bike_features_tbl %>% 
        
        mutate(category_2 %>%  fct_reorder(price, .fun = median)) %>% 
        mutate(label_text = str_glue("Model: {model}
                                     Price: {scales::dollar(price)}")) %>% 
        ggplot(aes(x = category_2, y = price)) +
        geom_violin() +
        geom_jitter(aes(text = label_text),
                    width = 0.1, 
                    color = "#2c3e50", 
                    alpha = 0.5) +
        facet_wrap (~ frame_material) +
        coord_flip() + 
        theme_tq() +
        theme(strip.text = element_text(margin = margin(5, 5, 5, 5, 
                                                        unit = "pt"))) +
        labs(title = "Product Gap Analysis",
             x = "",
             y = "") +
        scale_y_continuous(labels = scales::dollar_format()) ->
        
        g
    
    # INTERACTIVE VS STATIC
    
    if (interactive) {
        return(ggplotly(g, tooltip = "text"))
        
    } else {
        return(g)    
    }
    
    
    
}
