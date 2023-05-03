# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# REGRESSION MODELS ----

# GOAL: BUILD PREDICTION MODEL FOR PRICING ALGORITHM


# LIBRARIES & DATA ----

pkgs <- c("parsnip", "glmnet", "rpart", "rpart.plot", "ranger", "randomForest", "xgboost", "kernlab")
# If any of these packages are not installed, run this: install.packages(pkgs)

# Standard
library(readxl)
library(tidyverse)
library(tidyquant)
library(tidymodels)
library(vip)

# # Modelling
# library(parsnip)
# 
# # Pre-processing & Sampling
# library(recipes)
# library(rsample)
# 
# # Modelling Error Metrics
# library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

# Source Scripts
source("00_scripts/separate_bikes_and_outlier_detection.R")

# Read Data
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PROBLEM DEFINITION ----
# - Which Bike Categories are in high demand?
# - Which Bike Categories are under represented?
# - GOAL: Use a pricing algorithm to determine a new product price in a category gap

model_sales_tbl <- bike_orderlines_tbl %>%
    select(total_price, model, category_2, frame_material) %>%
    
    group_by(model, category_2, frame_material) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    arrange(desc(total_sales))

model_sales_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(total_sales, .fun = max) %>% 
               fct_rev()) %>%
    
    ggplot(aes(frame_material, total_sales)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    #coord_flip() +
    facet_wrap(~ category_2) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
    theme_tq() +
    labs(
        title = "Total Sales for Each Model",
        x = "Frame Material", y = "Revenue"
    )


# 2.0 TRAINING & TEST SETS ----

bike_orderlines_tbl %>% 
    select(price, model, category_2, frame_material) %>% 
    distinct() %>% 
    mutate(id = row_number()) %>% 
    relocate(id) %>% 
    separate_bike_model(keep_model_column = TRUE, append = TRUE) ->
    bike_features_tbl

bike_features_tbl

set.seed(1113)
rsample::initial_split(bike_features_tbl, prop = 0.8, strata = "model_base") ->
    split_obj

split_obj %>% training() %>% distinct(model_base)


split_obj %>% testing() %>% distinct(model_base)

train_tbl  <-  training(split_obj)
test_tbl <- testing(split_obj)

# *** FIX 1 *** ----
# Error: factor model_base has new levels Fat CAAD2
# - Need to move Fat CAAD2 from test to training set because model doesn't know how to handle
#   a new category that is unseen in the training data

train_tbl <- train_tbl %>%
    bind_rows(
        test_tbl %>% filter(model_base %>% str_detect("Fat CAAD2"))
    )

test_tbl <- test_tbl %>%
    filter(!model_base %>% str_detect("Fat CAAD2"))

train_tbl %>% distinct(model_base)
test_tbl %>% distinct(model_base)

model_formula <- price ~ .

train_preproc_tbl <- train_tbl %>%
    select(-id, -model, -model_tier)

train_preproc_tbl

test_preproc_tbl <- test_tbl %>% 
    select(-id, -model, -model_tier)

model_recipe <- recipe(model_formula, train_preproc_tbl) %>%
    step_normalize(all_numeric_predictors()) %>%
    # step_corr(all_predictors()) %>%
    prep()

train_preproc <- juice(model_recipe)
train_preproc %>% head()

test_preproc <- bake(model_recipe, test_preproc_tbl)

test_preproc %>% head()

# *** END FIX 1 *** ----

# 3.0. LINEAR METHODS ----
?linear_reg
?set_engine
?fit
?predict.model_fit
?yardstick::metrics

## 3.1. LINEAR REGRESSION - NO ENGINEERED FEATURES ----


### 3.1.1 Model ----

model_01_linear_lm_simple <- linear_reg(mode = "regression") %>% 
    set_engine("lm") %>% 
    fit(price ~ category_2 + frame_material, data = train_tbl)

model_01_linear_lm_simple %>% 
    predict(new_data = test_tbl) %>% 
    bind_cols(test_tbl %>% select(price)) %>% 
    yardstick::metrics(truth = price, estimate = .pred)
    # mutate(residuals = price - .pred) %>% 
    # summarise(
    #     mae = abs(residuals) %>% mean(),
    #     rmse = residuals^2 %>% mean() %>% sqrt()
    # )


### 3.1.2 Feature Importance ----

model_01_linear_lm_simple %>% class() # parsnip model object

model_01_linear_lm_simple$fit %>% class() # lm class

# Because broom supports "lm" method 
# To check go to broom at tidyverse.org > Articles > Available Methods

model_01_linear_lm_simple %>% 
    broom::tidy() %>% 
    arrange(p.value) %>% # p.value is a mesure of importance
    mutate(term = as_factor(term) %>% fct_rev()) %>% 
    ggplot(aes(x = estimate, y = term)) +
    geom_point() + 
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 01: Simple Linear Regression Model (lm)")

### 3.1.3 Function to Calculate Metrics ----

model_01_linear_lm_simple %>% 
    predict(new_data = test_tbl) %>% 
    bind_cols(test_tbl %>% select(price)) %>% 
    yardstick::metrics(truth = price, estimate = .pred)

calc_metrics <- function(model, new_data = test_tbl) {
    
    model %>% 
        predict(new_data = new_data) %>% 
        bind_cols(new_data %>% select(price)) %>% 
        yardstick::metrics(truth = price, estimate = .pred)
}

model_01_linear_lm_simple %>% calc_metrics(test_tbl)
model_01_linear_lm_simple %>% calc_metrics(train_tbl)

## 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

### 3.2.1 Model ----

train_tbl

linear_reg("regression") %>% 
    set_engine("lm") %>% 
    fit(price ~ ., data = train_preproc) ->
    model_02_linear_lm_complex

model_02_linear_lm_complex %>% calc_metrics(new_data = test_tbl)

### 3.2.2 Feature Importance ----

model_02_linear_lm_complex$fit %>% 
    broom::tidy() %>% 
    arrange(p.value) %>% # p.value is a mesure of importance
    mutate(term = as_factor(term) %>% fct_rev()) %>% 
    ggplot(aes(x = estimate, y = term)) +
    geom_point() + 
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 02: Complex Linear Regression Model (lm)")

# By default, the models begin with the first alphabetical element in the category:
# Default frame material = Aluminum
# Default category_2 = Cross Country Race
# Default model_base = Bad Habit

## 3.3 PENALIZED REGRESSION ----

### 3.3.1 Model ----
?linear_reg
?glmnet::glmnet



elastic_net_workflow <- workflow() %>% 
    add_formula(model_formula)

linear_reg(mode = "regression", mixture = 0.5, penalty = tune()) %>% 
    set_engine("glmnet") -> model_03_linear_glmnet_spec

model_03_linear_glmnet_spec

elastic_net_workflow %>% 
    add_model(model_03_linear_glmnet_spec) %>% 
    fit(train_tbl %>% select(-id, -model, -model_tier)) ->
    model_03_linear_glmnet

model_03_linear_glmnet$fit

linear_reg(mode = "regression", mixture = 0.5, penalty = 5.16) %>% 
    set_engine("glmnet") %>% 
    fit(model_formula, data = train_preproc) -> 
    model_03_linear_glmnet_optimal

model_03_linear_glmnet_optimal

model_03_linear_glmnet_optimal %>% calc_metrics(test_tbl)

set.seed(1234)
boots <- bootstraps(train_preproc,
                           times = 10, # number of bootstrap resamples
                           strata = category_2 )

penalty_grid <- grid_regular(penalty(),
                             levels = 100)

penalty_grid

set.seed(2020)
model_03_linear_glmnet_grid <- tune_grid(model_03_linear_glmnet_spec,
                        model_formula,
                        resamples = boots,
                        grid = penalty_grid)

model_03_linear_glmnet_grid %>%  collect_metrics()

best_rmse <- model_03_linear_glmnet_grid %>% 
    select_best(metric = "rmse")

best_rmse

model_03_linear_glmnet_final_spec <- finalize_model(model_03_linear_glmnet_spec, best_rmse)

model_03_linear_glmnet_final <- model_03_linear_glmnet_final_spec %>% 
    fit(model_formula, train_preproc)

model_03_linear_glmnet_final %>% calc_metrics(test_preproc)

### 3.3.2 Feature Importance ----

model_03_linear_glmnet_final$fit %>% 
    broom::tidy() %>% 
    filter(dev.ratio == max(dev.ratio)) %>% 
    arrange(desc(abs(estimate))) %>% # p.value is a mesure of importance
    mutate(term = as_factor(term) %>% fct_rev()) %>% 
    ggplot(aes(x = estimate, y = term)) +
    geom_point() + 
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 03: Generalised Linear Model - Elastic Net Regression (GLMNET)")


# 4.0 TREE-BASED METHODS ----

## 4.1 DECISION TREES ----

### 4.1.1 Model ----
?decision_tree
?rpart::rpart

decision_tree(mode = "regression", 
              cost_complexity = 0.001, 
              tree_depth = 7, 
              min_n = 7) %>% 
    set_engine("rpart") %>% 
    fit(model_formula, data = train_preproc) -> 
    model_04_tree_decision_tree

model_04_tree_decision_tree %>% calc_metrics(test_tbl)

### 4.1.2 Decision Tree Plot ----

?rpart.plot()


model_04_tree_decision_tree$fit %>% 
    rpart.plot(roundint = FALSE,
            type = 1,
            extra = 101,
            fallen.leaves = FALSE,
            cex = 0.7,
            main = "Model 04: Decision Tree",
            box.palette = "YlGnBl")

show.prp.palettes()

## 4.2 RANDOM FOREST ----

### 4.2.1 Model: ranger ----
?rand_forest()
?ranger::ranger
set.seed(1234)
rand_forest(mode = "regression",
            mtry = 8,
            trees = 5000,
            min_n = 10) %>% 
    set_engine("ranger", 
               replace = TRUE, 
               splitrule = "extratrees",
               importance = "impurity") %>% 
    fit(model_formula, data = train_preproc) -> 
    model_05_rand_forest_ranger

model_05_rand_forest_ranger %>% calc_metrics(test_tbl)

### 4.2.2 ranger: Feature Importance ----

model_05_rand_forest_ranger$fit %>% 
    ranger::importance() %>% 
    enframe() %>% 
    arrange(desc(value)) %>% 
    mutate(name = as_factor(name) %>% fct_rev()) %>% 
    ggplot(aes(value, name)) +
    geom_point() +
    labs(title = "ranger: Variable Importance",
         subtitle = "Model 05: Ranger Random Forest Model")
    
model_05_rand_forest_ranger$fit %>% 
    vip(
        num_features = 10,
        geom = "point"
    ) +
    labs(title = "ranger: Variable Importance",
         subtitle = "Model 05: Ranger Random Forest Model",
         x = "Variable",
         y = "Importance")

### 4.2.3 Model randomForest ----
?rand_forest()
?randomForest::randomForest
set.seed(1234)
rand_forest(mode = "regression") %>% 
    set_engine("randomForest") %>% 
    fit(model_formula, data = train_preproc) -> 
    model_06_rand_forest_randomForest

model_06_rand_forest_randomForest %>% calc_metrics(test_preproc)

### 4.2.4 randomForest: Feature Importance ----


model_06_rand_forest_randomForest$fit %>% 
    randomForest::importance() %>% 
    as_tibble(rownames = "name") %>% 
    arrange(desc(IncNodePurity)) %>% 
    mutate(name = as_factor(name) %>% fct_rev()) %>% 
    ggplot(aes(IncNodePurity, name)) +
    geom_point() +
    labs(title = "randomForest: Variable Importance",
         subtitle = "Model 06: randomForest Model")
    

## 4.3 XGBOOST ----

### 4.3.1 Model ----
?boost_tree
?xgboost::xgboost

boost_tree(mode = "regression",
           mtry = 30,
           learn_rate = 0.24,
           tree_depth = 7) %>% 
    set_engine("xgboost", objective = "reg:squarederror") %>% 
    fit(model_formula, data = train_preproc) ->
    model_07_boost_tree_xgboost

set.seed(1234)
model_07_boost_tree_xgboost %>% calc_metrics(test_tbl)

### 4.3.2 Feature Importance ----

model_07_boost_tree_xgboost$fit %>% 
    xgboost::xgb.importance(model = .) %>% 
    as_tibble() %>% 
    arrange(desc(Gain)) %>% 
    mutate(Feature = as_factor(Feature) %>% fct_rev()) %>% 
    ggplot(aes(Gain, Feature)) +
    geom_point() +
    labs(title = "XGBoost: Variable Importance",
         subtitle = "Model 07: XGBoost Model") 

# 5.0. TESTING THE ALGORITHMS OUT ----

bike_features_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(price)) %>%
    
    ggplot(aes(category_2, price)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    coord_flip() +
    facet_wrap(~ frame_material) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    labs(
        title = "Unit Price for Each Model",
        y = "", x = "Category 2"
    ) -> g1

## 5.1. NEW JEKYLL MODEL ----

new_over_mountain_jekyll <- tibble(
    model = "Jekyll Al 1",
    frame_material = "Aluminum",
    category_2 = "Over Mountain",
    model_base = "Jekyll",
    model_tier = "Aluminum 1",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 

new_over_mountain_jekyll

### 5.1.1. Linear Methods ----

predict(model_03_linear_glmnet_final, new_data = new_over_mountain_jekyll)

### 5.1.2. Tree-Based Methods ----

predict(model_07_boost_tree_xgboost, new_data = new_over_mountain_jekyll)

### 5.1.3. Iteration ----

tibble(
    model_id = str_c("Model 0", 1:7),
    model = list(
        model_01_linear_lm_simple,
        model_02_linear_lm_complex,
        model_03_linear_glmnet_final,
        model_04_tree_decision_tree,
        model_05_rand_forest_ranger,
        model_06_rand_forest_randomForest,
        model_07_boost_tree_xgboost
    )
) -> models_tbl

models_tbl

### 5.1.4. Add Prediction ----

models_tbl %>% 
    mutate(predictions = map(model, predict, new_data = new_over_mountain_jekyll)) %>% 
    unnest(predictions) %>% 
    mutate(category_2 = "Over Mountain") %>% 
    left_join(new_over_mountain_jekyll, by = "category_2") ->
    predictions_new_over_mountain_tbl

predictions_new_over_mountain_tbl

### 5.1.5. Update the Plot ----

g1 + geom_point(aes(y = .pred), 
                color = "red",
                alpha = 0.5,
                data = predictions_new_over_mountain_tbl) +
    ggrepel::geom_text_repel(aes(y = .pred, label = model_id),
                             size = 3,
                             max.overlaps = getOption("ggrepel.max.overlaps", default = 16),
                             data = predictions_new_over_mountain_tbl) -> g2

g2

# 5.2 NEW TRIATHALON MODEL ----

new_triathalon_slice_tbl <- tibble(
    model = "Slice Al 1",
    frame_material = "Aluminum",
    category_2 = "Triathalon",
    model_base = "Slice",
    model_tier = "Ultegra",
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 


## 5.2.1. Linear Methods ----

predict(model_03_linear_glmnet_final, new_data = new_triathalon_slice_tbl)

## 5.2.2. Tree-Based Methods ----

predict(model_07_boost_tree_xgboost, new_data = new_triathalon_slice_tbl) 

## 5.2.3. Add Prediction ----

models_tbl %>% 
    mutate(predictions = map(model, predict, new_data = new_triathalon_slice_tbl)) %>% 
    unnest(predictions) %>% 
    mutate(category_2 = "Triathalon") %>% 
    left_join(new_triathalon_slice_tbl, by = "category_2") ->
    predictions_new_triathlon_tbl

predictions_new_triathlon_tbl

g2 + geom_point(aes(y = .pred), 
           color = "red",
           alpha = 0.5,
           data = predictions_new_triathlon_tbl) +
    ggrepel::geom_text_repel(aes(y = .pred, label = model_id),
                             size = 3,
                             max.overlaps = getOption("ggrepel.max.overlaps", default = 16),
                             data = predictions_new_triathlon_tbl) -> g3

g3

# 6.0. ADDITIONAL ADVANCED CONCEPTS ----

# - CLASSIFICATION - Binary & Multi-Class
# - ADVANCED ALGORITHMS
#   - SVMs - svm_poly() and svm_rbf() - Must be normalized
#   - Neural Networks - keras - Must be normalized
#   - Stacking Models 
# - PREPROCESSING - recipes 
# - HYPERPARAMETER TUNING - purrr
# - SAMPLING & CROSS VALIDATION - rsample 
# - AUTOMATIC MACHINE LEARNING - H2O


# 7.0. BONUS - Pre-Processing and SVM-Regression ----

## 7.1. Pre-Processing ----
?recipe
?step_dummy
?prep
?bake

train_tbl

recipe_obj <- recipe(price~., data = train_tbl)  %>% 
    update_role(id, new_role = "id") %>%
    update_role_requirements(role = "id", bake = FALSE) %>%
    step_rm(id, model, model_tier) %>% 
    step_dummy(all_nominal(), one_hot = TRUE) %>% 
    step_log(price, skip = TRUE) %>% 
    step_center(price, skip = TRUE) %>% 
    step_scale(price, skip = TRUE) %>% 
    prep()

bake(recipe_obj, train_tbl) %>% glimpse()

bake(recipe_obj, train_tbl) -> train_transformed_tbl

train_transformed_tbl

bake(recipe_obj, test_tbl) -> test_transformed_tbl

test_transformed_tbl

tidy(recipe_obj)

tidy(recipe_obj, 5) -> scale
scale

tidy(recipe_obj, 4) -> center
center


##  7.2. SVM: Radial Basis ----

?svm_rbf
?kernlab::ksvm

train_transformed_tbl %>% glimpse()

svm_rbf("regression", cost = 11, rbf_sigma = 0.1, margin = 0.25) %>% 
    set_engine("kernlab", scaled = FALSE) %>% 
    fit(price ~ ., data = train_transformed_tbl) ->
    model_08_svm_rbf


model_08_svm_rbf %>% 
    predict(new_data = test_transformed_tbl) %>% 
    mutate(.pred = .pred * scale$value, # we need to perform reverse transformation
           .pred = .pred + center$value,
           .pred = exp(.pred)) %>% 
    bind_cols(test_tbl %>% select(price)) %>% 
    yardstick::metrics(truth = price, estimate = .pred)

## 7.3. SVM Model Predictions ----

bake(recipe_obj, new_data = new_over_mountain_jekyll) %>% 
    predict(object = model_08_svm_rbf, new_data = .) %>% 
    mutate(.pred = .pred * scale$value, # we need to perform reverse transformation
           .pred = .pred + center$value,
           .pred = exp(.pred)) 

predictions_new_over_mountain_tbl

g2

g3

bake(recipe_obj, new_data = new_triathalon_slice_tbl) %>% 
    predict(object = model_08_svm_rbf, new_data = .) %>% 
    mutate(.pred = .pred * scale$value, # we need to perform reverse transformation
           .pred = .pred + center$value,
           .pred = exp(.pred)) 

bike_features_tbl %>% 
    filter(category_2 == "Endurance Road") %>% 
    arrange(price)

# 8.0. SAVING & LOADING MODELS ----

fs::dir_create("00_models")

list(
    "MODEL_01__LM_SIMPLE"       = model_01_linear_lm_simple,
    "MODEL_02__LM_COMPLEX"      = model_02_linear_lm_complex,
    "MODEL_03__GLMNET"          = model_03_linear_glmnet_final,
    "MODEL_04__DECISION_TREE"   = model_04_tree_decision_tree,
    "MODEL_05__RF_RANGER"       = model_05_rand_forest_ranger,
    "MODEL_06__RF_RANDOMFOREST" = model_06_rand_forest_randomForest,
    "MODEL_07__XGBOOST"         = model_07_boost_tree_xgboost,
    "MODEL_08__SVM"             = model_08_svm_rbf
    ) %>% 
    enframe(name = "model_id", value = "model") -> # turns a list into a data frame (tibble)
    models_tbl

models_tbl

models_tbl %>% write_rds("00_models/parsnip_models_tbl.rds")

list(
    "RECIPE_01" = recipe_obj
) %>% 
    enframe(name = "recipe_id", value = "recipe") ->
    recipes_tbl

recipes_tbl %>% write_rds("00_models/recipes_tbl.rds")

calc_metrics %>% write_rds("00_scripts/calc_metrics.rds")

# Reading

models_tbl <- read_rds("00_models/parsnip_models_tbl.rds")

recipes_obj <- read_rds("00_models/recipes_tbl.rds")

calc_metrics <- read_rds("00_scripts/calc_metrics.rds")
