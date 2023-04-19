## This script constructs the models and associated info for the Opti-O2 
## manuscript. There are several modeling decisions that were made prior to this 
## script where a combination of NSE and R2 were used to determine the best set 
##of parameters to maximize fits for all
## WINDOWS of each season (Spring/Summer and Fall/Winter). The parameters are:
### # of windows (# of splits for individual test datasets): 5 (~spring-neap cycles)
### MTRY: 5 (Best for Fall/Winter, 6 was best for Spring/Summer, but 5 also 
### # matches default mtry (sqrt(15) ~ 4, and time is forced))
### BOOTSTRAP TYPE: moving, which consistently outperformed circular and nonoverlapping
### BLOCK SIZE: 576 (2 days) - best for Spring/Summer, close 2nd for Fall/Winter, 
### (only looked at 1-day and 2-day blocks to match lags)
## 
## Updated 2022-04-11 by pjr
##
# ####################### #
# ####################### #


# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(cowplot, #plot_grid
       tidyverse, #keep it tidy
       tidymodels, 
       hms,
       rsample, 
       ranger,
       rangerts,
       recipes, # prep()
       suncalc, #getSunlightPosition
       tictoc, #time stuff
       hydroGOF, #calculate metrics
       lubridate) #force_tz

## Set up lat and long for suncalc calls (not used in final version)
#bc_lat = 46.90579
#bc_long = -123.98025

## Set ggplot theme
theme_set(theme_bw())


# 2. Read in / set up dataset --------------------------------------------------

## First, read in data
df <- read_csv("data/230411_data_with_features.csv") %>% 
  group_by(season) %>% 
  slice(577:n()) %>% # need to clean out the first 2 days with NAs due to lagging
  #mutate(time = as_hms(datetime)) %>% # add time
  ungroup()
  

# 3. Set up models -------------------------------------------------------------
num_windows <- max(df$window)

## First, set up dataframe with list of models
model_list <- tibble(expand.grid(num_window = c(1:num_windows), 
                                 season_selection = c("Spring/Summer", "Fall/Winter")))

## Set rangerts parameters
bootstrap_type = "moving"
block_size = 576

## Now set hyperparameters
n_tree = 1000
m_try = 5

## Set up a vector of model variables #R2 of 0.414 and 0.548 for Spring / Winter
model_vars <- c("sin_time", "creek_temp", "creek_sal", "creek_do", "creek_depth", 
                "creek_depth_1d", "creek_sal_1d", "creek_temp_1d", "creek_do_1d", 
                #"creek_depth_2d", "creek_sal_2d", "creek_temp_2d",  "creek_do_2d",
                "fp_temp", "fp_sal", "fp_do", "fp_depth", 
                "fp_depth_1d", "fp_sal_1d", "fp_temp_1d", "fp_do_1d", 
                #"fp_depth_2d", "fp_sal_2d", "fp_temp_2d", "fp_do_2d", 
                "air_temp", "rain_mm", "par", "BP_hPa",
                "air_temp_1d", "par_1d", "rain_mm_1d", "BP_hPa_1d" 
                #"air_temp_2d", "rain_mm_2d", "par_2d", "BP_hPa_2d"
                )


# 4. Run models! ---------------------------------------------------------------

make_model <- function(num_window = num_window, 
                          season_selection = season_selection){
  
  x <- df %>% 
    filter(season == season_selection) %>% 
    select(datetime, window, seep_do, model_vars)
  
  df_train <- x %>% filter(window != num_window)
  df_test <- x %>% filter(window == num_window)
  
  model_recipe <- df_train %>% 
    select(-datetime, -window) %>% 
    recipe(seep_do ~ .) %>% 
    #step_integer(time) %>% 
    step_corr(all_predictors()) %>% 
    step_normalize(all_predictors(), -all_outcomes()) %>% 
    recipes::prep()
  
  test <- model_recipe %>% 
    bake(df_test)
  
  train <- juice(model_recipe)
  
  # rf_model = rangerts(seep_do ~ ., data = train,
  #                     num.trees = n_tree,
  #                     mtry = m_try,
  #                     split.select.weights = c(1, rep(0.5, times = ncol(train)-2)),
  #                     bootstrap.ts = bootstrap_type,
  #                     block.size = block_size,
  #                     importance = "impurity")
  
  rf_model = ranger(seep_do ~ ., data = train,
                     num.trees = n_tree,
                     mtry = m_try,
                     split.select.weights = c(1, rep(0.5, times = ncol(train)-2)),
                     importance = "impurity")
  
  print(paste0("Model ", "for window = ", num_window, 
               " / ", season_selection, " is done!"))
  
  ### SET UP DATASET ###
  ## Create predictions
  df_test$pred <- predict(rf_model, test)$predictions
  
  ## Finally, add model parameters to df_test
  df_test <- df_test %>% 
    mutate(season = season_selection, 
           window = num_window, 
           window_c = ifelse(season == "Spring/Summer", 
                             paste0("S", window), 
                             paste0("F", window)))
  
  ### SET UP METRICS ####
  ## Calculate metrics
  mae = hydroGOF::mae(df_test$pred, df_test$seep_do)
  nse = hydroGOF::NSE(df_test$pred, df_test$seep_do)
  r2 = hydroGOF::gof(df_test$pred, df_test$seep_do)["R2", ]
  rmse = hydroGOF::rmse(df_test$pred, df_test$seep_do)
  
  metrics <- tibble(mae = mae, 
                    nse = nse, 
                    r2 = r2, 
                    rmse = rmse, 
                    season = season_selection, 
                     window = num_window) %>% 
    mutate(window_c = ifelse(season == "Spring/Summer", 
                                      paste0("S", window), 
                                      paste0("F", window)))
  
  ### SET UP FEATURE IMPORTANCE ###
  ## Pull up variable names and set up column names
  var_names <- rf_model$variable.importance
  col_names <- c("predictor", "raw_fi")
  
  ## Convert feature importance to a tibble with variables as a column
  fi0 <- as.data.frame(var_names) %>% 
    tibble::rownames_to_column() %>% 
    as_tibble() %>% 
    select(1, last_col())
  
  ## Rename columns
  colnames(fi0) = col_names
  
  ## Finally, calculate fi as percentages
  fi = fi0 %>%
    filter(predictor != "sin_time") %>% #remove forced column
    select(predictor, raw_fi) %>%
    mutate(fi = raw_fi / sum(raw_fi)) %>% 
    mutate(season = season_selection, 
           window = num_window, 
           window_c = ifelse(season == "Spring/Summer", 
                             paste0("S", window), 
                             paste0("F", window)))
  
  ## Create output with all aspects
  output <- tibble(metrics = nest(metrics), 
                   fi = nest(fi),
                   test_data = nest(df_test))
  
  return(output)
}

## Create the actual models, but don't bind so we can pull different elements
tic("make models") #4.7 min
models <- model_list %>% 
  pmap(make_model)
toc()

## Pull specific elements to analyze model performance
extract_stuff <- function(what_ya_want){
  bind_rows(models) %>% 
    select({{what_ya_want}}) %>% 
    pull() %>% 
    unnest()
}

metrics <- extract_stuff(metrics)
fi <- extract_stuff(fi)
model_data <- extract_stuff(test_data)


write_csv(metrics, "data/model_metrics.csv")
write_csv(fi, "data/model_feature_importance.csv")
write_csv(model_data, "data/model_predictions.csv")

num_vars <- length(model_vars)
num_vars_used <- length(unique(fi$predictor))

num_vars_used / num_vars
setdiff(model_vars, unique(fi$predictor))

