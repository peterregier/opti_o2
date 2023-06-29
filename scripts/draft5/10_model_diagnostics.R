## This script examines a couple different model diagnostics recommended by reivewers
## for inclusion in the manuscript: 
### 1. Different combinations of drivers as predictor variables fed to models in 
#######order to understand how that impacts predictive power. 
### 2. ....
##
## 2023-04-12
## Peter Regier
##
# ########### #
# ########### #


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

theme_set(theme_bw())


# 2. Read in / set up dataset --------------------------------------------------

## First, read in data
df <- read_csv("data/230411_data_with_features.csv") %>% 
  group_by(season) %>% 
  slice(577:n()) %>% # need to clean out the first 2 days with NAs due to lagging
  ungroup()


# 3. Create additional candidate models and test fit ---------------------------
num_windows <- max(df$window)

## First, set up dataframe with list of models
model_list <- tibble(expand.grid(num_window = c(1:num_windows), 
                                 season_selection = c("Spring/Summer", "Fall/Winter"), 
                                 predictor_type = c("Aquatic", "Climatic", "Terrestrial", 
                                                    "Aquatic+Climatic", 
                                                    "Aquatic+Terrestrial", 
                                                    "Climatic+Terrestrial")))

## Set rangerts parameters
bootstrap_type = "moving"
block_size = 576

## Now set hyperparameters
n_tree = 1000
m_try = 5

## Set up a vector of model variables #R2 of 0.414 and 0.548 for Spring / Winter
model_vars <- c("creek_temp", "creek_sal", "creek_do", "creek_depth", 
                "creek_depth_1d", "creek_sal_1d", "creek_temp_1d", "creek_do_1d", 
                "fp_temp", "fp_sal", "fp_do", "fp_depth", 
                "fp_depth_1d", "fp_sal_1d", "fp_temp_1d", "fp_do_1d", 
                "air_temp", "rain_mm", "par", "BP_hPa",
                "air_temp_1d", "par_1d", "rain_mm_1d", "BP_hPa_1d")

var_types_1driver = tibble(vars = model_vars, 
                   type = case_when(grepl("creek_", vars) ~ "Aquatic", 
                                    grepl("fp_", vars) ~ "Terrestrial", 
                                    TRUE ~ "Climatic"))

var_types_2drivers <- bind_rows(var_types_1driver %>% filter(type == "Aquatic" | type == "Climatic") %>% mutate(type = "Aquatic+Climatic"), 
                                var_types_1driver %>% filter(type == "Aquatic" | type == "Terrestrial") %>% mutate(type = "Aquatic+Terrestrial"), 
                                var_types_1driver %>% filter(type == "Climatic" | type == "Terrestrial") %>% mutate(type = "Climatic+Terrestrial"))

var_types <- bind_rows(var_types_1driver, 
                       var_types_2drivers)


make_model <- function(num_window = num_window, 
                       season_selection = season_selection, 
                       predictor_type = predictor_type){
  
  vars_to_use <- var_types %>% 
    filter(type == predictor_type) %>% 
    pull(vars)  %>% 
    append("sin_time")
  
  x <- df %>% 
    filter(season == season_selection) %>% 
    select(datetime, window, seep_do, vars_to_use) 
  
  head(x)
  
  df_train <- x %>% filter(window != num_window)
  df_test <- x %>% filter(window == num_window)

  model_recipe <- df_train %>%
    select(-datetime, -window) %>%
    recipe(seep_do ~ .) %>%
    step_corr(all_predictors()) %>%
    step_normalize(all_predictors(), -all_outcomes()) %>%
    recipes::prep()

  test <- model_recipe %>%
    bake(df_test)

  train <- juice(model_recipe)

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

  return(metrics)
}

## Create the actual models, but don't bind so we can pull different elements
tic("make models") #30.6 min ##NOTE: USE read_csv() SHORTCUT BELOW!
models <- model_list %>% 
  pmap(make_model)
toc()

og_models <- read_csv("data/model_metrics.csv") %>% 
  mutate(type = "All")

all_models <- bind_rows(models) %>% 
  bind_cols(type = model_list$predictor_type) %>% 
  bind_rows(og_models) 

write_csv(all_models, "data/230412_fits_for_all_possible_driver_combos.csv")
#all_models <- read_csv("data/230412_fits_for_all_possible_driver_combos.csv")

all_models %>% 
  group_by(type) %>% 
  summarize(mean_r2 = mean(r2), 
            min_r2 = min(r2), 
            max_r2 = max(r2)) %>% 
  ggplot(aes(type)) + 
  geom_col(aes(y = mean_r2)) + 
  geom_errorbar(aes(ymin = min_r2, ymax = max_r2), position = position_dodge(0.8), width = 0.2) +
  geom_text(aes(y = mean_r2 / 2, label = round(mean_r2, 2)), color = "white") + 
  labs(x = "Predictors included (based on driver)", y = bquote(R^2)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("graphs/S1_r2_by_model_input_type.png", width = 6, height = 4.5)


# 3. Construct models with white noise included as a predictor -----------------

df_mod <- df %>% 
  mutate(white_noise = rnorm(1:n(), mean = 0, sd = 1))


## First, set up dataframe with list of models
model_list2 <- tibble(expand.grid(num_window = c(1:num_windows), 
                                 season_selection = c("Spring/Summer", "Fall/Winter")))


## Set up a vector of model variables #R2 of 0.414 and 0.548 for Spring / Winter
model_vars_wn <- c("sin_time", "white_noise", 
                   "creek_temp", "creek_sal", "creek_do", "creek_depth", 
                "creek_depth_1d", "creek_sal_1d", "creek_temp_1d", "creek_do_1d", 
                "fp_temp", "fp_sal", "fp_do", "fp_depth", 
                "fp_depth_1d", "fp_sal_1d", "fp_temp_1d", "fp_do_1d", 
                "air_temp", "rain_mm", "par", "BP_hPa",
                "air_temp_1d", "par_1d", "rain_mm_1d", "BP_hPa_1d")

make_model_wn <- function(num_window = num_window, 
                       season_selection = season_selection){
  
  x <- df_mod %>% 
    filter(season == season_selection) %>% 
    select(datetime, window, seep_do, model_vars_wn)
  
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

  return(fi)
}


## Create the actual models, but don't bind so we can pull different elements
tic("make models")
models_wn <- model_list2 %>% 
  pmap(make_model_wn)
toc()

wn_fi <- bind_rows(models_wn) %>% 
  mutate(season2 = case_when(season == "Spring/Summer" ~ "Summer", 
                             season == "Fall/Winter" ~ "Winter")) %>% 
  mutate(season = fct_relevel(season, "Spring/Summer")) %>% 
  mutate(pred_cat = case_when(grepl("creek", predictor) ~ "Aquatic", 
                              grepl("fp", predictor) ~ "Terrestrial",
                              grepl("white_noise", predictor) ~ "White Noise",
                              TRUE ~ "Climatic"))

wn_stats <- wn_fi %>% 
  mutate(pred2 = case_when(grepl("creek_depth", predictor) ~ "Creek depth", 
                           grepl("creek_sal", predictor) ~ "Creek salinity", 
                           grepl("creek_do", predictor) ~ "Creek DO", 
                           grepl("creek_temp", predictor) ~ "Creek temp", 
                           grepl("fp_do", predictor) ~ "FP DO", 
                           grepl("fp_temp", predictor) ~ "FP temp", 
                           grepl("fp_sal", predictor) ~ "FP salinity", 
                           grepl("fp_depth", predictor) ~ "FP depth", 
                           grepl("rain", predictor) ~ "Rain", 
                           grepl("temp", predictor) ~ "Temp",
                           grepl("par", predictor) ~ "Solar Flux Dens.",
                           grepl("hPa", predictor) ~ "Baro. Press.", 
                           grepl("white_noise", predictor) ~ "White noise")) %>% 
  group_by(pred2) %>% 
  summarize(mean = mean(fi) * 100, 
            min = min(fi) * 100, 
            max = max(fi) * 100)
  
ggplot(wn_stats, aes(fct_reorder(pred2, mean), mean)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(0.8), width = 0.2) + 
  labs(x = "Predictor", y = "Feature importance (%)") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("graphs/S2_feature_importance_white_noise.png", width = 8, height = 5)


# 4. Make residual plots -------------------------------------------------------

predictions <- read_csv("data/model_predictions.csv") %>% 
  dplyr::mutate(res = pred - seep_do)


mean_do <- predictions %>% 
  group_by(season) %>% 
  summarize(mean = mean(seep_do))

predictions %>% 
  mutate(time = as_hms(datetime)) %>% 
  group_by(time, season) %>% 
  summarize(mean = mean(res), 
            min = min(res), 
            max = max(res)) %>% 
  ggplot(aes(x = time)) + 
  geom_errorbar(aes(ymin = min, ymax = max), color = "gray") + 
  geom_point(aes(y = mean)) + 
  geom_hline(data = mean_do, aes(yintercept = mean)) + 
  geom_hline(data = mean_do, aes(yintercept = mean * -1)) + 
  labs(x = "Time of Day", y = "Mean residuals (mg/L)") + 
  facet_wrap(~season)
ggsave("graphs/Sx_residuals_by_time_of_day.png", width = 10, height = 4)


ggplot(predictions, aes(datetime)) + 
  geom_point(aes(y = seep_do), size = 0.2, color = "gray") + 
  geom_line(aes(y = pred), color = "red", alpha = 0.8) + 
  facet_wrap(~season, scales = "free")
ggsave("graphs/Sx_modeled_v_actual_ts.png", width = 10, height = 4)

## Make a residuals plot
ggplot(predictions %>% mutate(season = case_when(season == "Fall/Winter" ~ "Winter", 
                                                 season == "Spring/Summer" ~ "Summer")), 
       aes(seep_do, pred, color = season)) + 
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth(method = "lm", se = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  labs(x = "Measured Seep DO (mg/L)", y = "Predicted Seep DO (mg/L)", color ="") + 
  theme(legend.position = c(0.8, 0.3), 
        legend.background = element_blank(), 
        legend.box.background = element_blank())
ggsave("graphs/Sx_modeled_v_actual_scatter.png", width = 5, height = 5)







