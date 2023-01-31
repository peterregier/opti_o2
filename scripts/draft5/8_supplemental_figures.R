### This script makes supplemental figures
## 
## 2022-11-10
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(cowplot, #plot_grid
       tidyverse, #keep it tidy
       ggpubr,
       rstatix, # %>% t_test()
       lubridate) #force_tz

source("scripts/draft5/0_constants.R")


# 2. Read in datasets ----------------------------------------------------------

## Read in model metrics (R2/NSE/etc)
metrics <- read_csv("data/model_metrics.csv") %>% 
  mutate(season2 = case_when(season == "Spring/Summer" ~ "Summer", 
                             season == "Fall/Winter" ~ "Winter"))

fi <- read_csv("data/model_feature_importance.csv") %>% 
  mutate(season = fct_relevel(season, "Spring/Summer")) %>% 
  mutate(pred_cat = ifelse(grepl("creek", predictor), "Aquatic", 
                           ifelse(grepl("fp", predictor), "Terrestrial",
                                  "Climatic")))

model_data <- read_csv("data/model_predictions.csv") %>% 
  mutate(season = fct_relevel(season, "Spring/Summer"))

## Data for S1
df_ts <- read_csv("data/220930_data_with_features.csv") %>% 
  mutate(season2 = case_when(season == "Spring/Summer" ~ "Summer", 
                             season == "Fall/Winter" ~ "Winter"))


# 3. S1: precip/PAR boxplots ---------------------------------------------------

par_boxplot <- ggplot(df_ts, aes(season2, par)) + 
  geom_boxplot(fill = "gray") + 
  scale_y_continuous(trans = "sqrt") + 
  stat_compare_means(label.x = 1.5, label = "p.signif") + 
  labs(x = "", y = "PAR")

rain_boxplot <- ggplot(df_ts, aes(season2, rain_mm_1d)) + 
  geom_boxplot(fill = "gray") + 
  scale_y_continuous(trans = "sqrt") + 
  stat_compare_means(label.x = 1.5, label = "p.signif") + 
  labs(x = "", y = "24h Rain (mm)")

plot_grid(par_boxplot, rain_boxplot, nrow = 1)
ggsave("graphs/draft5/S1_climate-boxplots.pdf", width = 6, height = 3)


# 4. S2: GOF figure ------------------------------------------------------------

metrics_by_window <- metrics %>% 
  select(season2, window_c, r2, nse) %>% 
  pivot_longer(cols = c(r2, nse))

metrics_all <- metrics %>%
  #filter(window_c == "S4" | window_c == "F1") %>% 
  group_by(season2) %>% 
  summarize(R2 = mean(r2), 
            NSE = mean(nse))

ggplot(metrics_by_window, aes(window_c, value, fill = name)) + 
  geom_col(position = "dodge") + 
  geom_hline(data = metrics_all, aes(yintercept = R2)) + 
  geom_hline(data = metrics_all, aes(yintercept = NSE), linetype = "dashed") + 
  facet_wrap(~season2, nrow = 1, scales = "free_x") + 
  labs(x = "Test window", y = "R2/NSE Value", fill = "Metric")
ggsave("graphs/draft5/S2_model_GOF.png", width = 6, height = 4)



# 5. S3: O2 time-series

model_data <- read_csv("data/model_predictions.csv") %>% 
  mutate(season = fct_relevel(season, "Spring/Summer")) %>% 
  group_by(season) 

make_cusum2 <- function(driver, response){

  mean_driver = median(model_data %>% pull({{driver}}))
  model_data %>%
    ungroup() %>%
    select(season, {{response}}, {{driver}}) %>%
    arrange(., {{driver}}) %>% ## organize dataset by driver in ascending order
    #group_by(season) %>%
    mutate(normalized_do = ({{response}} - mean({{response}})) / sd({{response}})) %>% ## Normalized DO
    mutate(cusum_do = cumsum(normalized_do)) %>%  ## Cusum DO
    select(-{{response}}) %>%
    ggplot(aes({{driver}}, cusum_do)) +
    geom_line() +
    geom_vline(xintercept = mean_driver)
}



air_temp <- bind_rows(make_cusum2(air_temp_1d, creek_do) %>%
                        mutate(site = "1. Creek"),
                      make_cusum2(air_temp_1d, seep_do) %>%
                        mutate(site = "2. Seep"),
                      make_cusum2(air_temp_1d, fp_do) %>%
                        mutate(site = "3. Floodplain"))

creek_depth <- bind_rows(make_cusum2(creek_depth_1d, creek_do) %>%
                           mutate(site = "1. Creek"),
                         make_cusum2(creek_depth_1d, seep_do) %>%
                           mutate(site = "2. Seep"),
                         make_cusum2(creek_depth_1d, fp_do) %>%
                           mutate(site = "3. Floodplain"))

atemp_plot <- ggplot(air_temp, aes(air_temp_1d, cusum_do, color = site)) +
  geom_line()

cdepth_plot <- ggplot(creek_depth, aes(creek_depth_1d, cusum_do, color = site)) +
  geom_line()


# SZ. Make three example datasets showing different drivers on different days

figure_dates <- as.Date(c("2020-03-17", "2020-06-05", "2020-11-27"))

x <- model_data %>% 
  ungroup() %>% 
  filter(date(datetime) %in% figure_dates) %>% 
  select(datetime, contains("_do")) %>% 
  select(-contains("_1d")) %>% 
  pivot_longer(cols = c(-datetime)) 

p_depth <- model_data %>% 
  ungroup() %>% 
  filter(date(datetime) %in% figure_dates) %>% 
  select(datetime, creek_depth) %>% 
ggplot(aes(datetime, creek_depth)) + 
  geom_line() + 
  facet_wrap(~date(datetime), nrow = 1, scale = "free_x") + 
  labs(x = "", y = "Creek Depth (cm)")

p_do <- ggplot(x, aes(datetime, value, color = name)) + 
  geom_line() + 
  facet_wrap(~date(datetime), nrow = 1, scale = "free_x") + 
  theme(legend.position = c(0.5, 0.8))

plot_grid(p_depth, p_do, rel_heights = c(0.5, 1), ncol = 1)
ggsave("graphs/draft5/SZ_three_diel_timeseries.png", width = 9, height = 6)


