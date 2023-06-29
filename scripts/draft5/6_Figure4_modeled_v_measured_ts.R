## This script makes time-series plots to compare modeled and measured Seep DO
## 
## 2023-06-13
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

## Data for S1
predictions <- read_csv("data/model_predictions.csv") %>% 
  dplyr::mutate(res = pred - seep_do) %>% 
  mutate(season = case_when(season == "Spring/Summer" ~ "Spring/Summer 2020", 
                            season == "Fall/Winter" ~ "Fall 2020 - Winter 2021"))

ggplot(predictions %>% mutate(season = fct_relevel(base::as.factor(season), "Spring/Summer 2020")), aes(datetime)) + 
  geom_point(aes(y = seep_do), size = 0.2, color = "gray") + 
  geom_line(aes(y = pred), color = "red", alpha = 0.8) + 
  facet_wrap(~season, scales = "free") + 
  labs(x = "", y = "Seep DO (mg/L)")
ggsave("graphs/4_Figure4_modeled_v_actual_ts.png", width = 10, height = 4)