## This script explores feature importance across models by season: 
## Figure 3A: boxplots comparing median values by season across the 3 drivers
## Figure 3B: stacked geom_col comparing individual variable importance across
## season, faceted by driver.
##
## 2022-11-10
## Peter Regier


# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(cowplot, #plot_grid
       tidyverse, #keep it tidy
       ggpubr,
       rstatix, # %>% t_test()
       lubridate) #force_tz

source("scripts/draft5/0_constants.R")


# 2. Read in datasets ----------------------------------------------------------

## Import FI dataset, relevel season, and add category
fi <- read_csv("data/model_feature_importance.csv") %>% 
  mutate(season2 = case_when(season == "Spring/Summer" ~ "Summer", 
                             season == "Fall/Winter" ~ "Winter")) %>% 
  mutate(season = fct_relevel(season, "Spring/Summer")) %>% 
  mutate(pred_cat = ifelse(grepl("creek", predictor), "Aquatic", 
                           ifelse(grepl("fp", predictor), "Terrestrial",
                                  "Climatic")))


# 3. Create Figure 3A boxplot --------------------------------------------------

## Summarize FI by driver
fi_stats <- fi %>% group_by(season2, window_c, pred_cat) %>% 
  summarize(fi = sum(fi * 100)) %>% 
  ungroup() %>% 
  group_by(pred_cat, season2) %>% 
  summarize(mean = mean(fi), 
            min = min(fi), 
            max = max(fi)) %>% 
  mutate(driver = "All drivers")

## Make plot
fi_plot <- ggplot(fi_stats, aes(pred_cat, mean, fill = season2)) + 
  geom_col(alpha = 0.8, position = position_dodge(0.8), width = 0.8, color = "gray30") + 
  geom_errorbar(aes(ymin = min, ymax = max), position = position_dodge(0.8), width = 0.2) + 
  facet_wrap(~driver) +
  scale_fill_manual(values = rev(season_colors)) + 
  labs(x = "", y = "Feature importance (%)", fill = "") + 
  theme(legend.position = c(0.5, 0.8), 
        legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        axis.text.x = element_text(angle = 30, hjust = 1)) + 
  scale_y_continuous(limits = c(0, 65))


# 4. Create Figure 3B stacked bar chart ----------------------------------------

fi_summary <- fi %>% 
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
                           grepl("par", predictor) ~ "SFD",
                           grepl("hPa", predictor) ~ "BP")) %>% 
  group_by(pred2, season2) %>% 
  summarize(pred_cat = first(pred_cat), 
            fi = sum(fi)) %>% 
  ungroup() %>% 
  group_by(season2) %>% 
  mutate(fi_n = fi / sum(fi))
  

variable_plot <- ggplot(fi_summary, aes(season2, fi * (100/5), fill = pred2)) + 
  # divide by 5 because 5 windows (sum above, mean gave weird values)
  geom_col(width = 0.8, alpha = 0.8) + 
  scale_fill_brewer(palette = "Paired") + 
  facet_wrap(~pred_cat, scale = "free_x") + 
  labs(x = "", y = "Feature importance (%)", fill = "")  + 
  scale_y_continuous(limits = c(0, 65)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## Create a table for use for stats 
print(fi_summary %>% 
  ungroup() %>% 
  group_by(season2, pred2) %>% 
  summarize(fi * 20), n = 50)

# 5. Create plot and export ----------------------------------------------------

plot_grid(fi_plot, NULL, variable_plot, nrow = 1, align = "hv",
          rel_widths = c(0.6, 0.1, 1), labels = c("A", "", "B"))
ggsave("graphs/draft5/3_Figure3_feature_importance.png", 
       width = 10, height = 5)

