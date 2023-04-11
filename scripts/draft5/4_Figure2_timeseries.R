## This script makes some time-series and calculates some stats to be used to 
## contextualize and interpret model outputs
##

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(cowplot, #plot_grid
       tidyverse, #keep it tidy
       tidymodels, 
       ggpubr,
       plotly,
       parsedate, 
       lubridate) #force_tz

## Read in constants
source("scripts/draft5/0_constants.R")

## Set ggplot theme
theme_set(theme_bw())


# 2. Read in / set up dataset --------------------------------------------------

## First, read in data
df <- read_csv("data/220930_data_with_features.csv")


# 3. Calculate some stats ------------------------------------------------------

df_stats <- df %>% 
  group_by(season, window) %>% 
  summarize(creek_depth = mean(creek_depth), 
            creek_sal = mean(creek_sal),
            fp_depth = mean(fp_depth),
            fp_sal = mean(fp_sal),
            sfd = mean(par),
            rain_mm = mean(rain_mm))


# 4. Make some graphs ----------------------------------------------------------

## Window locations
windows <- df %>% 
  group_by(window_c) %>% 
  summarize(window_start = first(datetime))

## Make a graph for time-series
seep_ts <- ggplot(df, aes(datetime, seep_do)) + 
  geom_line(color = "gray") + 
  geom_smooth(method = "lm", se = F, color = "black") + 
  geom_point(data = df %>% filter(fp_depth < 0), aes(datetime, seep_do)) + 
  facet_wrap(~fct_relevel(as.factor(season), "Spring/Summer"), 
             nrow = 1, scales = "free_x") + 
  labs(x = "", y = "Seep DO (mg/L)")

## Set up function to make time-series graphs
plot_ts <- function(var, y_lab){
  
  ## Set some constants up
  df$index <- 1:nrow(df)
  
  colors = c("#595F72", "#46ACC2")
  p_value_position = max(df %>% select({{var}}) %>% drop_na()) * 0.9
  #r2_s = round(summary(lm({{var}} ~ index, data = df %>% filter(season == "Spring/Summer")))[[4]][2, 1] * 8640, 1)
#  r2_w = round(summary(lm({{var}} ~ index, data = df %>% filter(season == "Fall/Winter")))[[4]][2, 1] * 8640, 1)
  # slopes = tibble(season = c("Spring/Summer", "Fall/Winter"), 
  #                 slope = c(paste0("m = ", r2_s), paste0("m = ", r2_w)), 
  #                 x = c(max(df %>% filter(season == "Spring/Summer") %>% pull(datetime) - days(20)), 
  #                       max(df %>% filter(season == "Fall/Winter") %>% pull(datetime) - days(20))), 
  #                 y = c(max(df %>% pull({{var}}))))
  
  ## Make time-series
  ts_plot <- ggplot(df, aes(datetime, {{var}})) + 
    geom_line(aes(color = season), show.legend = F) + 
    geom_smooth(method = "lm", se = F, color = "black") + 
    geom_vline(data = windows, aes(xintercept = window_start), linetype = "dashed") + 
    facet_wrap(~fct_relevel(as.factor(season), "Spring/Summer"), 
               nrow = 1, scales = "free_x") + 
    labs(x = "", y = y_lab) + 
    scale_color_manual(values = colors)# + 
    #geom_text(data = slopes, aes(x = x, y = y, label = slope))
  
  ## make boxplot
  boxplot <- ggplot(df %>% mutate(season = case_when(season == "Spring/Summer" ~ "S", 
                                                     season == "Fall/Winter" ~ "W"), 
                                  case = "By Season"), 
                    aes(fct_relevel(as.factor(season), "S"), {{var}}, fill = season)) + 
    geom_boxplot(show.legend = F, width = 0.7) + 
    labs(x = "Season", y = "") + 
    stat_compare_means(label = "p.signif", 
                       label.x = 1.5, 
                       label.y = p_value_position) + 
    scale_fill_manual(values = colors) + 
    facet_wrap(~case)
  
  plot_grid(ts_plot, boxplot, nrow = 1, rel_widths = c(1, 0.2))
}


plot_grid(plot_ts(seep_do, "Seep DO (mg/L)"), 
          plot_ts(air_temp, "Air Temp. (C)"), 
          plot_ts(creek_depth, "Creek depth (cm)"), 
          plot_ts(fp_depth, "FP depth (cm)"), 
          ncol = 1, labels = c("A", "B", "C", "D"))
ggsave("graphs/draft5/2_Figure2_ts.png", width = 10, height = 8)



# calculate_slope <- function(){
#   m_s <- round(summary(lm(fp_depth ~ index, data = df %>% filter(season == "Spring/Summer")))[[4]][2, 1] * 12*24*30, 1)
#   m_w <- round(summary(lm(fp_depth ~ index, data = df %>% filter(season == "Fall/Winter")))[[4]][2, 1] * 12*24*30, 1)
#   tibble(r2 = c(m_s, m_w))
# }
# 
# calculate_slope()



# summary(lm(seep_do~datetime, data = df %>% 
#   filter(season == "Fall/Winter")))
# 
# 4.442e-07 * 288 * 30

# make_boxplot <- function(var, y_lab){
#   df %>% 
#     ggplot(aes(window_c, {{var}}, color = location)) + 
#     geom_boxplot() + 
#     facet_wrap(~fct_relevel(as.factor(season), "Spring/Summer"), 
#                nrow = 1, scales = "free_x") + 
#     labs(x = "", y = y_lab, color = "") + 
#     theme(legend.position = c(0.8, 0.8), 
#           legend.background = element_blank())
# }

# make_boxplot2 <- function(creek_var, fp_var, y_lab){
#   df %>% 
#     rename("Creek" = {{creek_var}}, 
#            "FP" = {{fp_var}}) %>% 
#     select(window_c, season, Creek, FP) %>% 
#     pivot_longer(cols = -c(window_c, season), names_to = "location", values_to = "value") %>% 
#     ggplot(aes(window_c, value, color = location)) + 
#     geom_boxplot() + 
#     facet_wrap(~fct_relevel(as.factor(season), "Spring/Summer"), 
#                nrow = 1, scales = "free_x") + 
#     labs(x = "", y = y_lab, color = "") + 
#     theme(legend.position = c(0.8, 0.8), 
#           legend.background = element_blank())
# }

# temp_boxplot <- df %>% 
#   select(season, creek_temp, fp_temp, air_temp) %>% 
#   rename("Creek" = creek_temp, 
#          "FP" = fp_temp, 
#          "Air" = air_temp) %>% 
#   pivot_longer(cols = -c(season), names_to = "location", values_to = "temp") %>% 
#   ggplot(aes(location, temp, fill = fct_relevel(as.factor(season), "Spring/Summer"), )) + 
#   geom_boxplot() +
#   labs(x = "", y = "Temp. (C)", fill = "Season") + 
#   theme(legend.position = c(0.8, 0.8), 
#         legend.background = element_blank())

# depth_boxplot <- df %>% 
#   select(season, creek_depth, fp_depth) %>% 
#   rename("Creek" = creek_depth, 
#          "FP" = fp_depth) %>% 
#   pivot_longer(cols = -c(season), names_to = "location", values_to = "depth") %>% 
#   ggplot(aes(location, depth, fill = fct_relevel(as.factor(season), "Spring/Summer"), )) + 
#   geom_boxplot() +
#   labs(x = "", y = "Depth (m)", fill = "Season") + 
#   theme(legend.position = c(0.7, 0.8), 
#         legend.background = element_blank())
# 
# sal_boxplot <- df %>% 
#   select(season, creek_sal, fp_sal) %>% 
#   rename("Creek" = creek_sal, 
#          "FP" = fp_sal) %>% 
#   pivot_longer(cols = -c(season), names_to = "location", values_to = "value") %>% 
#   ggplot(aes(location, value, fill = fct_relevel(as.factor(season), "Spring/Summer"), )) + 
#   geom_boxplot() +
#   labs(x = "", y = "Salinity (PSU)", fill = "Season") + 
#   theme(legend.position = c(0.7, 0.8), 
#         legend.background = element_blank())
# 
# boxplots <- plot_grid(temp_boxplot, depth_boxplot, sal_boxplot,
#           rel_widths = c(3, 2, 2), nrow = 1)
# 
# plot_grid(ts_plot, boxplots, ncol = 1)
# ggsave("graphs/draft5/2_Figure2.png", width = 10, height = 7)


## Make supplemental figure for boxplots that may be useful...

sfd_boxplot <- ggplot(df, aes(fct_relevel(as.factor(season), "Spring/Summer"), sfd)) + 
  geom_boxplot(fill = "gray") + 
  scale_y_continuous(trans = "sqrt") + 
  stat_compare_means(label.x = 1.5, label = "p.signif") + 
  labs(x = "", y = "SFD")

rain_boxplot <- ggplot(df, aes(fct_relevel(as.factor(season), "Spring/Summer"), rain_mm_1d)) + 
  geom_boxplot(fill = "gray") + 
  scale_y_continuous(trans = "sqrt") + 
  stat_compare_means(label.x = 1.5, label = "p.signif") + 
  labs(x = "", y = "24h Rain (mm)")

bp_boxplot <- 
  ggplot(df, aes(fct_relevel(as.factor(season), "Spring/Summer"), BP_hPa)) + 
  geom_boxplot(fill = "gray") + 
  scale_y_continuous(trans = "sqrt") + 
  labs(x = "", y = "BP (hPa)")

plot_grid(sfd_boxplot, rain_boxplot, nrow = 1)
ggsave("graphs/draft5/S1_climate-boxplots.pdf", width = 6, height = 3)




