## This script makes some time-series and calculates some stats to be used to 
## contextualize and interpret model outputs
##

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(cowplot, #plot_grid
       tidyverse, #keep it tidy
       tidymodels, 
       ggpubr,
       feasts, 
       tsibble, 
       parsedate, 
       lubridate) #force_tz

## Read in constants
source("scripts/draft5/0_constants.R")

## Set ggplot theme
theme_set(theme_bw())


# 2. Read in / set up dataset --------------------------------------------------

## First, read in data
df <- read_csv("data/230411_data_with_features.csv")


# 3. Calculate some stats ------------------------------------------------------

df_stats <- df %>% 
  group_by(season, window) %>% 
  summarize(creek_depth = mean(creek_depth), 
            creek_sal = mean(creek_sal),
            fp_depth = mean(fp_depth),
            fp_sal = mean(fp_sal),
            sfd = mean(par),
            rain_mm = mean(rain_mm))


# 4. Make time-series graphs ---------------------------------------------------

## Window locations
windows <- df %>% 
  group_by(window_c) %>% 
  summarize(window_start = first(datetime))

## Make a graph for time-series
# seep_ts <- ggplot(df, aes(datetime, seep_do)) + 
#   geom_line(color = "gray") + 
#   geom_smooth(method = "lm", se = F, color = "black") + 
#   geom_point(data = df %>% filter(fp_depth < 0), aes(datetime, seep_do)) + 
#   facet_wrap(~fct_relevel(as.factor(season), "Spring/Summer"), 
#              nrow = 1, scales = "free_x") + 
#   labs(x = "", y = "Seep DO (mg/L)")

var <- "seep_do"

x <- df %>% 
  
  select(datetime, season, var)

ts_x <- tsibble(x, key = season) %>% 
  fill_gaps() %>% 
 model(classical_decomposition(seep_do)) %>% 
  components()






ggplot(df, aes(datetime, seep_do)) + 
  geom_line(aes(color = season), show.legend = F) + 
  #geom_smooth(method = "lm", se = F, color = "black") + 
  geom_vline(data = windows, aes(xintercept = window_start), linetype = "dashed") + 
  facet_wrap(~fct_relevel(as.factor(season), "Spring/Summer"), 
             nrow = 1, scales = "free_x") + 
  labs(x = "", y = "") + 
  scale_color_manual(values = colors)



## Set up function to make time-series graphs
plot_ts <- function(var, y_lab){
  
  ## Set some constants up
  df$index <- 1:nrow(df)
  
  colors = c("#595F72", "#46ACC2")
  p_value_position = max(df %>% select({{var}}) %>% drop_na()) * 0.9

  
  ## Make time-series
  ts_plot <- ggplot(df, aes(datetime, {{var}})) + 
    geom_line(aes(color = season), show.legend = F) + 
    #geom_smooth(method = "lm", se = F, color = "black") + 
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





## Make supplemental figure for boxplots that may be useful...

sfd_boxplot <- ggplot(df, aes(fct_relevel(as.factor(season), "Spring/Summer"), sfd)) + 
  geom_boxplot(fill = "gray") + 
  scale_y_continuous(trans = "sqrt") + 
  stat_compare_means(label.x = 1.5, label = "p.signif") + 
  labs(x = "", y = "Solar Flux Dens. (W/m2)")

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




