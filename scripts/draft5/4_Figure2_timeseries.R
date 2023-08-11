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
df <- read_csv("data/230411_data_with_features.csv") %>% 
  mutate(season = case_when(season == "Spring/Summer" ~ "Spring/Summer 2020", 
                            season == "Fall/Winter" ~ "Fall 2020 - Winter 2021"))


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

## Set up function to make time-series graphs
plot_ts <- function(var, y_lab){
  
  ## Set some constants up
  df$index <- 1:nrow(df)
  
  colors = c("#46ACC2", "#595F72")
  p_value_position = max(df %>% dplyr::select({{var}}) %>% drop_na()) * 0.9

  ## Make time-series
  ts_plot <- ggplot(df %>% mutate(season = fct_relevel(base::as.factor(season), "Spring/Summer 2020")), 
                                  aes(datetime, {{var}})) + 
    geom_line(aes(color = season), show.legend = F) + 
    #geom_smooth(method = "lm", se = F, color = "black") + 
    geom_vline(data = windows, aes(xintercept = window_start), linetype = "dashed") + 
    scale_color_manual(values = colors) +
    facet_wrap(~season,
    #facet_wrap(~fct_relevel(base::as.factor(season), "Spring/Summer"), 
               nrow = 1, scales = "free_x") + 
    labs(x = "", y = y_lab)
    # + 
    #geom_text(data = slopes, aes(x = x, y = y, label = slope))
  
  ## make boxplot
  boxplot <- ggplot(df %>% mutate(season = case_when(season == "Spring/Summer 2020" ~ "S", 
                                                     season == "Fall 2020 - Winter 2021" ~ "W"), 
                                  case = "By Season") %>% 
                      mutate(season = fct_relevel(as.factor(season), "S")), 
                    aes(season, {{var}}, fill = season)) + 
    geom_boxplot(show.legend = F, width = 0.7) + 
    labs(x = "Season", y = "") + 
    stat_compare_means(label = "p.signif", 
                       label.x = 1.5, 
                       label.y = p_value_position) + 
    scale_fill_manual(values = colors) + 
    facet_wrap(~case)
  
  plot_grid(ts_plot, boxplot, nrow = 1, rel_widths = c(1, 0.2))
}

figure2 <- plot_grid(plot_ts(seep_do, "Seep DO (mg/L)"), 
          plot_ts(air_temp, "Air Temp. (C)"), 
          plot_ts(creek_depth, "Creek depth (cm)"), 
          plot_ts(fp_depth, "FP depth (cm)"), 
          ncol = 1, labels = c("A", "B", "C", "D"))
ggsave("graphs/2_Figure2_ts.png", width = 10, height = 8)

tiff("graphs/final_tiffs/2_Figure2-TS.tiff", units="in", width=10, height=8, res=300)
figure2
dev.off()


## Make supplemental table of means by window

## Helper function
mean_ = function(x){mean(x, na.rm = T)}

means_by_window <- df %>% 
  group_by(window_c) %>% 
  summarize(season = first(season),
            seep_do = mean_(seep_do), 
            air_temp = mean_(air_temp), 
            creek_depth = mean_(creek_depth), 
            fp_depth = mean_(fp_depth))
write_csv(means_by_window, "data/230418_windowed_means.csv")


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
ggsave("graphs/draft5/S5_climate-boxplots.pdf", width = 6, height = 3)




