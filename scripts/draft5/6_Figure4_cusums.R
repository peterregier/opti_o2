## This is a new script for calculating cusums for air temperature and creek
## depth for all three DO sites.
##
## 2022-11-23
## Peter Regier
## 
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(cowplot, #plot_grid
       tidyverse, #keep it tidy
       lubridate) #force_tz

## Set ggplot theme
theme_set(theme_bw())

tai_colors = c("#003459","#96E6B3", "#2A7F62")


# 2. Read in data --------------------------------------------------------------

df_raw <- read_csv("data/model_predictions.csv") %>% 
  mutate(season = fct_relevel(season, "Spring/Summer")) %>% 
  group_by(season) 

df <- df_raw %>% 
  select(air_temp_1d, creek_depth_1d, creek_do, seep_do, fp_do) %>% 
  pivot_longer(cols = contains("_do")) %>% 
  mutate(name = str_remove(name, "_do")) %>% 
  mutate(name = case_when(name == "creek" ~ "Creek", 
                          name == "seep" ~ "Seep", 
                          name == "fp" ~ "Floodplain"))


# 3. CLIMATE PROJECTION CONSTANTS ----------------------------------------------

## Air temperature and rainfall projections originate from 
## https://cig.uw.edu/wp-content/uploads/sites/2/2020/10/Literature_Review_FINAL_20201001.pdf
## Pgs 5 and 6 based on Mauger et al. 2015 (2071-2100 vs 1976-2005)
atemp_low = 2.8 ## An increase of 2.8C is predicted under the RCP4.5 (low) scenario
atemp_high = 4.7 ## An increase of 4.7C is predicted under the RCP8.5 (high) scenario

## Relative sea level rise projectsions originate from 
## https://wacoastalnetwork.com/research-and-tools/slr-visualization/
# The area used is WRIA 22 (Lower Chehalis)
# We used 90% likelihood projections: 0.5ft (RCP4.5), 0.8ft (RCP8.5)
depth_low = 15.24 ## An increase of 15.24 cm is predicted (RCP4.5)
depth_high = 24.38 ## An increase of 24.38 cm is predicted (RCP8.5)


# 4. Calculate cusums ----------------------------------------------------------

make_cusums <- function(driver){
  df %>%
    ungroup() %>%
    select(name, value, {{driver}}) %>%
    group_by(name) %>% 
    arrange(., {{driver}}) %>% ## organize dataset by driver in ascending order
    mutate(normalized_do = (value - mean(value)) / sd(value)) %>% ## Normalized DO
    mutate(cusum_do = cumsum(normalized_do))
}

atemp_cusum <- make_cusums(air_temp_1d)
cdepth_cusum <- make_cusums(creek_depth_1d)

plot_cusum <- function(var, rcp45 = NULL, rcp85 = NULL){
  
  data <- make_cusums({{var}})
  
  x_median = median(data %>% ungroup() %>% pull({{var}}))
  x_mean = mean(data %>% ungroup() %>% pull({{var}}))
  
  ggplot(data, aes({{var}}, cusum_do, color = name)) + 
    geom_line(lwd = 1.5, alpha = 0.3) + 
    geom_line(lwd = 0.5) + 
    annotate(geom = "rect", xmin = x_median, xmax = x_median + rcp85,
             ymin = -Inf, ymax = Inf,
             fill = "red", alpha = 0.2) +
    annotate(geom = "rect", xmin = x_median, xmax = x_median + rcp45,
             ymin = -Inf, ymax = Inf,
             fill = "blue", alpha = 0.2) +
    geom_vline(aes(xintercept = x_median)) + 
    ylab("Cusum Seep DO (unitless)")
}

font_size = 3
txt_offset = 1000

temp_plot <- plot_cusum(air_temp_1d, atemp_low, atemp_high) + 
  labs(x = "Air temp. (C)", color = "") + 
  scale_color_manual(values = tai_colors) + 
  annotate(geom = "text", x = 10, y = 0 + txt_offset, label = "RCP4.5", 
           angle = 90, color = "purple", size = font_size) + 
  annotate(geom = "text", x = 12, y = 0 + txt_offset, label = "RCP8.5", 
           angle = 90, color = "red", size = font_size) + 
  theme(legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.position = c(0.8, 0.8))

depth_plot <- plot_cusum(creek_depth_1d, depth_low, depth_high) + 
  labs(x ="Creek depth (cm)", color = "") + 
  scale_color_manual(values = tai_colors) + 
  annotate(geom = "text", x = 85, y = 0 - txt_offset, label = "RCP4.5", 
           angle = 90, color = "purple", size = font_size) + 
  annotate(geom = "text", x = 100, y = 0 - txt_offset, label = "RCP8.5", 
           angle = 90, color = "red", size = font_size) +
  theme(legend.background = element_blank(), 
        legend.box.background = element_blank(), 
        legend.position = c(0.8, 0.2))

plot_grid(temp_plot, depth_plot, nrow = 1, labels = c("A", "B"))
ggsave("graphs/draft5/4_Figure4_cusums.png", width = 7, height = 3)




