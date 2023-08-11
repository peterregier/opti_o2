## This script makes Figure 5, showing examples of how rising water levels may
## create more oxygenation events
##
## Peter Regier
## 2022-12-22
##
# ########## #
# ########## #

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(tidyverse, cowplot, plotly)

## Scenario depth increases
depth_threshold = 225
rcp45 = 15.24
rcp85 = 24.384

## Set scenario colors
rcp45_color = "purple"
rcp85_color = "red"

## Set ggplot theme
theme_set(theme_bw())


# 2. Load data -----------------------------------------------------------------

## First, read in data
df_raw <- read_csv("data/230411_data_with_features.csv") %>% 
  group_by(season) 

dfx <- df_raw %>% filter(datetime > "2020-11-01" & datetime < "2020-12-01")

p0 <- ggplot(df_raw, aes(datetime, creek_depth)) + 
  geom_line(color = "gray") + 
  geom_hline(yintercept = depth_threshold) + 
  geom_hline(yintercept = depth_threshold - rcp45, color = rcp45_color) + 
  geom_hline(yintercept = depth_threshold - rcp85, color = rcp85_color) + 
  facet_wrap(~season, scales = "free_x", nrow = 1)

p1 <- ggplot(df_raw, aes(datetime, fp_do)) + geom_line() + 
  facet_wrap(~season, scales = "free_x", nrow = 1)

plot_grid(p0, p1, ncol = 1)

df <- df_raw %>% 
  filter(datetime > "2020-05-01" & 
           datetime < "2020-05-25")
  
  
  bind_rows(df_raw %>% 
  filter(datetime > "2020-12-01" & 
           datetime < "2021-01-25"), 
  ) %>% 
  mutate(season = case_when(season == "Fall/Winter" ~ "Winter",
                            season == "Spring/Summer" ~ "Summer"))

point_size = 2
point_alpha = 0.5
  
q0 <- ggplot(df, aes(datetime, creek_depth)) + 
  geom_line() + 
  geom_point(data = df %>% filter(creek_depth > depth_threshold - rcp85), 
             color = rcp85_color, alpha = point_alpha, size = point_size) + 
  geom_point(data = df %>% filter(creek_depth > depth_threshold - rcp45), 
             color = rcp45_color, alpha = point_alpha, size = point_size) + 
  geom_point(data = df %>% filter(creek_depth > depth_threshold), 
             alpha = point_alpha, size = point_size) + 
  labs(x = "", y = "Creek depth (cm)") 

q1 <- ggplot(df, aes(datetime, fp_do)) + 
  geom_line() + 
  geom_point(data = df %>% filter(creek_depth > depth_threshold - rcp85), 
             color = rcp85_color, alpha = point_alpha, size = point_size) + 
  geom_point(data = df %>% filter(creek_depth > depth_threshold - rcp45), 
             color = rcp45_color, alpha = point_alpha, size = point_size) +
  geom_point(data = df %>% filter(creek_depth > depth_threshold), 
             alpha = point_alpha, size = point_size) + 
  annotate(geom = "text", x = as.POSIXct("2020-05-20"), y = 5, label = "Current") + 
  annotate(geom = "text", x = as.POSIXct("2020-05-20"), y = 4, label = "RCP4.5", color = rcp45_color) + 
  annotate(geom = "text", x = as.POSIXct("2020-05-20"), y = 3, label = "RCP8.5", color = rcp85_color) + 
  labs(x = "", y = "Floodplain DO (mg/L)") 

figure6 <- plot_grid(q0, q1, ncol = 1, align = "hv")
ggsave("graphs/6_Figure6_flooding_ts.png", width = 7, height = 5)

tiff("graphs/final_tiffs/6_Figure6_flooding_ts.tiff", units="in", width=7, height=5, res=300)
figure6
dev.off()

