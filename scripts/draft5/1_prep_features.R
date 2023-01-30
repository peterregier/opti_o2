## New script, the goal is to streamline all the data prep decisions and explain 
## them, in order. Steps: 
### 1. ID two periods (done in dataset construction)
### 2. Create features for Creek (1d rolling means)
### 3. Create features for Floodplain (1d rolling means)
### 4. Create features for Climate (1 day rolling mean except rain (1/7d sums))
### 5. Create features for Time (solar altitude, time since solar noon)
### 6. Assess variables to use based on Boruta

# 1. Setup ---------------------------------------------------------------------

require(pacman)
p_load(cowplot, #plot_grid
       tidyverse, #keep it tidy
       roll,
       zoo,
       lubridate) #force_tz

df_raw <- read_csv("data/220929_final_dataset.csv")

## Make rolling mean columns for 1 and 2 day windows
roll_mean2 <- function(var_name){
  df_raw %>% 
    mutate(!!paste0({{var_name}}, "_", "1d") := 
             rollmean(x = lag(eval(parse(text=var_name)), 288), k = 288, align = "right", fill = NA), 
           !!paste0({{var_name}}, "_", "2d") := 
             rollmean(x = lag(eval(parse(text=var_name)), 288*2), k = 2*288, align = "right", fill = NA)) %>% 
    select(!!paste0({{var_name}}, "_", "1d"), 
           !!paste0({{var_name}}, "_", "2d"))
}

var_list <- c("fp_depth", "fp_sal", "fp_temp", "fp_do",
              "creek_depth", "creek_sal", "creek_temp", "creek_do", 
              "BP_hPa", "par", "air_temp")

## Create a dataframe with all the roll_mean vars
df_roll_mean <- df_raw %>% 
  bind_cols(var_list %>% #bind the vars created to the og tibble
              map(roll_mean2) %>% #map roll_mean2 to all vars in var_list
              bind_cols()) #internal binds all map()-created vars

## Rain is sum, not mean, so do manually
df_all_rolls <- df_roll_mean %>% 
  mutate(rain_mm_1d = roll_sum(rain_mm, width = 288), 
         rain_mm_2d = roll_sum(rain_mm, width = 2*288))

## Now, because we're roll_mean/roll_summing up to the first 2 days, drop the 
## first two days of each dataset
df_trimmed <- df_all_rolls %>% 
  filter(datetime > as.POSIXct("2020-03-04 23:55:00", tz = "UTC")) %>% 
  filter(datetime < as.POSIXct("2020-10-01 00:05:00", tz = "UTC") | 
           datetime >= as.POSIXct("2020-10-03 00:05:00", tz = "UTC")) 


# Add windows ------
  
## Add windows for final dataset
num_windows = 5 # number of windows

df <- df_trimmed %>% 
  group_by(season) %>% 
  mutate(window = cut(datetime, num_windows, labels = c(1:num_windows)), 
         window_c = ifelse(season == "Spring/Summer", 
                           paste0("S", window), 
                           paste0("F", window))) %>% 
  ungroup()

# Write out ----------

write_csv(df, "data/220930_data_with_features.csv")


