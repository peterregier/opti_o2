## This  script is the sole source for data used for the paper. It's a bit of a 
## beast because it combines a wide range of datasets, calculates a number of 
## different things, and then trims first based on Opti-O2 guidance, then based 
## on and also commented well to explain data decisions

# 1. Set up environment --------------------------------------------------------

## Load packages using the pacman library
require(pacman)
p_load(tidyverse, # tidy data handling
       lubridate,
       suncalc,
       roll, 
       parsedate) # auto-parse datetime formats

## Load constants
source("scripts/draft5/0_constants.R")

common_tz = "America/Los_Angeles"

# 2. Import and join data ------------------------------------------------------

## Import two Climavue datasets, convert datetime to dttm type, select useful
## columns, rename, and convert characters to numeric, then merge

## Dataset 1: 06/26/2019 - 06/29/2020
climate1 <- read_csv("data/draft3_data/climavue_data_5min_20190626-20200629.csv", skip=1) %>% 
  slice(3:n()) %>% mutate(datetime = lubridate::force_tz(parsedate::parse_date(TIMESTAMP), 
                                                         common_tz)) %>%
  select(datetime, Rain_mm_Tot, BP_hPa, AirT_C_Avg, SlrFD_kW_Avg) %>% 
  rename(rain_mm = Rain_mm_Tot,
         air_temp = AirT_C_Avg, 
         par = SlrFD_kW_Avg) %>% 
  mutate_if(is.character, as.numeric) %>% 
  drop_na()

## Dataset 2: 01/21/2020 - 09/24/2021
climate2 <- read_csv("data/draft3_data/climavue_data_5min_20200121-20210924.csv", skip=1) %>% 
  slice(3:n()) %>% mutate(datetime = lubridate::force_tz(parsedate::parse_date(TIMESTAMP), 
                                                         common_tz)) %>%
  select(datetime, Rain_mm_Tot, BP_hPa, AirT_C_Avg, SlrFD_kW_Avg) %>% 
  rename(rain_mm = Rain_mm_Tot,
         air_temp = AirT_C_Avg, 
         par = SlrFD_kW_Avg) %>% 
  mutate_if(is.character, as.numeric) %>% 
  drop_na()

## Bind into a single dataframe (union only keeps unique rows) and add lagged
## variables
climate <- dplyr::union(climate1, climate2) 


## Import creek Opti-O2 and Troll combined dataset: convert datetime to dttm 
## type, rename, then select useful columns
creek <- read_csv("./data/draft3_data/Beaver Creek River data 03_05_19 to 06_21_21.csv") %>% 
  mutate(datetime = lubridate::force_tz(parsedate::parse_date(`DT River`), 
                                        common_tz)) %>%
  rename(creek_temp = "T (°C) river", 
         creek_sal = "Salinity (psu) river",
         creek_do = "DO (mg/l) river", 
         creek_depth = "Water height above sensor (cm)") %>% 
  select(datetime, creek_temp, creek_sal, creek_do, creek_depth)

## Import seep Opti-O2 dataset: convert datetime to dttm type, rename, then
## select useful columns
seep <- read_csv("./data/draft3_data/Beaver Creek Seep data 03_05_19 to 02_02_21 no formulas.csv") %>% 
  mutate(datetime = lubridate::force_tz(parsedate::parse_date(`Datetime`), 
                                        common_tz), 
         seep_do = coalesce(as.numeric(`Dissolved Oxygen Concentration (mg/L)`), 
                            `DO (mg/L) with YSI Salinity`)) %>%   
  rename(seep_temp = "Calibrated Sensor Temperature (C)") %>% 
  select(datetime, seep_temp, seep_do)

## Import floodplain Opti-O2 and Troll combined dataset: convert datetime to 
## dttm type, rename, then select useful columns
floodplain <- read_csv("./data/draft3_data/Beaver Creek Well data 06_26_19 to 06_21_21.csv") %>% 
  mutate(datetime = lubridate::force_tz(parsedate::parse_date(`Datetime`), 
                                        common_tz)) %>%
  rename(fp_temp = "Calibrated Sensor Temperature (C)", 
         fp_sal = "Salinity (PSU)",
         fp_do = "Dissolved Oxygen Concentration (mg/L)", 
         fp_depth = "Calculated Depth (cm)") %>% 
  select(datetime, fp_temp, fp_sal, fp_do, fp_depth)

## Join all data into one master dataset using inner_join (only keep rows that
## have data for all columns)
df <- full_join(creek, seep, by = "datetime") %>% 
  full_join(., floodplain, by = "datetime") %>% 
  inner_join(., climate, by = "datetime") 
  

# 3. Clean data based on Opti-O2 QC --------------------------------------------

## Dates were removed for one of the following reasons given by Opti-O2 folks:
### floodplain sensors was in air
### no reliable salinity data for the creek
### no reliable salinity data for the floodplain
### sensors were being serviced
### DO optical signal indicated error
  # filter(datetime >= "2019-07-02" & datetime <= "2019-07-05" | 
  #          datetime >= "2019-07-31" & datetime <= "2019-08-02" | #26d
  #          datetime >= "2019-08-05" & datetime <= "2019-08-09" | #3d
  #          datetime >= "2019-08-28" & datetime <= "2019-09-04" | #19d
  #          datetime >= "2019-09-06" & datetime <= "2019-11-08" | #2d
  #          datetime >= "2020-01-22" & datetime <= "2020-03-01" | #2+mo
  #          datetime >= "2020-03-03" & datetime <= "2020-05-09" | #1d
  #          datetime >= "2020-05-11" & datetime <= "2020-05-26" | #2d
  #          datetime >= "2020-06-04" & datetime <= "2020-06-21" | #8d
  #          datetime >= "2020-06-24" & datetime <= "2020-06-27" | #3d
  #          datetime >= "2020-07-02" & datetime <= "2020-07-11" | #5d
  #          datetime >= "2020-07-22" & datetime <= "2020-07-24" | #11d
  #          datetime == "2020-08-03" | #10d
  #          datetime >= "2020-08-28" & datetime <= "2020-09-02" | #25d
  #          datetime >= "2020-09-18" & datetime <= "2021-02-02") %>% #16d


## Named for being the new take on the dataset (allowing some QC-sketch data in)
df_new <- inner_join(creek, seep, by = "datetime") %>% 
  inner_join(., floodplain, by = "datetime") %>% 
  inner_join(., climate, by = "datetime") %>%
  filter(datetime >= "2020-03-03" & datetime <= "2021-02-02") %>% 
  mutate(datetime = with_tz(datetime, tzone = common_tz),
         date = lubridate::date(datetime), 
         time = hms::as_hms(datetime)) %>% group_by(date) %>% 
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M:%S %Z"))) %>% 
  #filter(n() > daily_count_threshold) %>% 
  filter(!all(seep_do == 0)) %>% #removes 10/8/21
  ungroup()

## First, read in data
df_all <- df_new %>% 
  mutate(datetime = parsedate::parse_date(datetime), 
         datetime_utc = with_tz(datetime, tzone = "UTC"), 
         date_utc = as_date(datetime_utc)) %>% 
  mutate(yday = lubridate::yday(date), 
         month = lubridate::month(date)) 
         #sun_position = getSunlightPosition(date = datetime_utc,
          #                                  lat = bc_lat, 
           #                                 lon = bc_long, 
            #                                keep = "altitude") %>% pull(altitude))

## Per Opti-O2, sus dates are 3/2, 5/10, and 5/26-6/4 (12 days total, ~10%)
df_summer <- df_all %>% 
  filter(datetime_utc < as.POSIXct("2020-06-27", tz = "UTC")) %>% 
  filter(datetime_utc < as.POSIXct("2020-06-01", tz = "UTC") | 
           datetime_utc > as.POSIXct("2020-06-04", tz = "UTC")) %>% 
  mutate(season = "Spring/Summer")

## Per Opti-O2, all dates are good
df_winter <- df_all %>% 
  filter(datetime > as.POSIXct("2020-10-01", tz = "UTC") & 
           datetime < as.POSIXct("2021-02-01", tz = "UTC")) %>% 
  mutate(season = "Fall/Winter")

df <- bind_rows(df_summer, df_winter)


# 3. Write out dataset ---------------------------------------------------------

write_csv(df, "data/220929_final_dataset.csv")
