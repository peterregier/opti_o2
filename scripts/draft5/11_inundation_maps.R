## Potential supplemental figure used to 1) address reviewer's comment/concern that
## the site would be reclaimed by the sea by 2100, making our predictions somewhat
## spurious, and 2) to provide more quantitative information to back up our pretty
## theoretical discussion of how things may change in response to climate change
##
## 2023-04-18
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse,
       raster,
       #rgdal,
       rnaturalearth, #ne_states()
       nhdplusTools, 
       smoothr, #smooth()
       ggstar, # geom_star()
       ggspatial, #annotation_north_arrow()
       tmaptools, #crop_shape()
       ggthemes, #theme_map()
       sf)

## Set a common crs so everything is projected the same
#common_crs <- 4326
common_crs <- "+proj=longlat +datum=WGS84" #use for all spatial layers

## Color used in the map
fp_color = "#FDD979"


# 2. Calculate minimum and maximum depths --------------------------------------

creek_depths <- read_csv("data/220929_final_dataset.csv") %>% 
  dplyr::mutate(creek_depth_m = creek_depth / 100) %>% 
  summarize(mean_depth = mean(creek_depth_m), 
            min_depth = min(creek_depth_m), 
            max_depth = max(creek_depth_m))



# 3. Make the BC map (the heavy lift) ------------------------------------------

#First layer: BC watershed boundary
bc_bound_raw <- read_sf("data/gis/200910_mapv2/BC boundary/area-of-interest.shp")
bc_bound_smooth <- smoothr::smooth(bc_bound_raw, method = "ksmooth") #smooth out delineation
bc_bound <- st_transform(bc_bound_smooth, common_crs)

# #Second layer (shp): Flowlines for streams
flow <- read_sf("data/gis/DNR_watercourses/DNR_Hydrography_-_Watercourses.shp")
flow1 <- st_transform(flow, common_crs)
flow2 <- crop_shape(flow1, bc_bound, polygon=T) #Second, trim to polygon


#Third layer (raster): Digital terrain model
dem_raw <- raster::raster("data/gis/200910_mapv2/FEMA DTM/swwa_fema_2009_dtm_42.tif")
e <- extent(766000, 780000, 585000, 596000) #specific box to crop out unwanted area
dem0 <- crop(dem_raw, e) #crop that puppy
dem1 <- projectRaster(dem0, crs = common_crs) #re-project raster
dem1_mask <- raster::mask(dem1, bc_bound)
dem_df <- as.data.frame(dem1_mask, xy = T) #convert to dataframe to play nicely with ggplot
colnames(dem_df)[1] <- "long"
colnames(dem_df)[2] <- "lat"
colnames(dem_df)[3] <- "elevation"
dem_df$elevation <- dem_df$elevation*0.3048 #convert feet to meters

#Clean up NAs produced by masking rasters
dem_df_crop <- dem_df[!is.na(dem_df$elevation),]

## Time to determine the elevation of our creek sensor, and normalize the DEM
## to that: 
bc01_lat = 46.9059
bc01_long = -123.9806
interval = 0.00001

creek_elev_m <- dem_df %>% filter(long >= bc01_long - interval & 
                    long <= bc01_long + interval) %>% 
  filter(lat >= bc01_lat - interval & 
           lat <= bc01_lat + interval) %>% 
  summarize(creek_elev_m = mean(elevation)) %>% 
  pull()

## Now lets calculate some metrics. Baseline = bs, 4.5 = mod, 8.5 = ex

## Add the changes in depth predicted under 4.5 and 8.5 scenarios
mod_m = 15.24 / 100
ex_m = 24.38 / 100

bs_ll = creek_elev_m + creek_depths$min_depth 
bs_mean = creek_elev_m + creek_depths$mean_depth 
bs_hh = creek_elev_m + creek_depths$max_depth 
mod_ll = creek_elev_m + creek_depths$min_depth + mod_m
mod_mean = creek_elev_m + creek_depths$mean_depth + mod_m
mod_hh = creek_elev_m + creek_depths$max_depth + mod_m
ex_ll = creek_elev_m + creek_depths$min_depth + ex_m
ex_mean = creek_elev_m + creek_depths$mean_depth + ex_m
ex_hh = creek_elev_m + creek_depths$max_depth + ex_m


## Final step: categorize the DEM
# dem_cat <- dem_df_crop %>% 
#   mutate(cat = case_when(elevation > 19 ~ "Forest", 
#                          elevation <= 19 & elevation > 3.55 ~ "Transition",
#                          elevation <= 3.5 & elevation > 1.5 ~ "Floodplain", 
#                          elevation <= 1.5 ~ "Creek"))


create_plot <- function(metric, plot_title){
  
  ## Set threshold for coloring / area calculation
  elevation_cutoff = 15
  
  x <- dem_df_crop %>% filter(elevation >= elevation_cutoff)
  fp <- dem_df_crop %>% filter(elevation < elevation_cutoff)
  flooded <- dem_df_crop %>% filter(elevation <= metric)
  
  percent = paste0(round(nrow(flooded) / nrow(fp) * 100, 1), "%")
  
  ggplot() + 
    geom_raster(data = x, aes(x = long, y = lat), fill = "gray", alpha = 0.7) + 
    geom_raster(data = fp, aes(x = long, y = lat, fill = elevation), alpha = 0.7) + 
    geom_raster(data = flooded, aes(x = long, y = lat), fill = "blue" , alpha = 0.7) + 
    scale_fill_gradientn(colors = c("#FFFAC2", "#629F6E", "#A2D19A", "#7F583E")) + 
    theme_map() + 
    ggtitle(paste0(plot_title, " - ", percent, " of area < 15m flooded"))
}


bs_ll_plot <- create_plot(bs_ll, "Baseline (lowest low)")
bs_mean_plot <- create_plot(bs_mean, "Baseline (mean)")
bs_hh_plot <- create_plot(bs_hh, "Baseline (highest high)")
mod_ll_plot <- create_plot(mod_ll, "RCP4.5 (lowest low)")
mod_mean_plot <- create_plot(mod_mean, "RCP4.5 (mean)")
mod_hh_plot <- create_plot(mod_hh, "RCP4.5 (highest high)")
ex_ll_plot <- create_plot(ex_ll, "RCP8.5 (lowest low)")
ex_mean_plot <- create_plot(ex_mean, "RCP8.5 (mean)")
ex_hh_plot <- create_plot(ex_hh, "RCP8.5 (highest high)")

final_plot <- plot_grid(plot_grid(bs_ll_plot, bs_mean_plot, bs_hh_plot, ncol = 1), 
          plot_grid(mod_ll_plot, mod_mean_plot, mod_hh_plot, ncol = 1), 
          plot_grid(ex_ll_plot, ex_mean_plot, ex_hh_plot, ncol = 1), 
          nrow = 1)

ggsave("graphs/230418_flooding_scenarios.png", width = 14, height = 12)



