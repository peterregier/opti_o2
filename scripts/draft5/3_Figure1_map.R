## This script creates  portions of the map figure for the Opti-O2 paper reboot.
## Based largely off of 0_make_map.R from premis_pr. 3 elements are needed:
## 1. WA map with BC location noted
## 2. BC watershed with sensor locations noted
## 3. 2x transects, one for sensor diagram, one for conceptual model
##
## 2022-10-28 (updated 2023-04-13)
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

# 2. Set up the WA map ---------------------------------------------------------

## Make a layer for the state outline
wa <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  filter(name == "Washington") %>% 
  st_transform(crs = common_crs)

## Mark the location of BC
bc_location <- tibble(lat = 46.90577, long = -123.98219) %>% 
  st_as_sf(coords = c("long", "lat"), 
           crs = common_crs)

## Make the WA map figure
ggplot() + 
  geom_sf(data = wa, fill = "gray95") + 
  geom_star(aes(x = -123.98219, y = 46.90577), 
            size = 3, fill = "red") +
  #geom_sf(data = bc_location, size = 5, color = "red") +
  theme_map()
ggsave("graphs/1_wa_map_for_Figure1.png", width = 3, height = 2)


# 2. Make the BC map (the heavy lift) ------------------------------------------

#First layer: BC watershed boundary
bc_bound_raw <- read_sf("data/gis/200910_mapv2/BC boundary/area-of-interest.shp")
bc_bound_smooth <- smoothr::smooth(bc_bound_raw, method = "ksmooth") #smooth out delineation
bc_bound <- st_transform(bc_bound_smooth, common_crs)

## Ignored because linkes are a little off of the DEM
# #Second layer: flowlines
# flowlines <- get_nhdplus(AOI = bc_bound) %>% 
#   st_transform(crs = common_crs) %>% 
#   crop_shape(bc_bound)

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


#Forth layer (raster): Hillshading
hs_raw <- raster::raster("./data/gis/200910_mapv2/FEMA DTM/swwa_fema_2009_dtm_42_hs.tif")
hs0 <- crop(hs_raw, e) #crop that puppy
hs1 <- projectRaster(hs0, crs = common_crs) #re-project raster
hs2 <- crop(hs1, extent(bc_bound)) #crop raster to BC watershed
hs2_mask <- mask(hs2, bc_bound)
hs_df <- as.data.frame(hs2_mask, xy = T) #convert to dataframe to play nicely with ggplot
colnames(hs_df)[1] <- "long"
colnames(hs_df)[2] <- "lat"
colnames(hs_df)[3] <- "slope"

#Clean up NAs produced by masking rasters
dem_df_crop <- dem_df[!is.na(dem_df$elevation),]
hs_df_crop <- hs_df[!is.na(hs_df$slope),]

## Final step: categorize the DEM
dem_cat <- dem_df_crop %>% 
  mutate(cat = case_when(elevation > 19 ~ "Forest", 
                         elevation <= 19 & elevation > 3.55 ~ "Transition",
                         elevation <= 3.5 & elevation > 1.5 ~ "Floodplain", 
                         elevation <= 1.5 ~ "Creek"))

p <- ggplot() + 
  geom_raster(data = dem_df_crop, 
              aes(x = long, y = lat, fill = elevation), alpha = 0.7) +
  geom_raster(data=hs_df_crop, 
              aes(x = long, y = lat, alpha = slope), show.legend = F) +
  geom_sf(data = flow2, color = "darkblue", lwd = 0.4, alpha = 0.4) +
  geom_sf(data = flow2 %>% filter(WC_GNIS_NM == "Beaver Creek"), color = "darkblue", lwd = 0.4, alpha = 0.6) +
  geom_sf(data = bc_bound, color = "black", fill = NA) + 
  geom_point(aes(x = -123.980369, y = 46.905669), size = 5) + 
  geom_point(aes(x = -123.980369, y = 46.905669), size = 3, color = "white") + 
  scale_fill_gradientn(colors = c("#FFFAC2", "#629F6E", "#A2D19A", "#7F583E")) + 
  scale_alpha(range = c(0, 0.5)) + theme_map()

p + 
  ggspatial::annotation_scale(
    location = "bl",
    pad_x = unit(0.8, "in"),
    bar_cols = c("black", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.8, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "grey20")
  )

ggsave("graphs/1_bc_map_for_Figure1.png", width = 6, height = 6)


# 3. Now, make the transect for the sensor and conceptual diagrams -------------

## Since this is a E-W transect, we're going to set a bounding box, then group
## by longitude and bin. First, set the bounding box. Length of the transect is
## 34.8 m, determined manually from Google Earth
bbox_bc01 <- c(-123.980423, -123.980080, 46.905655, 46.905720)

## Trim DEM to bounding box
dem_bc01 <- dem_df_crop[(dem_df_crop$long>bbox_bc01[1]&
                           dem_df_crop$long<bbox_bc01[2]&
                           dem_df_crop$lat>bbox_bc01[3]&
                           dem_df_crop$lat<bbox_bc01[4]),]

## Bin by longitude
bc01_elevation <- as_tibble(dem_bc01 %>% group_by(long) %>% 
  summarize(elev=mean(elevation))) %>% 
  mutate(distance_m = seq(from = 0, to = 26.5, by = (26.5/28))) 
## These are semi-bogus numbers, not sure why but had to divide by 37, /38 errored

## ID points for bbox
transect <- tibble(site = c("Well", "Seep", "Creek"), 
                       long = c(-123.980406, -123.980196, -123.980105), 
                       lat = c(46.905662, 46.905670,  46.905691), 
                   distance_m = c(4, 19.5, 25), 
                   elev = c(3, 2, 1.34))

## Make the plot
ggplot(bc01_elevation, aes(x = distance_m, y = elev)) + 
  geom_line(alpha = 0.6, lwd = 3, color = "#7F583E") + 
  geom_line(color = "#74160C", lwd = 1) + 
  geom_area(aes(ymin = 1), fill = "#D9B540", alpha = 0.5) +
  labs(x = "Distance (m)", y = "Elevation (m NAVD88)") + 
  scale_x_continuous(expand = c(0,0)) + 
  coord_cartesian(ylim = c(1, 3.5)) +
  theme_classic()
ggsave("graphs/230413_transect.pdf", width = 6, height = 3)




