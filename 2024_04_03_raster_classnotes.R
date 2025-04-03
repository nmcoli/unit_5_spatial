# NMC 2025-04-03
# 5.1 Raster Lecture


#raster, mapdata and marmap, and next week we'll be using the sf

#install.packages("raster")
#install.packages("mapdata")
#install.packages("marmap")

library(tidyverse)
library(ggplot2)
library(raster)
library(mapdata)
library(marmap)

# size of grid cell (resolution) is important
# earth is an ellipsoid, thus 3-D on 2-D space, keep in mind your Coordinated Refernece System

# multi band raster: create a raster stack or raster brick, why? want to match up multiple "variables" along the same pixel.

# Satellite Remote Sensing Data: 
# Data Product Levels:
# L0: Raw data, no geolocation
# L1: Remote Sensing Reluctance- the light in a given frequency band geolocated to the Earth's surface, after accounting for sensor corrections. 
# L2: geophysical products- 
# L3: Binned/mapped derived products- most refined.

## Finish taking notes ##

# Collecting light over a narrow area of earth's surface or a larger range of earth's surface


# Data NASA Ocean Biology Processing Group (OBPG)

# `Level-3 Mapped > Aqua-MODIS > 2002 > 01 Jul 2002 (182)`

# There you'll see a list of files with names such as 
# AQUA_MODIS.20020701_20230731.L3m.MC.CHL.chlor_a.9km.nc

# Let's dismantle the file naming convention here: 
  
# `AQUA_MODIS`: Satellite sensor (A for the Modis Aqua mission)
#  `20020701`: start date in the format YYYYMMDD, so July 1, 2002 (Julian date 182)
#  `20230731`: end date in the format YYYYMMDD, so July 31, 2023
#  `.L3m`: Level 3 mapped 
#  `MC`: Monthly Climatology
#  `CHL`: Chlorophyll suite of products
#  `chlor_a`: The specific chlorophyll product (distinct from chlor_ocx which is a legacy algorithm)
#  `9km`: resolution; the length of the side of one grid cell
#  `.nc`: netCDF file type

chl_raster = raster('data/AQUA_MODIS.20020701_20230731.L3m.MC.CHL.chlor_a.9km.nc')
chl_raster
class(chl_raster) #raster layer of class raster

names(chl_raster) = "chl_a" #renamed the name of the raster for ease

chl_pts = raster::rasterToPoints(chl_raster, spatial = TRUE) # converts raster into points, reminds it that it's geolocated to data on the Earth.
class(chl_pts)

chl_df = data.frame(chl_pts) #df represents data frame
summary(chl_df)
head(chl_df)

hist(chl_df$chl_a) # here we see lots of zeros, we may want to transform it first b/c there was a heavy right tail
hist(log10(chl_df$chl_a)) # now it looks much better. 

# Color Scales will be important in this Unit

# small amounts look dark, large amounts look red
x = c(1,2,3)
x[-1]

cols = rainbow(7, rev = TRUE)[-1]

# data is the data frame version of our raster data
global_chl_map = ggplot() + 
  geom_raster(data = chl_df, aes(x = x, y = y, fill = log10(chl_a)))+
  scale_fill_gradientn(colors = cols, 
                       limits = c(-1.5, 0.75),
                       oob = scales :: squish, #chl values outside of bounds gets squished (squish function from the scales package of tidy verse)
                       name = "log10(chl_a)") +
  ggtitle("Chl a July Climatology") +
  theme_classic()
global_chl_map

#ggsave(global_chl_map, filename = "figures/chl_a_july_climatology.pdf"), 
#device = pdf, 
  

lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds,lat_bounds)))
chl_GOM_raster

chl_GOM_df = data.frame(rasterToPoints(chl_GOM_raster, spatial = T))
head(chl_GOM_df)
world_map = map_data("worldHires")

GOM_chl_map = ggplot() + 
  geom_raster(data = chl_GOM_df, aes(x=x, y=y, fill = log10(chl_a)))+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill ="black")+
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand = FALSE) +
  scale_fill_gradientn(colors = cols, limits = c(-1,1.75))+
  theme_bw()
GOM_chl_map

batth_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], 
                                    lon2 = lon_bounds[2], 
                                    lat1 = lat_bounds[1], 
                                    lat2 = lat_bounds[2],
                                    resolution = 4) # 4 arcminutes
