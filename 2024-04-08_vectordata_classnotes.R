## NMC; 2024-04-08
# 5.2 Vector Data (Whales)


library(sf)
library(tidyverse)
#library(ggplot2)
#library(raster)
library(mapdata)
library(marmap)

USA_crit_hab = st_read(dsn = "data/North_Atlantic_Right_Whale_Critical_Habitat/",
                       layer = "North_Atlantic_Right_Whale_Critical_Habitat")
head(USA_crit_hab)
st_crs(USA_crit_hab)$epsg
# WGS*84 = 4326

# want to translate form NAD83 to WGS 84
USA_crit_hab_sf = st_transform(USA_crit_hab, crs = 4326)

carcass = read.csv("data/RW_carcasses_2017.csv")

# rember we want the last coordinate to match the first coordinate to close a polygon shape

CAN_crit_hab = read.csv("data/NARW_canadian_critical_habitat_2017.csv")
head(CAN_crit_hab)

Can_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::group_by(habitat,country) %>%
  dplyr::summarize(do_union = FALSE) %>%
  st_cast(to = "POLYGON")

head(Can_crit_hab_sf)


# Bind Canada and US together, adjust USA so it mirrors Canadian attributes

head(USA_crit_hab_sf)
USA_crit_hab_sf$habitat = c("GOM, SEUS")
USA_crit_hab_sf$country = "USA"
USA_crit_hab = USA_crit_hab_sf %>%
  dplyr::select (country, habitat, geometry)

head(USA_crit_hab_sf)

crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)
head(crit_hab)

## go back and find capitalization error ##