# Class 4/10

# Originally Lily's Copy; edited by Nina on 4/21


library(sf) # simple features (spatial vector data) st_read, st_transform
# library(broom) # part of tidyverse
library(tidyverse)
library(mapdata)  # map_data
library(marmap) # getNOAA.bathy()

# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html
USA_crit_hab = st_read(dsn = 'data/North_Atlantic_Right_Whale_Critical_Habitat/',layer = 'North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles
USA_crit_hab
USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) #crs="+proj=longlat +datum=WGS84")

#Load in Canadian RW critical habitat coordinates http://www.dfo-mpo.gc.ca/species-especes/profiles-profils/rightwhaleNA-baleinenoireAN-eng.html
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)

# Turn data frame into sf points, then sf polygon
CAN_crit_hab_sf = CAN_crit_hab %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326) %>% # convert to sf
  dplyr::group_by(habitat, country) %>% 
  dplyr::summarize(do_union=FALSE) %>% # collapses data into multipoint; do_union=FALSE prevents reordering points; check out ?summarise.sf
  st_cast("POLYGON") # converts btwn spatial geometries
print(CAN_crit_hab_sf) # 2 simple features, with habitat and country attributes

# Simply USA_crit_hab data frame to match CAN_crit_hab
plot(USA_crit_hab_sf$geometry[1], axes=TRUE) # GOM habitat
plot(USA_crit_hab_sf$geometry[2], axes=TRUE) # FL / GA habitat
USA_crit_hab_sf$habitat=c("GOM", "SEUS")
USA_crit_hab_sf$country="USA"
USA_crit_hab_sf = USA_crit_hab_sf %>% 
  dplyr::select(country, habitat, geometry) # drops all other variables from shapefile

# Join the USA and Canada critical habitat sf objects
crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)

lat_bounds= c(39,53)
lon_bounds= c(-72, -54)
world_map= map_data("worldHires", ylim= lat_bounds, xlim= lon_bounds)

crit_map= ggplot()+
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill = "black")+ #maps the US
  geom_sf(data = crit_hab, aes(fill=country), alpha=0.5)+ # maps the US and then also critical habitat territory
  geom_point(data = carcass, aes(x=Longitude, y=Latitude,
                                 color= Carcass.condition.at.first.observation))+ # adds the carcess points
  coord_sf(xlim = lon_bounds, ylim = lat_bounds)+ # narrows the scope of the lat and long bounds
  theme_classic()
crit_map
ggsave(plot = crit_map, filename = "figures/crit_map.pdf")

### AIS Automatic Identification System 


library(lubridate)
# using day because we filtered down to just 1 day. 
ais_day= read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais_day)
dim(ais_day)

# are there any vessels in the SE critical habitat?? and if so are the going slow enough?!
# 10 knots or less if you are 65 ft or larger. 

# shrink boundaries (lat and long)
SE_lat_bounds= c(25,34)
SE_lon_bounds= c(-82, -76)

# reload data since we change our lat and long bondaries just to make sure we are looking at the correct coastline.
world_map= map_data("worldHires", ylim= SE_lat_bounds, xlim= SE_lon_bounds)
USA_crit_hab

# plots all boats
ais_map_points= ggplot()+
  geom_polygon(data=world_map, aes(x=long, y= lat, group = group), fill= "darkgray")+
  geom_sf(data = USA_crit_hab, alpha= 0.5, fill= "pink")+
  geom_point(data = ais_day, aes(x=LON, y=LAT))+
  coord_sf(xlim = SE_lon_bounds, ylim= SE_lat_bounds)+
  theme_classic()

ais_map_points

#Now, which points fall inside the polygon for the SEUS crit hab, ie which ships intersect with RW Critical Habitat
Sys.time()
ships_RW_intersect= ais_day %>% #simple csv --> translate to simple feature geometry
  st_as_sf(coords = c("LON", "LAT"), crs= 4269) %>%
  st_intersection(USA_crit_hab %>% dplyr::select(geometry)) # which points overlap with crit habitat (a simple feature polygon, just use geometry column)
Sys.time()

# Who are we made at?
# simple filter, if your lenght is > 20 meters (60 ft), and your speed is >10 knots from the intersect data. 
law_breakers = ships_RW_intersect %>%
  filter(Length > 20, SOG > 10)

head(law_breakers)
dim(law_breakers) # 848 boats!!! ahhhhhhhhh :(
unique(law_breakers$VesselName) # now we find the names
length(unique(law_breakers$CallSign)) # how many law breakers

class(law_breakers$BaseDateTime)#oh no,convert to time down below in mutate


illegal_paths= law_breakers %>%
  mutate(date_time= lubridate::ymd_hms(BaseDateTime)) %>% #mutates date to a date
  arrange(date_time)%>% # arranges by time and group by boat to create a single line that depicts a general idea of the journey the vessel took.
  group_by(CallSign) %>%
  summarise(do_union = FALSE)%>% # change from points to multipoints (single vessle has multiple points representing its path)
  st_cast(to= "LINESTRING")%>% # 
  st_make_valid() # makes the points into a line valid (i.e. if theres boats that only pinged once there is not a second point to create a line)
  
head(illegal_paths)  

law_breaking_map= ggplot()+
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill= "darkgreen")+ # map of US
  geom_sf(data = USA_crit_hab, alpha= 0.5, fill="darkslategrey")+ # geom_sf automatically looks for the geometry column as teh aesthetic; also map of critical habitat.
  geom_sf(data=illegal_paths, aes(color= CallSign, fill=CallSign))+ # map of illegal paths by call sign
  coord_sf(xlim = SE_lon_bounds, ylim = SE_lat_bounds) # crops the image. 
law_breaking_map # ends up showing where the shipping lanes are!

illegal_path_lengths= illegal_paths %>%
  mutate(track_length_m= st_length(geometry)) # please give me the length of the line in the geometry column; gives us individual track lengths per vessel. 

head(illegal_path_lengths)
sum(illegal_path_lengths$track_length_m) # 700 km in total vessell track on one day in 2017!
