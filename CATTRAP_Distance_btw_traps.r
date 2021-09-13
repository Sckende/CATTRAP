library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('sp')
library('lubridate')
library('dplyr')

rm(list = ls())

loc <- read.table('C:/Users/ccjuhasz/Desktop/CATTRAP_GENET_CAM_loc.txt',
                  sep = '\t',
                  dec = ',',
                  h = T)
summary(loc)

# Spatial object conversion

# projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
projcrs <- CRS("+init=epsg:32740")
loc_sp <- sf::st_as_sf(loc,
                       coords = c('x', 'y'),
                       crs = projcrs)
class(loc_sp)

mapview(loc_sp,
        zcol = 'HABITAT',
        burst = T)

# Distance between points
# Under cover
closed <- loc_sp[loc_sp$HABITAT == 'CLOSED',]
clo_dist <- st_distance(closed)

min_dist_clo <- apply(clo_dist, 1, function(w) sort(w)[2])

mean(min_dist_clo); sd(min_dist_clo)

# On trails
open <- loc_sp[loc_sp$HABITAT == 'OPEN',]
op_dist <- st_distance(open)

min_dist_op <- apply(op_dist, 1, function(w) sort(w)[2])

mean(min_dist_op); sd(min_dist_op)