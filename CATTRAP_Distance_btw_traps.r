library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('sp')
library('lubridate')
library('dplyr')

rm(list = ls())

# loc <- read.table('C:/Users/ccjuhasz/Desktop/CATTRAP_GENET_CAM_loc.txt',
#                   sep = '\t',
#                   dec = ',',
#                   h = T)
loc <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/DATA/CATTRAP_GENET_CAM_loc.txt',
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

# Comparaison de moyenne pour les distances entre caméras par type de milieu
?shapiro.test
shapiro.test(min_dist_op) # distribution non normale
shapiro.test(min_dist_clo) # distribution normale

?kruskal.test
kruskal.test(list(min_dist_clo,
                  min_dist_op)) # pas de différence significative
# Kruskal-Wallis rank sum test
# 
# data:  list(min_dist_clo, min_dist_op)
# Kruskal-Wallis chi-squared = 0.27242, df = 1, p-value = 0.6017

# Distance moyenne entre les caméras toutes périodes confondues
mean(c(min_dist_op,
       min_dist_op))
sd(c(min_dist_op,
       min_dist_op))
