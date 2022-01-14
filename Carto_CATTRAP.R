# ---- Map of genetic prelevement ----- #
rm(list = ls())

library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('sp')
library('lubridate')
library('dplyr')
library('adehabitatHR')
library('viridis')
library('viridisLite')

cat <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/DATA/CAT_GENET_data_chat_2017.txt',
                  head = T,
                  sep = '\t',
                  dec = ',')


summary(cat)

cat <- cat[!cat$zone == 'TAM',]
cat$date_capt <- as.Date(cat$date_capt,
                           format = "%d/%m/%Y") # Date format

projUTM <- '+init=epsg:32740'

cat.sp <- st_as_sf(cat,
                   coords = c('X', 'Y'),
                   crs = projUTM)
map <- mapview(cat.sp,
               zcol = 'zone',
               cex = 3,
               legend = F)

# Visualization avec le mask utilisé lors des analyses SECR
bob2 <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_Maido_partiel_limite.rds')[2,]

MAI.cat.sp <- cat.sp[cat.sp$zone == 'MAI',]
MAI.cat.sp$year <- as.factor(year(MAI.cat.sp$date_capt))
mapview(MAI.cat.sp) + mapview(bob2)
mapview(MAI.cat.sp,
        zcol = 'year',
        burst = T) + mapview(bob2)
mapview(MAI.cat.sp[MAI.cat.sp$year == 2015,]) + mapview(bob2)

summary(cat.sp$date_capt[cat.sp$zone == 'MAI'])
date(cat.sp$date_capt[cat.sp$zone == 'MAI'])
# mapshot(map,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/DATA/map_test.png')
# 
# mapshot(map,
#         file = 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/DATA/map_test.png',
#         remove_controls = c("homeButton", "layersControl"))
