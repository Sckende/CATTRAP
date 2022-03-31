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

#### distance entre les cam et les 4 chats capturés 6 mois avant l'étude ####
grid <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_grid.txt",
                   head = T,
                   sep = '\t',
                   dec = ',')

projUTM <- '+init=epsg:32740'
grid.sp <- st_as_sf(grid,
                    coords = c('X', 'Y'),
                    crs = projUTM)
cat.sp2 <- cat.sp[year(cat.sp$date_capt) == 2015 & month(cat.sp$date_capt) %in% 9:12 & cat.sp$zone == "MAI", ]

min(st_distance(cat.sp2[1,],
            grid.sp)) # 5925.35 m
min(st_distance(cat.sp2[2,],
            grid.sp)) # 7798.253 m
min(st_distance(cat.sp2[3,],
            grid.sp)) # 6222.516 m
min(st_distance(cat.sp2[4,],
            grid.sp)) # 6188.725 m
