#### EXPLORATION ET ANALYSES OF MAIDO/GBR CAPTURES DATA ####

rm(list = ls())

#### REQUIRED PACKAGES ####
library(sf)
library(sp)
# library(rgdal)
# library(raster)
library(ggplot2)
library(dplyr)

#### DATA IMPORTATION ####

# ---- Dataframes
cat <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN/CATTRAP_Bilan_2010-2020.txt', h = T, sep = '\t', dec = ',') # global cat catch data
cat_crop <- cat[cat$lon <= max(cat$lon[cat$site_code == 'GBR'], na.rm = T) + 5, ] 

camTrap <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN/CATTRAP_cameratrap_GPS_2015-2016.txt', h = T, sep = '\t', dec = '.') # cat cameratrap data

# ---- Shapefiles
hab <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Habitats/habitat.shp') # shapefile for habitats
hab_color <- rainbow(n = length(unique(hab$NOM)))
crs_hab <- st_crs(hab)

site <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Code_site/Code_site.shp') # shapefile for trap areas - created and updated by Yahaia
site_color <- rainbow(n = length(unique(site$SITE)))

#### DATA FORMATTING ####

# ---- Conversion from .txt to simple feature spatial object
cat_sf <- st_as_sf(cat[!is.na(cat$lat),], coords = c('lon', 'lat'), crs = crs_hab)
cat_crop_sf <- st_as_sf(cat_crop[!is.na(cat_crop$lat),], coords = c('lon', 'lat'), crs = crs_hab)

camTrap_sf <- st_as_sf(camTrap, coords = c('lon', 'lat'), crs = crs_hab)

#### DATA VISUALISATION ####

# ---- All data
ggplot() +
  geom_sf(data = hab,
          aes(fill = NOM)) +
  scale_color_manual(values = hab_color) +
  geom_sf(data = site,
          fill = rgb(1, 1, 1, 0.5)) +
  geom_sf(data = cat_sf) +
  geom_sf(data = camTrap_sf,
          shape = 8,
          color = 'red')


# ---- Maido
ggplot() +
  geom_sf(data = site[site$SITE == 'MDO' | site$SITE == 'GBR',],
          aes(fill = SITE)) +
  scale_color_manual(values = c(rgb(1, 1, 1, 0.5), rgb(1, 1, 0, 0.5))) +
  geom_sf(data = cat_crop_sf) +
  geom_sf(data = camTrap_sf,
          shape = 8,
          color = 'red')

#### DATA EXTRACTION FOR TRAPS IN MAIDO ####
#### WARNING - NEED TO BE UPDATED WITH NEW SITE SHAPEFILE ####
# Several capture points outside of the Maido polygon

# ---- Creation of MAIDO shapefile
MDO_shp <- site[site$SITE == 'MDO',]
MDO_shp <- sf::st_transform(MDO_shp, crs = crs_hab)
st_crs(MDO_shp) # crs conversion for intersects

# ggplot() +
#   geom_sf(data = MDO_shp) +
#   geom_sf(data = cat_crop_sf)

cat_crop_sf$id2 <- 1:length(cat_crop_sf$site)

points_list <- st_intersects(MDO_shp, cat_crop_sf) # extraction

MDO_data <- cat_crop_sf[cat_crop_sf$id2 %in% points_list[[1]], ] # new spatial datframe
# st_write(MDO_data, paste0('C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN', "/", "MDO_data.csv"), layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
unique(MDO_data$local)

# ---- Visualization 
ggplot() +
  geom_sf(data = MDO_shp) +
  geom_sf(data = cat_crop_sf) +
  geom_sf(data = MDO_data,
          color = 'green')

# ---- Exploration

summary(MDO_data)
sort(unique(MDO_data$season_year))


MDO_trap_rate <- MDO_data %>%
  dplyr::group_by(season_year) %>%
  dplyr::summarise(total_cat_trap = sum(trap_cat), total_trap_night = sum(trap_night), catch_rate = (sum(trap_cat)/sum(trap_night)*100))


ggplot(data = MDO_trap_rate, aes(x = season_year)) +
  geom_col(aes(y = total_trap_night)) +
  geom_line(aes(y = 100*catch_rate), group = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./100,
                                         name = 'cat trapped rate (/100trap-days)'))

                     