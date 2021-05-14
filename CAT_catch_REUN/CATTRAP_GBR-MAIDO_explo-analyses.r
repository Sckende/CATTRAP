#### EXPLORATION ET ANALYSES OF MAIDO/GBR CAPTURES DATA ####

rm(list = ls())

#### REQUIRED PACKAGES ####
library(sf)
# library(rgdal)
# library(raster)
library(ggplot2)

#### DATA IMPORTATION ####

# ---- Dataframes
cat <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN/CATTRAP_Bilan_2010-2020.txt', h = T, sep = '\t', dec = ',') # global cat catch data

CamTrap <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN/CATTRAP_cameratrap_GPS_2015-2016.txt', h = T, sep = '\t', dec = '.') # cat cameratrap data

# ---- Shapefiles
hab <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Habitats/habitat.shp') # shapefile for habitats
hab_color <- rainbow(n = length(unique(hab$NOM)))
crs_hab <- st_crs(hab)

site <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Code_site/Code_site.shp') # shapefile for trap areas - created and updated by Yahaia
site_color <- rainbow(n = length(unique(site$SITE)))
crs_site <- st_crs(site)

#### DATA FORMATTING ####

# ---- Conversion from .txt to simple feature spatial object
cat_sf1 <- st_as_sf(cat[!is.na(cat$lat),], coords = c('lon', 'lat'), crs = crs_hab)
cat_sf2 <- st_as_sf(cat[!is.na(cat$lat),], coords = c('lon', 'lat'), crs = crs_site)

#### DATA VISUALISATION ####

ggplot() +
  geom_sf(data = hab,
          aes(fill = NOM)) +
  scale_color_manual(values = hab_color) +
  geom_sf(data = site,
          fill = rgb(1, 1, 1, 0.5)) +
  geom_sf(data = cat_sf1)
