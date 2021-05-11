#### Exploration & analysis of trapped cats data ####
rm(list = ls())

cat <- read.table("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN/CATTRAP_Bilan_2010-2020.txt", h = T, dec = ",", sep = "\t")
dim(cat)
summary(cat)

# Keeping the CAGE type only
cat_trap <- cat[cat$dispo_type == "CAGE",]

#### EXPLORATION ####
for(i in c(2, 4:7)){
print(table(cat_trap[,i], useNA = "always"))
}

# ---- Exploration of captured cat number ---- 
tapply(cat_trap$trap_cat, cat_trap$season, sum)
tapply(cat_trap$trap_cat, cat_trap$season_year, sum)
tapply(cat_trap$trap_night, cat_trap$season_year, sum)


# AV2M applied the new protocol only from 2019
table(cat_trap$structure, cat_trap$season_year)

# ---- Computation of raw capture rate/season_year ----
require(dplyr)
rate <- cat_trap %>%
  dplyr::group_by(season_year) %>%
  dplyr::summarise(total_cat_trap = sum(trap_cat), total_trap_night = sum(trap_night), catch_rate = (sum(trap_cat)/sum(trap_night)*100))

barplot(rate$catch_rate, bty = "n", col = rainbow(length(rate$season_year)), names.arg = rate$season_year)

# For the 2010-2015 period
period <- rate$season_year[1:5]

barplot(rate$catch_rate[rate$season_year %in% period], bty = "n", col = rgb(1, 0, 0, 0.6), names.arg = rate$season_year[rate$season_year %in% period])
par(new = T)
barplot(rate$total_trap_night[rate$season_year %in% period], bty = "n", col = rgb(0, 0, 1, 0.4), xaxt = 'n', yaxt = 'n')
axis(side = 4)

rate[rate$season_year %in% period,]

# ---- Categorization of protocol type 'old' vs. 'new' ----
# rate$season_number <- 1:nrow(rate)
# rate$protocol <- c(rep("old", 6), rep("new", 4))
# ***WARNING*** - Be carefull because application year of new protocol was different depending on the structure


#### EXPLO ANALYSIS ####
library("lme4") # For generalised linear models
library("visreg") # Vizualisation of model effects
library("DHARMa") # For simulations
library("AICcmodavg") # For AIC comparison
library("car") # For the Anova command


m1 <- stats::glm(rate$total_cat_trap ~ rate$season_number,
                family = poisson(),
                offset = rate$total_trap_night)
summary(m1)

m2 <- stats::lm(rate$total_cat_trap ~ rate$protocol,
                offset = rate$total_trap_night)
summary(m2)

m3 <- stats::lm(rate$total_cat_trap ~ rate$season_number + rate$protocol,
                offset = rate$total_trap_night)
summary(m3)

m4 <- stats::lm(rate$total_cat_trap ~ rate$season_number + rate$season_number^2,
                offset = rate$total_trap_night)
summary(m4)

AICcmodavg::aictab(m1, m2, m3, m4)


??gam
gam1 <- mgcv::gam(rate$total_cat_trap ~ s(rate$season_number),
          family = poisson(),
          offset = log(rate$total_trap_night),
          method = "REML")
summary(gam1)
plot(gam1, page = 1, all.terms = TRUE, residuals = TRUE)



lm1 <- stats::glm(rate$total_cat_trap ~ rate$season_number,
                  family = poisson(),
                  offset = log(rate$total_trap_night))
summary(lm1)
plot(gam1, page = 1, all.terms = TRUE, residuals = TRUE)

#### GLOBAL SPATIAL VISUALISATION ####

# ---- Choosing a color range ----
rainb_col <- rainbow(n = length(unique(cat_trap$site_code)))
cat_trap$site_color <- rainb_col[as.numeric(as.factor(cat_trap$site_code))]

# ---- Data explo ----
count <- table(cat_trap$site_code)
barplot(count, col = rainb_col[as.numeric(as.factor(unique(cat_trap$site_code)))])

# ---- Data visualization ----
plot(cat_trap$lon,
     cat_trap$lat,
     pch = 20,
     col = rainb_col[as.numeric(as.factor(cat_trap$site_code))],
     cex = 2)

#legend("topright", legend = unique(cat_trap$site_code), col = rainb_col[unique(cat_trap$site_code_number)], pch = 20)

points(cat_trap$lon[cat_trap$site_code == "GBR"],
     cat_trap$lat[cat_trap$site_code == "GBR"],
     pch = 20,
     #col = rainb_col[cat_trap$site_code_number[cat_trap$site_code == "AZO"]]
     col = "black")

#### GBR SPATIAL VISUALISATION ####

# ---- Data subset ---- 
# with site_code = "GBR"
GBR_trap <- cat_trap[cat_trap$site_code == "GBR",]

# ---- Working on localities ---- 
table(GBR_trap$local, useNA = 'always')

# ----  Cleaning data ---- 
GBR_trap$local[stringr::str_detect(GBR_trap$local,
                             'Piste for') == TRUE] <- 'Piste forestiere Maido'

# ---- Different color for each locality ---- 
unique(GBR_trap$local)
rainb_colGBR <-rainbow(length(unique(GBR_trap$local)))

# ---- Visualization ---- 
plot(GBR_trap$lon,
     GBR_trap$lat,
     pch = 20,
     col = rainb_colGBR[as.numeric(as.factor(GBR_trap$local))],
     cex = 2)
points(GBR_trap$lon[GBR_trap$local == "Piste de La Glacière"],
       GBR_trap$lat[GBR_trap$local == "Piste de La Glacière"],
       pch = 20,
       col = "black") # Here problem with category of localities for each GPS points

# *** ==> necessity of delimitation with a polygon delimited the Maido area ***
# Use the shapefile !!!

#### EXTRACTION OF GRAND BENARRE DATABASE ####

# write.table(GBR_trap, 'C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Catch_rate/CATTRAP_GBR_data.txt', sep = '\t', dec = '.')

#### WORK WITH SHAPEFILES ####

# ---- Habitats ---- 
library('sf')
# Load the files
hab <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Habitats/habitat.shp')

# EXploration of METADATA of shapefile
st_geometry_type(hab) # Object type => hab is composed by 488 polygons

st_crs(hab) # CRS for Coordinate Reference System => hab is in the CRS 'RGR92 / UTM zone 40S'

st_bbox(hab) # the extent of the shapefile = geographic edge or the overall geographic coverage of the spatial object
ncol(hab)
nrow(hab)
dim(hab)

hab # => all of the metadata and attributes for the shapefile object

# Plot of the shapefile object with ggplot2
library(ggplot2)

ggplot() +
  geom_sf(data = hab,
          color = rainbow(n = length(unique(hab$NOM)))[as.numeric(as.factor(hab$NOM))],
          fill = rainbow(n = length(unique(hab$NOM)), alpha = 0.5)[as.numeric(as.factor(hab$NOM))]) +
  coord_sf()

# Conversion of .txt into shp
# Need to have lat & lon, AND DATUM & PROJECTION

CRS <- st_crs(hab) # Use the information about CRS of habitat shapefile for the conversion
trap_points <- st_as_sf(cat_trap[!is.na(cat_trap$lat),],
                        coords = c('lon', 'lat'),
                        crs = CRS) # need to have data, columns name X & Y coordinates & crs
st_crs(trap_points)

# Plot global trap points!
ggplot() +
  geom_sf(data = hab,
          color = rainbow(n = length(unique(hab$NOM)))[as.numeric(as.factor(hab$NOM))],
          fill = rainbow(n = length(unique(hab$NOM)), alpha = 0.5)[as.numeric(as.factor(hab$NOM))]) +
  geom_sf(data = trap_points) +
  coord_sf()

#### Plot GBR trap points with habitat type ####
# Coversion of .txt file into sf object
GBR_trap <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN/CATTRAP_GBR_data.txt', h = T, sep = '\t', dec = '.')
library(sf)

GBR_trapPOINTS <- st_as_sf(GBR_trap[!is.na(GBR_trap$lon),],
                           coords = c('lon', 'lat'),
                           crs = CRS)
st_crs(GBR_trapPOINTS)

library(ggplot2)
ggplot() +
  geom_sf(data = hab,
          color = rainbow(n = length(unique(hab$NOM)))[as.numeric(as.factor(hab$NOM))],
          fill = rainbow(n = length(unique(hab$NOM)), alpha = 0.5)[as.numeric(as.factor(hab$NOM))]) +
  geom_sf(data = GBR_trapPOINTS) +
  coord_sf()

# ---- Localities .... without localities ... ---- 

library('sf')
library('raster')
library('rgdal')
# Load the files
admin0 <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp')
admin1 <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm1.shp')
admin2 <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm2.shp')
raster_run <- raster('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/mnt_relief.tif',
                     crs = '+proj=utm +zone=40 +south +datum=WGS84') # Fond de carte
sites <- st_read('C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Code_site/Code_site.shp')

# EXploration of METADATA of shapefile
st_geometry_type(admin0); st_geometry_type(admin1); st_geometry_type(admin2) 

st_crs(admin0); st_crs(admin1); st_crs(admin2)

st_bbox(admin0); st_bbox(admin1); st_bbox(admin2)

# Plot of the shapefile object with ggplot2
library(ggplot2)

new_color <- rainbow(n = nrow(admin2), alpha = 0.5)
ggplot() +
  # geom_sf(data = admin0) +
  # geom_sf(data = admin1) +
  geom_sf(data = admin2,
          aes(fill = NAME_2)) +
  scale_color_manual(values = new_color) +
  geom_sf(data = GBR_trapPOINTS) +
  coord_sf()

#### EXPLORATION OF GBR DATA ####
summary(GBR_trapPOINTS)
unique(GBR_trapPOINTS$local)


# plot

new_color <- rainbow(n = length(unique(sites$SITE2)), alpha = 0.8)
ggplot() +
  geom_sf(data = sites,
          aes(fill = SITE2)) +
  scale_color_manual(values = new_color) +
  geom_sf(data = GBR_trapPOINTS) +
  coord_sf()

# Cleaning data
local <- unique(GBR_trapPOINTS$local)[2:6]
GBR_data <- GBR_trapPOINTS[!(GBR_trapPOINTS$local %in% local),]
table(GBR_data$local, useNA = "always")

# ---- Plot depending on localities ----
new_color <- rainbow(n = length(unique(GBR_data$local)), alpha = 1)
ggplot() +
  # geom_sf(data = admin2) +
  geom_sf(data = GBR_data,
          aes(color = local)) +
  scale_color_manual(values = new_color) +
  coord_sf()

# ---- Plot depending on seasons ----
new_color <- rainbow(n = length(unique(GBR_data$season_year)), alpha = 1)
ggplot() +
  # geom_sf(data = admin2) +
  geom_sf(data = GBR_data,
          size = 4,
          aes(color = season_year)) +
  scale_color_manual(values = new_color) +
  coord_sf()

# Add camera-traps of Naïs

CamTrap <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/CAT_catch_REUN/CATTRAP_cameratrap_GPS_2015-2016.txt', h = T, sep = '\t', dec = '.')
library(sf)

CamTrap <- st_as_sf(CamTrap, coords = c('lon', 'lat'), crs = CRS)
st_crs(CamTrap)

ggplot() +
  # geom_sf(data = admin2) +
  # geom_sf(data = GBR_data,
  #         size = 4,
  #         aes(color = season_year)) +
  # scale_color_manual(values = new_color) +
  geom_sf(data = CamTrap,
          #shape = 8,
          size = 3,
          aes(color = field)) +
  coord_sf()

# Distance betw points

library('nngeo')
nngeo::st_nn(CamTrap, CamTrap, k = 2, returnDist = TRUE) 

nngeo::st_nn(CamTrap[CamTrap$field == 'open',], CamTrap[CamTrap$field == 'open',], k = 2, returnDist = TRUE) 

nngeo::st_nn(CamTrap[CamTrap$field == 'closed',], CamTrap[CamTrap$field == 'closed',], k = 2, returnDist = TRUE) 
