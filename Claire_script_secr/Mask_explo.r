# ---------------------------------------------------- #
#### Creation and test a new mask in SECR analyses ####
# -------------------------------------------------- #
rm(list = ls())

library(sf)
library(mapview)

#### Exploration ####
Ha <- sf::st_read('C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/Habitats/habitat.shp')
names(Ha); unique(Ha$NOM)
plot(Ha[Ha$NOM == "Vegetation_altitude",])
Ha[Ha$NOM == '',]
Ha <- Ha[-488,]


Ha2 <- Ha[Ha$NOM == "Vegetation_altitude",]
plot(Ha2)
mapview::mapview(Ha2)


grid <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_grid.txt",
                   head = T,
                   sep = '\t',
                   dec = ',')

projUTM <- '+init=epsg:32740'
grid.sp <- st_as_sf(grid,
                    coords = c('X', 'Y'),
                    crs = projUTM)
mapview(grid.sp, col.regions = 'red', cex = 2) + mapview(Ha2)
mapview(grid.sp, col.regions = 'red', cex = 2)

class(Ha$NOM)
Ha$NOM <- as.factor(Ha$NOM)
mapview(Ha,
        zcol = 'NOM',
        burst = T,
        legend = F) + mapview(grid.sp, col.regions = 'red', cex = 2)

# -----> Visualization of trap position with 3000 m buffer - cf the thumb rule in SECR package for the choice of the buffer size 
grid_buffer <- st_union(st_buffer(grid.sp,
                                  dist = 3000))

mapview(grid.sp, col.regions = 'red', cex = 2) +
  mapview(grid_buffer)

# -----> Modification of buffer for deletion of cliff drop

library(mapedit)

# -----> Edition of a new polygon with partial cliff limitations
# bob <- editMap(mapview(grid.sp, col.regions = 'red', cex = 2) +
#           mapview(grid_buffer))$finished

# saveRDS(bob,
#         'C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_Maido_partiel_limite.rds')

# -----> Loading the new polygon
bob2 <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_Maido_partiel_limite.rds')[2,]

mapview(bob2)
st_crs(grid_buffer)
st_crs(bob2)
bob2 <- st_transform(bob2, 32740)

new_grid_buffer <- st_intersection(bob2, grid_buffer)
mapview(new_grid_buffer) + mapview(grid_buffer)

mapview(new_grid_buffer) + mapview(grid.sp)

# -----> Conversion from sf object to sp object for using in secr package
library(sp)
nbg.sp <- as_Spatial(new_grid_buffer)
class(nbg.sp)

# saveRDS(nbg.sp,
#         'C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_NEW_MASK.rds')

mapview(nbg.sp)

#### Test of the new MASK ####

library(secr)

# -----> Loading data
capt <- "C:/Users/ccjuhasz/Desktop/ex-GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Capture.txt"
trapfile1 <- "C:/Users/ccjuhasz/Desktop/ex-GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_OPEN.txt"
trapfile2 <- "C:/Users/ccjuhasz/Desktop/ex-GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_CLOSED.txt"

cat_maido <- read.capthist(capt,c(trapfile2, trapfile1),
                           fmt = "trapID",
                           #covnames=c("sex","age","group"),
                           #trapcovnames=c("device"),
                           detector = "count")
# -----> Mask definition for analysis

plot(traps(cat_maido$OPEN))

cat_mask <- make.mask(traps(cat_maido$OPEN),
                        type = 'trapbuffer',
                        buffer = 3000,
                        poly = nbg.sp)
plot(cat_mask)

# -----> Comparaison betw model with buffer and model with mask
# hazard rate dfn
MtestBuf <- secr.fit(cat_maido,
                    model = list(D~1, g0~1, sigma~1),
                    detectfn = 1, # hazard rate
                    CL = F,
                    buffer = 3000,
                    verify = F)

MtestMask <- secr.fit(cat_maido,
                     model = list(D~1, g0~1, sigma~1),
                     detectfn = 1, # hazard rate
                     CL = F,
                     mask = cat_mask,
                     verify = F)

AIC(MtestBuf, MtestMask)

# -----> All models with mask

Mcat02 <- secr.fit(cat_maido,
                   model = list(D~session, g0~1, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)
# D ~ 0.0024/ha (CLOSED), 0.0018/ha (OPEN)(*100 for conversion in km2)
# g0 ~ 0.096
# sigma ~ 848
# z ~ 3.95

Mcat04 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = T)
# D ~ 0.0021/ha = 0.21/km2
# g0 ~ 0.096
# sigma ~ 820m
# z ~ 3.94

Mcat05 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)


Mcat06 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~session, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)
# D ~ 0.0028/ha
# g0 ~ 0.094
# sigma ~ 545 (CLOSED) / 898 (OPEN)
# z ~ 3.55 

Mcat07 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~session, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat08 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~session, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat09 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~1, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat10 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~1, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat11 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~session, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat12 <- secr.fit(cat_maido,
                   model = list(D~session, g0~session, sigma~session, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat13 <- secr.fit(cat_maido,
                   model = list(D~session, g0~session, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

Mcat14 <- secr.fit(cat_maido,
                   model = list(D~session, g0~1, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = F)

AIC(Mcat02,Mcat04,Mcat05,Mcat06,Mcat07,Mcat08,Mcat09,Mcat10,Mcat11,Mcat12,Mcat13,Mcat14)

#### Model averaging ####

secr::model.average(Mcat04, Mcat06)

plot(Mcat04, xval = 0:3000)

p <- plot(Mcat06, xval = 0:3000)
plot(p$`session = CLOSED`, type = 'l', col = 'darkgreen', bty = 'n')
lines(p$`session = OPEN`, type = 'l', col = 'darkorange')

abline(v = 350, col = 'darkgrey', lty = 'dashed')
abline(v = 550, col = 'darkgrey', lty = 'dashed')

#### HR computation with model averaged estimates ####

# , , D
# 
#                estimate  SE.estimate         lcl         ucl
# session=CLOSED 0.002176766 0.0006743297 0.001202651 0.003939889
# session=OPEN   0.002176766 0.0006743297 0.001202651 0.003939889
# 
# , , g0
# 
#                estimate SE.estimate        lcl       ucl
# session=CLOSED 0.09540967  0.02361477 0.05810855 0.1527715
# session=OPEN   0.09540967  0.02361477 0.05810855 0.1527715
# 
# , , sigma
# 
#                estimate SE.estimate      lcl      ucl
# session=CLOSED 716.2983    221.6898 395.9619 1295.790
# session=OPEN   866.6064    159.7289 605.6767 1239.946
# 
# , , z
# 
#                estimate SE.estimate      lcl      ucl
# session=CLOSED 3.789854   0.6802843 2.673254 5.372849
# session=OPEN   3.789854   0.6802843 2.673254 5.372849

# ----- CLOSED AREA -----#
# Home range 95% and 50%
HR95clo <- 3.14*((circular.r(p = 0.95,
                             detectfn = 'HR', # hazard rate
                             detectpar = list(sigma = 1, z = 3.79)))*716.2983)^2
# HR95closed = 6 785 316 m2 = 6.79 km2
HR50clo <- 3.14*((circular.r(p = 0.5,
                             detectfn = 'HR', # hazard rate
                             detectpar = list(sigma = 1, z = 3.79)))*716.2983)^2
# HR50closed = 516 750.1 m2 = 0.52 km2

# ----- OPEN AREA -----#
HR95op <- 3.14*((circular.r(p = 0.95,
                            detectfn = 'HR', # hazard rate
                            detectpar = list(sigma = 1, z = 3.79)))*866.6064)^2
# HR95open = 9 931 756 m2 = 9.93 km2
HR50op <- 3.14*((circular.r(p = 0.5,
                            detectfn = 'HR', # hazard rate
                            detectpar = list(sigma = 1, z = 3.79)))*866.6064)^2
# HR50open = 756 373.8 m2 = 0.76 km2
