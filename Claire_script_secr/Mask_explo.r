# ---------------------------------------------------- #
#### Creation and test a new mask in SECR analyses ####
# -------------------------------------------------- #

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

# -----> Loading data
capt <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Ringler_script/CAM_DATA/Capture.txt"
trapfile1 <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Ringler_script/CAM_DATA/Trap_MULTI-SESSION_OPEN.txt"
trapfile2 <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Ringler_script/CAM_DATA/Trap_MULTI-SESSION_CLOSED.txt"

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
# D ~ 0.21, 0.17
# g0 ~ 0,08
# sigma ~ 820

Mcat04 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   mask = cat_mask,
                   verify = T)
# D ~ 0.19/km2
# g0 ~ 0,08
# sigma ~ 820m
# z ~ 3.88

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
# D ~ 0.19
# g0 ~ 0,10
# sigma ~ 533 (CLOSED) / 913 (OPEN)
# z = 

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
