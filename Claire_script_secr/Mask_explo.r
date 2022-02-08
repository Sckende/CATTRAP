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

# Area of the new buffered grid

area <- st_area(new_grid_buffer) # in m2
area/1000000 # in km2
sqrt(area/1000000) # 7.8km x 7.8km


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
# it's not necessary to add mask covariates (trails vs vegetation cover for each pixels)
# since mask covariates are only used for modelling density surfaces (D),
# not for modelling parameters such as g0, and sigma

plot(traps(cat_maido$OPEN))

cat_mask <- make.mask(traps(cat_maido$OPEN),
                        type = 'trapbuffer',
                        buffer = 3000,
                        poly = nbg.sp)
plot(cat_mask)

# -----> Comparaison betw model with buffer and model with mask
# hazard rate dfn
# MtestBuf <- secr.fit(cat_maido,
#                     model = list(D~1, g0~1, sigma~1),
#                     detectfn = 1, # hazard rate
#                     CL = F,
#                     buffer = 3000,
#                     verify = F)
# 
# MtestMask <- secr.fit(cat_maido,
#                      model = list(D~1, g0~1, sigma~1),
#                      detectfn = 1, # hazard rate
#                      CL = F,
#                      mask = cat_mask,
#                      verify = F)
# 
# AIC(MtestBuf, MtestMask)

# -----> Comparaison betw model with different detection function
# hazard rate dfn
Mhr <- secr.fit(cat_maido,
                    model = list(D~1, g0~1, sigma~1),
                    detectfn = 1, # hazard rate
                    CL = F,
                    mask = cat_mask,
                    verify = F) # -----> the best

Mhn <- secr.fit(cat_maido,
                     model = list(D~1, g0~1, sigma~1),
                     detectfn = 0, # halfnormal
                     CL = F,
                     mask = cat_mask,
                     verify = F)

Mhhr <- secr.fit(cat_maido,
                     model = list(D~1, g0~1, sigma~1),
                     detectfn = 15, # hazard hazard rate
                     CL = F,
                     mask = cat_mask,
                     verify = F)


Mhe <- secr.fit(cat_maido,
                     model = list(D~1, g0~1, sigma~1),
                     detectfn = 16, # hazard exponential
                     CL = F,
                     mask = cat_mask,
                     verify = F)

Me <- secr.fit(cat_maido,
                     model = list(D~1, g0~1, sigma~1),
                     detectfn = 2, # exponential
                     CL = F,
                     mask = cat_mask,
                     verify = F)

Mhhn <- secr.fit(cat_maido,
                     model = list(D~1, g0~1, sigma~1),
                     detectfn = 14, # hazard halfnormal
                     CL = F,
                     mask = cat_mask,
                     verify = F)
AIC(Mhr, Mhn, Mhhr, Mhe, Me, Mhhn) # --> deltaAIC < 2 : Mhr, puis Mhhr, puis Mhe

# -----> All models with mask

# Mcat02 <- secr.fit(cat_maido,
#                    model = list(D~session, g0~1, sigma~1, z~1),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
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

# Mcat05 <- secr.fit(cat_maido,
#                    model = list(D~1, g0~session, sigma~1, z~1),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)


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

# Mcat07 <- secr.fit(cat_maido,
#                    model = list(D~1, g0~session, sigma~session, z~1),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
# 
# Mcat08 <- secr.fit(cat_maido,
#                    model = list(D~1, g0~1, sigma~session, z~session),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
# 
# Mcat09 <- secr.fit(cat_maido,
#                    model = list(D~1, g0~1, sigma~1, z~session),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
# 
# Mcat10 <- secr.fit(cat_maido,
#                    model = list(D~1, g0~session, sigma~1, z~session),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
# 
# Mcat11 <- secr.fit(cat_maido,
#                    model = list(D~1, g0~session, sigma~session, z~session),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
# 
# Mcat12 <- secr.fit(cat_maido,
#                    model = list(D~session, g0~session, sigma~session, z~session),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
# 
# Mcat13 <- secr.fit(cat_maido,
#                    model = list(D~session, g0~session, sigma~1, z~1),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)
# 
# Mcat14 <- secr.fit(cat_maido,
#                    model = list(D~session, g0~1, sigma~1, z~1),
#                    detectfn = 1, # hazard rate
#                    CL = F,
#                    mask = cat_mask,
#                    verify = F)

AIC(Mcat02,Mcat04,Mcat05,Mcat06,Mcat07,Mcat08,Mcat09,Mcat10,Mcat11,Mcat12,Mcat13,Mcat14)

#### Model averaging ####

av <- secr::model.average(Mcat04, Mcat06)

# ---- #
pp1 <- plot(Mcat04, limits = T, xval = 0:3000)

# png("G:/Mon Drive/Projet_Publis/CATTRAP/Figures/CATTRAP_Mcat04_Detection_prob.tiff",
#     res = 300,
#     width = 45,
#     height = 30,
#     pointsize = 12,
#     unit = "cm",
#     bg = "transparent")
# 
# plot(pp1$`session = CLOSED`$x,
#      pp1$`session = CLOSED`$ucl,
#      bty = 'n',
#      type = 'l',
#      lty = 'dotted',
#      xlab = 'Distance (m)',
#      ylab = 'Detection probability',
#      lwd = 2,
#      cex.lab = 1.5,
#      cex.axis = 1.5)
# lines(pp1$`session = CLOSED`$x,
#       pp1$`session = CLOSED`$y,
#       lty = 'solid',
#       lwd = 2)
# lines(pp1$`session = CLOSED`$x,
#       pp1$`session = CLOSED`$lcl,
#       lty = 'dotted',
#       lwd = 2)
# 
# dev.off()

# ---- #
pp2 <-plot(Mcat06, limit = T, xval = 0:3000)

# png("G:/Mon Drive/Projet_Publis/CATTRAP/Figures/CATTRAP_Mcat06_BOTH_Detection_probs.tiff",
#     res = 300,
#     width = 45,
#     height = 30,
#     pointsize = 12,
#     unit = "cm",
#     bg = "transparent")
# 
# plot(pp2$`session = CLOSED`$x,
#      pp2$`session = CLOSED`$ucl,
#      bty = 'n',
#      type = 'l',
#      lty = 'dotted',
#      xlab = 'Distance (m)',
#      ylab = 'Detection probability',
#      lwd = 2,
#      cex.lab = 1.5,
#      cex.axis = 1.5)
# lines(pp2$`session = CLOSED`$x,
#       pp2$`session = CLOSED`$y,
#       lty = 'solid',
#       lwd = 2)
# lines(pp2$`session = CLOSED`$x,
#       pp2$`session = CLOSED`$lcl,
#       lty = 'dotted',
#       lwd = 2)
# 
# dev.off()
# png("G:/Mon Drive/Projet_Publis/CATTRAP/Figures/CATTRAP_Mcat06_OPEN_Detection_prob.tiff",
#     res = 300,
#     width = 45,
#     height = 30,
#     pointsize = 12,
#     unit = "cm",
#     bg = "transparent")
# 
# lines(pp2$`session = OPEN`$x,
#      pp2$`session = OPEN`$ucl,
#      bty = 'n',
#      type = 'l',
#      lty = 'dotted',
#      xlab = 'Distance (m)',
#      ylab = 'Detection probability',
#      lwd = 2,
#      cex.lab = 1.5,
#      cex.axis = 1.5,
#      col = 'darkgrey')
# lines(pp2$`session = OPEN`$x,
#       pp2$`session = OPEN`$y,
#       lty = 'solid',
#       lwd = 2,
#       col = 'darkgrey')
# lines(pp2$`session = OPEN`$x,
#       pp2$`session = OPEN`$lcl,
#       lty = 'dotted',
#       lwd = 2,
#       col = 'darkgrey')
# 
# dev.off()

# Trend in the detection probability with the distance (d)

g0 <- 0.09540968
d <- 0:3000
sigma.closed <- 716.2983
sigma.open <- 866.6064
z <- 3.789854

y.closed <- g0*(1 - exp(-(d/sigma.closed)^(-z)))
y.open <- g0*(1 - exp(-(d/sigma.open)^(-z)))

Est <- data.frame(dist = d, closed = y.closed, open = y.open) 
head(Est)

plot(Est$dist,
     Est$closed, type = 'l', col = 'darkgreen', bty = 'n', xpd = T, yaxt = 'n', ylab = '', xlab = 'Distance (m)', cex.lab = 1.5, cex.axis = 1.5)
text(x = -100, y = 0.12, 'Detection probability', xpd = T, cex = 1.5)
axis(2, seq(0, 0.11, 0.01), las = 2, xpd = T, cex.axis = 1.5, cex.lab = 1.5)
lines(Est$dist, Est$open, type = 'l', col = 'darkorange')

# Valeur de sigma pour une diminution de la proba max de capture de 10%
max(Est$closed) == max(Est$open)
p.max <- max(Est$closed)
p.max10 <- p.max - ((p.max*10)/100)
p.max5 <- p.max - ((p.max*5)/100)
p.max1 <- p.max - ((p.max*1)/100)
# ----- #
p.max25 <- p.max - ((p.max*25)/100)
p.max50 <- p.max - ((p.max*50)/100)

# DISTANCE MAX FOR MAXIMAL DETECTION IN CLOSED AREA
d.closed10 <- max(Est$dist[Est$closed >= p.max10]) # 574 m
d.closed5 <- max(Est$dist[Est$closed >= p.max5]) # 536 m
d.closed1 <- max(Est$dist[Est$closed >= p.max1]) # 478 m
# ----- #
d.closed25 <- max(Est$dist[Est$closed >= p.max25]) # 657 m
d.closed50 <- max(Est$dist[Est$closed >= p.max50]) # 789 m

# DISTANCE MAX FOR MAXIMAL DETECTION IN OPEN AREA
d.open10 <- max(Est$dist[Est$open >= p.max10]) # 695 m
d.open5 <- max(Est$dist[Est$open >= p.max5]) # 648 m
d.open1 <- max(Est$dist[Est$open >= p.max1]) # 579 m
# ----- #
d.open25 <- max(Est$dist[Est$open >= p.max25]) # 795 m
d.open50 <- max(Est$dist[Est$open >= p.max50]) # 954 m

# Adding on the plot
abline(v = d.closed10, col = 'darkgreen', lty = 'dotted')
abline(v = d.open10, col = 'darkorange', lty = 'dotted')
# ----- #
abline(v = d.closed5, col = 'darkgreen', lty = 'dashed')
abline(v = d.open5, col = 'darkorange', lty = 'dashed')
# ----- #
abline(v = d.closed1, col = 'darkgreen', lty = 'twodash')
abline(v = d.open1, col = 'darkorange', lty = 'twodash')

# Figure for paper

# png("G:/Mon Drive/Projet_Publis/CATTRAP/Figures/CATTRAP_Estimates_Detection_probs.tiff",
# res = 300,
# width = 45,
# height = 30,
# pointsize = 12,
# unit = "cm",
# bg = "transparent")

par(mar = c(5.1, 5.1, 4.1, 2.1)) # default mar = c(5.1, 4.1, 4.1, 2.1)
plot(Est$dist,
     Est$closed, type = 'l', col = 'black', bty = 'n', xpd = T, yaxt = 'n', ylab = '', xlab = 'Distance (m)', cex.lab = 2, cex.axis = 2, lwd = 3)
text(x = -25, y = 0.105, 'Detection probability', xpd = T, cex = 2)
axis(2, seq(0, 0.10, 0.01), las = 2, xpd = T, cex.axis = 2, cex.lab = 2)
lines(Est$dist, Est$open, type = 'l', col = 'darkgrey', lwd = 3)

# Adding on the plot
abline(v = d.closed50, col = 'black', lty = 'dotted', lwd = 3)
abline(v = d.open50, col = 'darkgrey', lty = 'dotted', lwd = 3)
# ----- #
abline(v = d.closed25, col = 'black', lty = 'dashed', lwd = 3)
abline(v = d.open25, col = 'darkgrey', lty = 'dashed', lwd = 3)
# ----- #
abline(v = d.closed1, col = 'black', lty = 'twodash', lwd = 3)
abline(v = d.open1, col = 'darkgrey', lty = 'twodash', lwd = 3)

legend(x = 2000,
       y = 0.10,
       legend = c('1 %', '25 %', '50 %'),
       lty = c('twodash', 'dashed', 'dotted'),
       lwd = 3,
       bty = 'n',
       cex = 2,
       title = 'Treshold of detection probability')

# dev.off()

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
