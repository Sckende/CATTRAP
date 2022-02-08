# ---------------------------------------------------- #
#### Creation and test a new mask in SECR analyses ####
# -------------------------------------------------- #
rm(list = ls())

library(sf)
library(mapview)
library(secr)
library(sp)

###############################
# Loading the capture grid ####
###############################
grid <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_grid.txt",
                   head = T,
                   sep = "\t",
                   dec = ",")

projUTM <- "+init=epsg:32740"
grid_sp <- st_as_sf(grid,
                    coords = c("X", "Y"),
                    crs = projUTM)

mapview(grid_sp,
        col.regions = "red",
        cex = 2)

######################################################
# Estimation of the buffer size with secr package ####
######################################################

# -----> Capture file
capt <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/GLOBAL_option-ABANDONNED/capture_GLOBAL_option.txt"

# -----> Trap file
trap <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/GLOBAL_option-ABANDONNED/trap_GLOBAL_option5.txt"

# -----> Data check
count.fields(capt)
count.fields(trap)

# -----> SECR file creation
CM <- secr::read.capthist(capt,
                          trap,
                          fmt = "trapID",
                          covnames = "HABITAT",
                          detector = "count",
                          noccasions = 63)
summary(CM)

# -----> Estimated sigma
est_sig <- RPSV(CM,
                CC = TRUE)
buffer <- ceiling(est_sig * 4)
buffer # rounded to 3,000 m

####################
# Mask creation ####
####################
# it's not necessary to add mask covariates
# (trails vs vegetation cover for each pixels)
# since mask covariates are only used for modelling density surfaces (D),
# not for modelling parameters such as g0, and sigma

# -----> Visualization of trap position with 3000 m buffer
# cf the thumb rule in SECR package for the choice of the buffer size
grid_buffer <- st_union(st_buffer(grid_sp,
                                  dist = 3000))

mapview(grid_sp,
        col.regions = "red",
        cex = 2) +
  mapview(grid_buffer)

# -----> Modification of buffer for deletion of cliff drop
crop_buf <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_Maido_partiel_limite.rds')[2,]

mapview(crop_buf)
crop_buf <- st_transform(crop_buf,
                         32740)
final_grid_buffer <- st_intersection(crop_buf,
                                     grid_buffer)
mapview(final_grid_buffer) + mapview(grid_sp)

# -----> Area of the new buffered grid
area <- st_area(final_grid_buffer) # 60548878 m2
area/1000000 # 60.54888 km2
sqrt(area/1000000) # 7.8km x 7.8km

# -----> Conversion from sf object to sp object for using in secr package
mask <- as_Spatial(final_grid_buffer)
class(mask)

# -----> Mask creation
cat_mask <- make.mask(traps(CM),
                      type = "trapbuffer",
                      buffer = 3000,
                      poly = mask)
plot(cat_mask)
plot(traps(CM))
traps(CM)

########################################################################
# Selection of the detection function - half-normal vs. hazard rate ####
########################################################################

fit0 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~1),
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F) # ---> best detection function

fit00 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~1),
                       mask = cat_mask,
                       detectfn = 0, # half-normal
                       CL = F,
                       verify = F)

AIC(fit0, fit00)

#                      model    detectfn npar    logLik     AIC    AICc dAICc AICcwt
# fit0  D~1 g0~1 sigma~1 z~1 hazard rate    4 -146.4083 300.817 308.817 0.000 0.7342
# fit00     D~1 g0~1 sigma~1  halfnormal    3 -150.4243 306.849 310.849 2.032 0.2658

###################################################
# Covariate 'HABITAT' (cloded vs. open) effect ####
###################################################

fit1 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~1),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)

fit2 <- secr::secr.fit(CM,
                       model = list(D~g, g0~1, sigma~1),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)

fit3 <- secr::secr.fit(CM,
                       model = list(D~g, g0~g, sigma~1),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)
fit4 <- secr::secr.fit(CM,
                       model = list(D~g, g0~g, sigma~g),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)
fit5 <- secr::secr.fit(CM,
                       model = list(D~1, g0~g, sigma~g),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)
fit6 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~g),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)
fit7 <- secr::secr.fit(CM,
                       model = list(D~1, g0~g, sigma~1),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)
fit8 <- secr::secr.fit(CM,
                       model = list(D~g, g0~1, sigma~g),
                       groups = "HABITAT",
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)


AIC(fit1,
    fit2,
    fit3,
    fit4,
    fit5,
    fit6,
    fit7,
    fit8)

#                     model    detectfn npar    logLik     AIC    AICc  dAICc AICcwt
# fit6 D~1 g0~1 sigma~g z~1 hazard rate    5 -312.8223 635.645 650.645  0.000 0.5362
# fit1 D~1 g0~1 sigma~1 z~1 hazard rate    4 -317.5142 643.028 651.028  0.383 0.4428
# fit7 D~1 g0~g sigma~1 z~1 hazard rate    5 -316.3982 642.796 657.796  7.151 0.0150
# fit2 D~g g0~1 sigma~1 z~1 hazard rate    5 -317.3129 644.626 659.626  8.981 0.0060
# fit8 D~g g0~1 sigma~g z~1 hazard rate    6 -312.5549 637.110 665.110 14.465 0.0000
# fit5 D~1 g0~g sigma~g z~1 hazard rate    6 -312.6700 637.340 665.340 14.695 0.0000
# fit3 D~g g0~g sigma~1 z~1 hazard rate    6 -316.3859 644.772 672.772 22.127 0.0000
# fit4 D~g g0~g sigma~g z~1 hazard rate    7 -312.3937 638.787 694.787 44.142 0.0000

summary(fit6)

# $versiontime
# [1] "4.4.5, run 14:33:11 08 févr. 2022, elapsed 525.53 s"

# $traps
#  Detector Number  Spacing UsagePct
#     count     41 126.6491 38.28881

# $capthist
#  Occasions Detections    Animals  Detectors      Moves
#         63         60         10         41         33

# $groupinfo
# [1] "(CLOSED=4, OPEN=6)"

# $mask
#  Cells  Spacing     Area
#   4102 121.5469 6060.148

# $modeldetails
#     CL fixed distribution hcov
#  FALSE  none      poisson

# $AICtable
#                 model    detectfn npar    logLik     AIC    AICc
#  D~1 g0~1 sigma~g z~1 hazard rate    5 -312.8223 635.645 650.645

# $coef
#                   beta   SE.beta        lcl       ucl
# D           -6.4086414 0.3446728 -7.0841877 -5.733095
# g0          -2.7861824 0.2791255 -3.3332584 -2.239106
# sigma        6.0759326 0.2914873  5.5046281  6.647237
# sigma.gOPEN  0.7953662 0.2638065  0.2783150  1.312418
# z            1.3027007 0.1832901  0.9434586  1.661943

# $predicted
# $predicted$`session = GLOBAL, g = CLOSED`
#        link     estimate  SE.estimate          lcl          ucl
# D       log 1.647261e-03 5.850535e-04 8.382554e-04 3.237043e-03
# g0    logit 5.807544e-02 1.526892e-02 3.444769e-02 9.629328e-02
# sigma   log 4.352552e+02 1.296146e+02 2.458270e+02 7.706522e+02
# z       log 3.679220e+00 6.800684e-01 2.568851e+00 5.269538e+00

# $predicted$`session = GLOBAL, g = OPEN`
#        link     estimate  SE.estimate          lcl          ucl
# D       log 1.647261e-03 5.850535e-04 8.382554e-04 3.237043e-03
# g0    logit 5.807544e-02 1.526892e-02 3.444769e-02 9.629328e-02
# sigma   log 9.642001e+02 2.182563e+02 6.221443e+02 1.494319e+03
# z       log 3.679220e+00 6.800684e-01 2.568851e+00 5.269538e+00