# ---------------------------------------------------- #
#### Creation and test a new mask in SECR analyses ####
# -------------------------------------------------- #

# *** Modifications des fichiers source pour ré-orienter les analyses *** #
# Retrait de la covariable individuelle (open/close, fichier - trap_GLOBAL_option5.txt) #
# Création d'une covariable pour caractériser les traps (fichier - capture_GLOBAL_option.txt) #
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

# trap_file <- read.traps(file = trap,
#                         detector = "count",
#                         covnames = "hab_type",
#                         binary.usage = TRUE,
#                         trapID = "Detector")
# CM2 <- make.capthist(captures = capt,
#                      traps = trap_file,
#                      fmt = "trapID",
#                      noccasions = 63)
# summary(trap_file)
# -----> Data check
count.fields(capt)
count.fields(trap)

# -----> SECR file creation
CM <- secr::read.capthist(capt,
                          trap,
                          fmt = "trapID",
                          trapcovnames = "hab_type",
                          detector = "count",
                          noccasions = 63)

usage(traps(CM))
summary(CM)
summary(traps(CM))

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
# Covariate 'hab_type' (cloded vs. open) effect ####
###################################################

fit1 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~1),
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)
summary(fit1)
fit2 <- secr::secr.fit(CM,
                       model = list(D~1, g0~hab_type, sigma~1),
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)

fit3 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~hab_type),
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)
fit4 <- secr::secr.fit(CM,
                       model = list(D~1, g0~hab_type, sigma~hab_type),
                       mask = cat_mask,
                       detectfn = 1, # hazard rate
                       CL = F,
                       verify = F)

AIC(fit1,
    fit2,
    fit3,
    fit4)

#                     model    detectfn npar    logLik     AIC    AICc  dAICc AICcwt
# fit1               D~1 g0~1 sigma~1 z~1 hazard rate    4 -146.4083 300.817 308.817   0.000      1
# fit2        D~1 g0~hab_type sigma~1 z~1 hazard rate    6 -139.8154 291.631 319.631  10.814      0
# fit3        D~1 g0~1 sigma~hab_type z~1 hazard rate    6 -142.3548 296.710 324.710  15.893      0
# fit4 D~1 g0~hab_type sigma~hab_type z~1 hazard rate    8 -136.9045 289.809 433.809 124.992      0

summary(fit1)

# $versiontime
# [1] "4.4.5, run 16:29:11 09 mars 2022, elapsed 11.78 s"

# $traps
#  Detector Number  Spacing UsagePct
#     count     41 126.6491 2412.195

# $capthist
#  Occasions Detections    Animals  Detectors      Moves
#          1         60         10         41         21 

# $mask
#  Cells  Spacing     Area
#   4102 121.5469 6060.148

# $modeldetails
#     CL fixed distribution hcov
#  FALSE  none      poisson

# $AICtable
#                 model    detectfn npar    logLik     AIC    AICc
#  D~1 g0~1 sigma~1 z~1 hazard rate    4 -146.4083 300.817 308.817

# $coef
#            beta   SE.beta       lcl       ucl
# D     -5.888997 0.3425120 -6.560308 -5.217686
# g0    -2.814714 0.2536359 -3.311832 -2.317597
# sigma  6.749905 0.1827173  6.391786  7.108025
# z      1.341887 0.1642288  1.020004  1.663769

# $predicted
#        link     estimate  SE.estimate          lcl          ucl
# D       log 2.769754e-03 9.771892e-04   0.00141545 5.419857e-03
# g0    logit 5.653420e-02 1.352846e-02   0.03516752 8.967603e-02
# sigma   log 8.539779e+02 1.573480e+02 596.92166051 1.221732e+03
# z       log 3.826255e+00 6.326421e-01   2.77320624 5.279171e+00

# save.image(file = "my_work_space.RData")
