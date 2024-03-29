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
# capt <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/GLOBAL_option-ABANDONNED/capture_GLOBAL_option.txt"
capt <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/CAPTURE_CORRIGE.txt"

# -----> Trap file
# trap <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/GLOBAL_option-ABANDONNED/trap_GLOBAL_option5_with_covar.txt"
trap <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/TRAP_CORRIGE.txt"


# -----> Data check
count.fields(capt)
count.fields(trap)

# -----> SECR file creation
CM <- secr::read.capthist(capt,
                          trap,
                          fmt = "trapID",
                          trapcovnames = "hab_type",
                          detector = "count",
                          noccasions = 68,
                          verify = T)

usage(traps(CM))
summary(CM)
summary(traps(CM))
x11()
plot(CM,
     tracks = TRUE)

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
                       verify = F,
                       details = list(fastproximity = FALSE)) # ---> best detection function
# summary(fit0)
# print(fit0)
fit00 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~1),
                       mask = cat_mask,
                       detectfn = 0, # half-normal
                       CL = F,
                       verify = F,
                       details = list(fastproximity = FALSE))
summary(fit00)

AIC(fit0, fit00)

#                      model    detectfn npar    logLik     AIC    AICc dAICc AICcwt
# fit0  D~1 g0~1 sigma~1 z~1 hazard rate    4 -310.5827 629.165 637.165 0.000 0.7394
# fit00     D~1 g0~1 sigma~1  halfnormal    3 -314.6254 635.251 639.251 2.086 0.2606
# Warning message:
# In AIC.secr(fit0, fit00) : models not compatible for AIC

####################################
# Covariate 'modele_cam' effect ####
####################################

###################################################
# Covariate 'hab_type' (cloded vs. open) effect ####
###################################################
print("fit1")
fit1 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~1),
                       mask = cat_mask,
                       detectfn = 0, # half normal
                #        detectfn = 1, # hazard rate
                       CL = F,
                       verify = F,
                       details = list(fastproximity = FALSE))
summary(fit1)
print("fit2")
fit2 <- secr::secr.fit(CM,
                       model = list(D~1, g0~hab_type, sigma~1),
                       mask = cat_mask,
                       detectfn = 0, # half normal
                #        detectfn = 1, # hazard rate
                       CL = F,
                       verify = F,
                       details = list(fastproximity = FALSE))
summary(fit2)
print("fit3")
fit3 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~hab_type),
                       mask = cat_mask,
                       detectfn = 0, # half normal
                #        detectfn = 1, # hazard rate
                       CL = F,
                       verify = F,
                       details = list(fastproximity = FALSE))
summary(fit3)
print("fit4")
fit4 <- secr::secr.fit(CM,
                       model = list(D~1, g0~hab_type, sigma~hab_type),
                       mask = cat_mask,
                       detectfn = 0, # half normal
                #        detectfn = 1, # hazard rate
                       CL = F,
                       verify = F,
                       details = list(fastproximity = FALSE))
summary(fit4)
AIC(fit1,
    fit2,
    fit3,
    fit4)

# Warning message:
# In secr::secr.fit(CM, model = list(D ~ 1, g0 ~ hab_type, sigma ~  :
#   at least one variance calculation failed 
#                                   model    detectfn npar    logLik     AIC    AICc   dAICc AICcwt
# fit1               D~1 g0~1 sigma~1 z~1 hazard rate    4 -310.5827 629.165 637.165   0.000      1
# fit2        D~1 g0~hab_type sigma~1 z~1 hazard rate    6 -304.0432 620.086 648.086  10.921      0
# fit3        D~1 g0~1 sigma~hab_type z~1 hazard rate    6 -306.5353 625.071 653.071  15.906      0
# fit4 D~1 g0~hab_type sigma~hab_type z~1 hazard rate    8 -301.0773 618.155 762.155 124.990      0

summary(fit1)

# $versiontime
# [1] "4.4.5, run 10:50:52 10 mars 2022, elapsed 470.61 s"

# $traps
#  Detector Number  Spacing UsagePct
#     count     41 126.6491 38.28881

# $capthist
#  Occasions Detections    Animals  Detectors      Moves 
#         63         60         10         41         33 

# $mask
#  Cells  Spacing     Area
#   4102 121.5469 6060.148

# $modeldetails
#     CL fixed distribution hcov
#  FALSE  none      poisson

# $AICtable
#                 model    detectfn npar    logLik     AIC    AICc
#  D~1 g0~1 sigma~1 z~1 hazard rate    4 -310.5827 629.165 637.165

# $coef
#            beta   SE.beta       lcl       ucl
# D     -5.889026 0.3425030 -6.560320 -5.217733
# g0    -2.784541 0.2619698 -3.297993 -2.271090
# sigma  6.745073 0.1842882  6.383875  7.106272
# z      1.344216 0.1643313  1.022133  1.666300

# $predicted
#        link     estimate  SE.estimate          lcl          ucl
# D       log 2.769673e-03 9.771335e-04 1.415433e-03 5.419604e-03
# g0    logit 5.816528e-02 1.435125e-02 3.564012e-02 9.354575e-02
# sigma   log 8.498616e+02 1.579587e+02 5.922182e+02 1.219592e+03
# z       log 3.835180e+00 6.345192e-01 2.779116e+00 5.292549e+00

# save.image(file = "my_work_space2.RData")

# Suite au problème d'estimation de la variance, utilisation de la fonction "halfnormal"#
#########################################################################################

#                               model   detectfn npar    logLik     AIC    AICc  dAICc AICcwt
# fit1               D~1 g0~1 sigma~1 halfnormal    3 -314.6254 635.251 639.251  0.000 0.7641
# fit2        D~1 g0~hab_type sigma~1 halfnormal    5 -308.3008 626.602 641.602  2.351 0.2359
# fit3        D~1 g0~1 sigma~hab_type halfnormal    5 -312.3492 634.698 649.698 10.447 0.0000
# fit4 D~1 g0~hab_type sigma~hab_type halfnormal    7 -305.4191 624.838 680.838 41.587 0.0000


summary(fit1,
        all.levels = T)
predict(fit1,
        all.levels = T)
# $versiontime
# [1] "4.4.5, run 10:33:50 18 mars 2022, elapsed 285.91 s"

# $traps
#  Detector Number  Spacing UsagePct
#     count     41 126.6491 38.28881

# $capthist
#  Occasions Detections    Animals  Detectors      Moves
#         63         60         10         41         33 

# $mask
#  Cells  Spacing     Area
#   4102 121.5469 6060.148

# $modeldetails
#     CL fixed distribution hcov
#  FALSE  none      poisson

# $AICtable
#             model   detectfn npar    logLik     AIC    AICc
#  D~1 g0~1 sigma~1 halfnormal    3 -314.6254 635.251 639.251

# $coef
#            beta   SE.beta       lcl       ucl
# D     -6.011508 0.3355610 -6.669196 -5.353821
# g0    -2.843722 0.2824033 -3.397223 -2.290222
# sigma  6.878572 0.1047178  6.673329  7.083815

# $predicted
#        link     estimate  SE.estimate          lcl          ucl
# D       log   0.00245039 8.459543e-04 1.269419e-03 4.730045e-03
# g0    logit   0.05500672 1.467960e-02 3.238238e-02 9.193601e-02
# sigma   log 971.23864923 1.019854e+02 7.910246e+02 1.192510e+03


# D ~ 0.25/km2
# g0 ~ 0.06
# sigma ~ 971 m

# Home range 95% and 50%
HR95 <- 3.14*((circular.r(p = 0.95,
                          detectfn = 'HN', # half-normal
                          detectpar = list(sigma = 1)))*971)^2

radius95 <- circular.r(p = 0.95,
                          detectfn = 'HN', # half-normal
                          detectpar = list(sigma = 1))*971
# HR95 = 7.9 km2
HR50 <- 3.14*((circular.r(p = 0.5,
                          detectfn = 'HN', # hazard rate
                          detectpar = list(sigma = 1)))*971)^2 #sigma = le nombre de fois qu'on multiplie le sigma- circular.r permet de determiner un demultiplicateur
radius50 <- circular.r(p = 0.50,
                          detectfn = 'HN', # half-normal
                          detectpar = list(sigma = 1))*971 

summary(fit2)
# $versiontime
# [1] "4.4.5, run 10:38:36 18 mars 2022, elapsed 1059.48 s"

# $traps
#  Detector Number  Spacing UsagePct
#     count     41 126.6491 38.28881

# $capthist
#  Occasions Detections    Animals  Detectors      Moves
#         63         60         10         41         33

# $mask
#  Cells  Spacing     Area
#   4102 121.5469 6060.148

# $modeldetails
#     CL fixed distribution hcov
#  FALSE  none      poisson

# $AICtable
#                    model   detectfn npar    logLik     AIC    AICc
#  D~1 g0~hab_type sigma~1 halfnormal    5 -308.3008 626.602 641.602

# $coef
#                          beta      SE.beta         lcl        ucl
# D                  -6.0078503 3.353226e-01  -6.6650706  -5.350630
# g0                 -3.3221975 3.351079e-01  -3.9789970  -2.665398
# g0.hab_typeclose" -16.3090336 2.103556e-07 -16.3090340 -16.309033
# g0.hab_typeopen     0.8743071 2.891371e-01   0.3076088   1.441005
# sigma               6.8720852 1.043943e-01   6.6674762   7.076694

# $predicted
#        link     estimate  SE.estimate          lcl          ucl
# D       log 2.459369e-03 8.484166e-04 1.274667e-03 4.745161e-03
# g0    logit 3.481748e-02 1.126138e-02 1.836096e-02 6.504628e-02
# sigma   log 9.649586e+02 1.010112e+02 7.864083e+02 1.184048e+03


# save.image(file = "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/RESULTS/my_work_space3.RData")

##### Distance maximale parcourue ####
######################################
MMDM(CM,
     min.recapt = 1,
     full = TRUE,
     userdist = NULL,
     mask = cat_mask)

#### Plot of the modele ####
############################
load("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/RESULTS/my_work_space3.RData")
plot(fit1,
     limits = F,
     xval = 0:3500)

# Trend in the detection probability with the distance (d)

g0 <- 0.05500672
d <- 0:3500
sigma <- 971.23864923

# Half-normal detection function
y <- g0 * exp(- (d^2) / (2 * sigma^2))

Est <- data.frame(dist = d, y = y) 
head(Est)

plot(Est$dist,
     Est$y,
     type = 'l',
     col = 'darkgreen',
     bty = 'n',
     xpd = T,
     yaxt = 'n',
     ylab = '',
     xlab = 'Distance (m)',
     cex.lab = 1.5,
     cex.axis = 1.5)

text(x = 100,
     y = 0.065,
     'Detection probability',
     xpd = T,
     cex = 1.5)

axis(2,
     seq(0, 0.06, 0.01),
     las = 2,
     xpd = T,
     cex.axis = 1.5,
     cex.lab = 1.5)
# Add a mark for sigma value
y = Est$y[Est$dist == 971]
arrows(x0 = 971,
       y0 = y + 0.01,
       x1 = 971,
       y1 = y - 0.01,
       code = 3,
       length = 0,
       col = "pink",
       lwd = 3)
# Valeur de sigma pour une diminution de la proba max de capture de 10%
p.max <- max(Est$y)
p.max10 <- p.max - ((p.max*10)/100)
p.max5 <- p.max - ((p.max*5)/100)
p.max1 <- p.max - ((p.max*1)/100)
# ----- #
p.max25 <- p.max - ((p.max*25)/100)
p.max50 <- p.max - ((p.max*50)/100)

# DISTANCE MAX FOR MAXIMAL DETECTION IN CLOSED AREA
d.10 <- max(Est$dist[Est$y >= p.max10]) # 445 m
d.5 <- max(Est$dist[Est$y >= p.max5]) # 311 m
d.1 <- max(Est$dist[Est$y >= p.max1]) # 137 m
# ----- #
d.25 <- max(Est$dist[Est$y >= p.max25]) # 736 m
d.50 <- max(Est$dist[Est$y >= p.max50]) # 1143 m

# Adding on the plot
abline(v = d.10, col = 'darkgreen', lty = 'dotted')
# ----- #
abline(v = d.5, col = 'darkgreen', lty = 'dashed')
# ----- #
abline(v = d.1, col = 'darkgreen', lty = 'twodash')
# ----- #
abline(v = d.25, col = '#e98f53', lty = 'twodash')
# ----- #
abline(v = d.50, col = '#c90808', lty = 'twodash')

# ----- # Sigma
abline(v = 971, col = "black")

# Figure for paper

png("G:/Mon Drive/Projet_Publis/CATTRAP/Figures/CATTRAP_Estimates_Detection_probs_GLOBAL_2.png",
res = 300,
width = 45,
height = 30,
pointsize = 12,
unit = "cm",
bg = "transparent")

# par(mar = c(5.1, 6.1, 4.1, 2.1)) # default mar = c(5.1, 4.1, 4.1, 2.1)
plot(Est$dist,
     Est$y,
     lwd = 3,
     type = 'l',
     col = 'black',
     bty = 'n',
     xpd = T,
     yaxt = 'n',
     ylab = '',
     xlab = 'Distance (m)',
     cex.lab = 1.5,
     cex.axis = 1.5)

text(x = 100,
     y = 0.065,
     'Detection probability',
     xpd = T,
     cex = 1.5)

axis(2,
     seq(0, 0.06, 0.01),
     las = 2,
     xpd = T,
     cex.axis = 1.5,
     cex.lab = 1.5)

# abline(v = d.1, col = 'darkgreen', lty = 'twodash', lwd = 3)
# # ----- #
# abline(v = d.25, col = '#e98f53', lty = 'dashed', lwd = 3)
# # ----- #
# abline(v = d.50, col = '#c90808', lty = 'dotted', lwd = 3)
# # ----- #
# abline(v = 971, col = "black")


# legend(x = 2000,
#        y = 0.055,
#        legend = c('1 %', '25 %', '50 %', 'sigma'),
#        lty = c('twodash', 'dashed', 'dotted', 'solid'),
#        col = c('darkgreen', '#e98f53', '#c90808', 'black'),
#        lwd = 3,
#        bty = 'n',
#        cex = 2,
#        title = 'Treshold of detection probability')

# Add a mark for sigma value
y = Est$y[Est$dist == 971]
arrows(x0 = 971,
       y0 = y + 0.01,
       x1 = 971,
       y1 = y - 0.01,
       code = 3,
       length = 0,
       col = "darkgrey",
       lwd = 3)
dev.off()

###################################################
# Time effect on the sigma for the best modele ####
###################################################

time1 <- secr::secr.fit(CM,
                        model = list(D ~ 1, g0 ~ 1, sigma ~ 1),
                        mask = cat_mask,
                        detectfn = 0, # half normal
                        CL = F,
                        verify = F,
                        details = list(fastproximity = TRUE))
summary(time1)

time2 <- secr::secr.fit(CM,
                        model = list(D ~ 1, g0 ~ T, sigma ~ 1), # T = linear trend over occasions on link scale
                        mask = cat_mask,
                        detectfn = 0, # half normal
                        CL = F,
                        verify = F,
                        details = list(fastproximity = TRUE))
summary(time2)

AIC(time1,
    time2)

#                  model   detectfn npar    logLik     AIC    AICc   dAICc AICcwt
# time1 D~1 g0~1 sigma~1 halfnormal    3 -150.4243 306.849 310.849   0.000      1
# time2 D~1 g0~T sigma~1 halfnormal    4 -307.5565 623.113 631.113 320.264      0


############################
# Test of camera types ####
##########################
# -----> Capture file
capt <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/CAPTURE_CORRIGE.txt"

# -----> Trap file
# trap <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/GLOBAL_option-ABANDONNED/trap_GLOBAL_option5_with_covar.txt"
trap <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/TRAP_CORRIGE_CAM_test.txt"


# -----> Data check
count.fields(capt)
count.fields(trap)

# -----> SECR file creation
CM2 <- secr::read.capthist(capt,
                          trap,
                          fmt = "trapID",
                          trapcovnames = "model_cam",
                          detector = "count",
                          noccasions = 68,
                          verify = T)

cam1 <- secr::secr.fit(CM2,
                        model = list(D~1, g0~1, sigma~1),
                        mask = cat_mask,
                        detectfn = 0, # half normal
                        CL = F,
                        verify = F,
                        details = list(fastproximity = TRUE))
summary(cam1)

cam2 <- secr::secr.fit(CM2,
                        model = list(D~1, g0~model_cam, sigma~1),
                        mask = cat_mask,
                        detectfn = 0, # half normal
                        CL = F,
                        verify = F,
                        details = list(fastproximity = TRUE))
summary(cam2)

AIC(cam1,
    cam2)

#                         model   detectfn npar    logLik     AIC    AICc  dAICc AICcwt
# cam1         D~1 g0~1 sigma~1 halfnormal    3 -150.4243 306.849 310.849  0.000      1
# cam2 D~1 g0~model_cam sigma~1 halfnormal    6 -143.8348 299.670 327.670 16.821      0