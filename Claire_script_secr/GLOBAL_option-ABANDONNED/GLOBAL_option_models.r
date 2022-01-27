#### GLOBAL OPTION SCRIPT ####
# To cope with the non-independance of camera-trap in each type of habitat
# 2 possibilities :
# an unique model with habitat as covariable
# two models, one per habitat
rm(list = ls())

require(secr)

getwd()
# setwd("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr")

# Capture file
capt <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/GLOBAL_option-ABANDONNED/capture_GLOBAL_option.txt"
# Trap file
trap <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/GLOBAL_option-ABANDONNED/trap_GLOBAL_option5.txt"

# Data check
count.fields(capt)
count.fields(trap)

# SECR file creation
help(read.capthist)
CM <- secr::read.capthist(capt,
                          trap,
                          fmt = 'trapID',
                          covnames = "HABITAT",
                          detector = "count",
                          noccasions = 63)
summary(CM)

# DL the mask 
nbg.sp <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/CATTRAP_NEW_MASK.rds')
mapview::mapview(nbg.sp)
cat_mask <- make.mask(traps(CM),
                      type = 'trapbuffer',
                      buffer = 3000,
                      poly = nbg.sp)
plot(cat_mask)
plot(traps(CM))

#####################################################################
# Selection of the detection function - half-normal vs. hazard rate #
#####################################################################

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

#################################################
# Covariate 'HABITAT' (cloded vs. open) effects #
#################################################
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


AIC(fit1, fit2, fit3, fit4, fit5)

#                     model    detectfn npar    logLik     AIC    AICc  dAICc AICcwt
# fit1 D~1 g0~1 sigma~1 z~1 hazard rate    4 -317.5142 643.028 651.028  0.000 0.9866
# fit2 D~g g0~1 sigma~1 z~1 hazard rate    5 -317.3129 644.626 659.626  8.598 0.0134
# fit5 D~1 g0~g sigma~g z~1 hazard rate    6 -312.6700 637.340 665.340 14.312 0.0000
# fit3 D~g g0~g sigma~1 z~1 hazard rate    6 -316.3859 644.772 672.772 21.744 0.0000
# fit4 D~g g0~g sigma~g z~1 hazard rate    7 -312.3937 638.787 694.787 43.759 0.0000

summary(fit1)

# $versiontime
# [1] "4.4.5, run 14:54:23 27 janv. 2022, elapsed 412.6 s"

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
#  D~1 g0~1 sigma~1 z~1 hazard rate    4 -317.5142 643.028 651.028

# $coef
#            beta   SE.beta       lcl       ucl
# D     -6.582169 0.3425021 -7.253461 -5.910877
# g0    -2.784539 0.2619698 -3.297991 -2.271088
# sigma  6.745072 0.1842880  6.383875  7.106270
# z      1.344217 0.1643311  1.022134  1.666300

# $predicted
# $predicted$`session = GLOBAL, g = CLOSED`
#        link     estimate  SE.estimate          lcl          ucl
# D       log 1.384842e-03 4.885676e-04 7.077209e-04 2.709809e-03
# g0    logit 5.816538e-02 1.435127e-02 3.564019e-02 9.354591e-02
# sigma   log 8.498607e+02 1.579584e+02 5.922178e+02 1.219590e+03
# z       log 3.835182e+00 6.345187e-01 2.779118e+00 5.292549e+00

# $predicted$`session = GLOBAL, g = OPEN`
#        link     estimate  SE.estimate          lcl          ucl
# D       log 1.384842e-03 4.885676e-04 7.077209e-04 2.709809e-03
# g0    logit 5.816538e-02 1.435127e-02 3.564019e-02 9.354591e-02
# sigma   log 8.498607e+02 1.579584e+02 5.922178e+02 1.219590e+03
# z       log 3.835182e+00 6.345187e-01 2.779118e+00 5.292549e+00
