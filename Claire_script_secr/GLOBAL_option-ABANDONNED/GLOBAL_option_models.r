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
CM <- secr::read.capthist(capt,
                          trap,
                          covnames = "HABITAT",
                          detector = "count",
                          noccasions = 63)

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

###################################
# Group (cloded vs. open) effects #
###################################
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

summary(fit5)









