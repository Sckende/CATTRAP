#### GLOBAL OPTION SCRIPT ####
# To cope with the non-independance of camera-trap in each type of habitat
# 2 possibilities :
# an unique model with habitat as covariable
# two models, one per habitat
rm(list = ls())

getwd()
setwd("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr")

# Capture file
capt <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capture_GLOBAL_option.txt"
# Trap file
trap <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap_GLOBAL_option5.txt"

# Data check
count.fields(capt)
count.fields(trap)

CM <- secr::read.capthist(capt, trap, covnames = "HABITAT", detector = "count", noccasions = 63)


# fit0 <- secr::secr.fit(CM,
#                             model = list(D~1, g0~1, sigma~1),
#                             mask = NULL)

fit1 <- secr::secr.fit(CM,
                       model = list(D~1, g0~1, sigma~1),
                       groups = "HABITAT",
                       mask = NULL)

fit2 <- secr::secr.fit(CM,
                       model = list(D~g, g0~1, sigma~1),
                       groups = "HABITAT",
                       mask = NULL)

fit3 <- secr::secr.fit(CM,
                       model = list(D~g, g0~g, sigma~1),
                       groups = "HABITAT",
                       mask = NULL)
fit4 <- secr::secr.fit(CM,
                       model = list(D~g, g0~g, sigma~g),
                       groups = "HABITAT",
                       mask = NULL)
fit5 <- secr::secr.fit(CM,
                       model = list(D~1, g0~g, sigma~g),
                       groups = "HABITAT",
                       mask = NULL)
# fit6 <- secr::secr.fit(CM,
#                        model = list(D~1, g0~g, sigma~g),
#                        groups = "HABITAT",
#                        mask = NULL,
#                        buffer = 1000)

require(secr)
AIC(fit1, fit2, fit3, fit4, fit5)

summary(fit5)
