### CMR chats Maido

# 2 sessions, 30 jours, 20/21 cameras traps
# CLOSED = foret, hos sentier
# OPEN = sentiers 
#-----------------------------#
#### Multi-session models ####
#---------------------------#

rm(list = ls())

capt <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Capture.txt"


trapfile1 <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_OPEN.txt"
trapfile2 <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_CLOSED.txt"

Ha <- maptools::readShapeSpatial("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/Habitat")

library(secr)
#load(".RData")


### secr models
cat_maido <- read.capthist(capt,c(trapfile2, trapfile1),
                           fmt = "trapID",
                           #covnames=c("sex","age","group"),
                           #trapcovnames=c("device"),
                           detector = "proximity")

# cat_maido <- read.capthist(file.choose(),c(file.choose(),file.choose()),
#                            fmt = "trapID",
#                            #covnames=c("sex","age","group"),
#                            #trapcovnames=c("device"),
#                            detector = "proximity")


cat_maido <- shareFactorLevels(cat_maido)

#### for comparaison btw halfnormal & hazard rate dfn ####
# halfnormal dfn
Mtest01 <- secr.fit(cat_maido,
                    model = list(D~1, g0~1, sigma~1),
                    detectfn = 0, # halfnormal
                    CL = F,
                    buffer = 3000,
                    verify = F)

# hazard rate dfn
Mtest02 <- secr.fit(cat_maido,
                    model = list(D~1, g0~1, sigma~1),
                    detectfn = 1, # hazard rate
                    CL = F,
                    buffer = 3000,
                    verify = F)

AIC(Mtest01, Mtest02)

# halfnormal dfn
Mcat01 <- secr.fit(cat_maido,
                   model = list(D~session, g0~1, sigma~1),
                   detectfn = 0, # halfnormal
                   CL = F,
                   buffer = 3000,
                   verify = F)
# D ~ 0.18, 0.15
# g0 ~ 0,07
# sigma ~ 900

# hazard dfn
# In help of the function - "the "hazard-rate" detection functions described by Hayes and Buckland (1983), are not recommended for SECR because of their long tail"
Mcat02 <- secr.fit(cat_maido,
                   model = list(D~session, g0~1, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)
# D ~ 0.21, 0.17
# g0 ~ 0,08
# sigma ~ 820

Mcat04 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)
# D ~ 0.19/km2
# g0 ~ 0,08
# sigma ~ 820m
# z ~ 3.88

# Home range 95% and 50%
HR95 <- 3.14*((circular.r(p = 0.95,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.88)))*820)^2
# HR95 = 7.9 km2
HR50 <- 3.14*((circular.r(p = 0.5,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.88)))*820)^2
# HR50 = 0.7 km2

Mcat05 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)


Mcat06 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~session, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)
# D ~ 0.19
# g0 ~ 0,08
# sigma ~ 820

Mcat07 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~session, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

Mcat08 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~session, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

Mcat09 <- secr.fit(cat_maido,
                   model = list(D~1, g0~1, sigma~1, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

Mcat10 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~1, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

Mcat11 <- secr.fit(cat_maido,
                   model = list(D~1, g0~session, sigma~session, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

Mcat12 <- secr.fit(cat_maido,
                   model = list(D~session, g0~session, sigma~session, z~session),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

AIC(#Mcat01,
  Mcat02,Mcat04,Mcat05,Mcat06,Mcat07,Mcat08,Mcat09,Mcat10,Mcat11,Mcat12)
# best model...Mcat04
# graph proba de detection vs distance au centre d'activité/domaine vital
plot(Mcat04,xval=0:2000)


