### CMR chats Maido

# 2 sessions, 30 (open) & 33 (closed) jours, 20/21 cameras traps
# CLOSED = foret, hors sentier
# OPEN = sentiers 
#-----------------------------#
#### Multi-session models ####
#---------------------------#

rm(list = ls())

# capt <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Capture.txt"
capt <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Ringler_script/CAM_DATA/Capture.txt"


# trapfile1 <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_OPEN.txt"
trapfile1 <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Ringler_script/CAM_DATA/Trap_MULTI-SESSION_OPEN.txt"
# trapfile2 <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_CLOSED.txt"
trapfile2 <- "C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/CATTRAP/Ringler_script/CAM_DATA/Trap_MULTI-SESSION_CLOSED.txt"

Ha <- maptools::readShapeSpatial("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/Habitat")

library(secr)
#load(".RData")


### secr models
cat_maido <- read.capthist(capt,c(trapfile2, trapfile1),
                           fmt = "trapID",
                           #covnames=c("sex","age","group"),
                           #trapcovnames=c("device"),
                           detector = "count")

# cat_maido <- read.capthist(file.choose(),c(file.choose(),file.choose()),
#                            fmt = "trapID",
#                            #covnames=c("sex","age","group"),
#                            #trapcovnames=c("device"),
#                            detector = "proximity")


cat_maido <- shareFactorLevels(cat_maido)

# plot of captures with tracks
par(mfrow = c(1, 2)); plot(cat_maido, tracks = T)

# Successive trap-revealed movements
m <-unlist(moves(cat_maido))
par(mar =c(3.2,4,1,1),mgp =c(2.1,0.6,0))# reduce margins
hist(m,xlab ="Movement  m",main ="") # majority of movement is done between 0 & 1500 m
plot(ecdf(m))

#Quick and biased estimate of sigma
initialsigma <- RPSV(cat_maido,CC =TRUE)

#### Average individual movement statistics ####
secr::dbar(cat_maido) #the mean distance between consecutive capture locations, pooled over individuals (e.g. Efford 2004). moves returns the raw distances.
secr::MMDM(cat_maido) #the average maximum distance between detections of each individual i.e. the observed range length averaged over individuals (Otis et al. 1978).

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
# Mcat01 <- secr.fit(cat_maido,
#                    model = list(D~session, g0~1, sigma~1),
#                    detectfn = 0, # halfnormal
#                    CL = F,
#                    buffer = 3000,
#                    verify = F)
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
                   verify = T)
# D ~ 0.19/km2
# g0 ~ 0,08
# sigma ~ 820m
# z ~ 3.88

# Home range 95% and 50%
HR95 <- 3.14*((circular.r(p = 0.95,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.92)))*820)^2
# HR95 = 7.9 km2
HR50 <- 3.14*((circular.r(p = 0.5,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.92)))*820)^2 #sigma = le nombre de fois qu'on multiplie le sigma- circular.r permet de d?terminer un d?multiplicateur 

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
# g0 ~ 0,10
# sigma ~ 533 (CLOSED) / 913 (OPEN)
# z = 

# ----- CLOSED AREA -----#
# Home range 95% and 50%
HR95 <- 3.14*((circular.r(p = 0.95,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*533)^2
# HR95closed = 5870115
HR50 <- 3.14*((circular.r(p = 0.5,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*533)^2
# HR50closed = 283558.4

# ----- OPEN AREA -----#
HR95 <- 3.14*((circular.r(p = 0.95,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*913)^2
# HR95open = 17223988
HR50 <- 3.14*((circular.r(p = 0.5,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*913)^2
# HR50open = 832012.1

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

Mcat13 <- secr.fit(cat_maido,
                   model = list(D~session, g0~session, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

Mcat14 <- secr.fit(cat_maido,
                   model = list(D~session, g0~1, sigma~1, z~1),
                   detectfn = 1, # hazard rate
                   CL = F,
                   buffer = 3000,
                   verify = F)

AIC(Mcat04,Mcat05,Mcat06,Mcat07,Mcat08,Mcat09,Mcat10,Mcat11,Mcat12,Mcat13,Mcat14)
# best model...Mcat04 & Mcat06
# graph proba de detection vs distance au centre d'activite/domaine vital
par(mfrow = c(1, 2))
plot(Mcat04,xval=0:2000)
plot(Mcat06, xval=0:2000, col = "blue")


#### Model averaging ####

secr::model.average(Mcat04, Mcat06)

#### HR computation with model averaged estimates ####

# SIGMA
#                 estimate      lcl      ucl
# session=CLOSED 684.9466     365.5407 1283.446
# session=OPEN   876.3692     601.0967 1277.703

# ----- CLOSED AREA -----#
# Home range 95% and 50%
HR95clo <- 3.14*((circular.r(p = 0.95,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*684.9466)^2
# HR95closed = 9 694 058 m2 = 9.70 km2
HR50clo <- 3.14*((circular.r(p = 0.5,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*684.9466)^2
# HR50closed = 468 275.6 m2 = 0.50 km2

# ----- OPEN AREA -----#
HR95op <- 3.14*((circular.r(p = 0.95,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*876.3692)^2
# HR95open = 15 869 615 m2 = 15.70 km2
HR50op <- 3.14*((circular.r(p = 0.5,
                          detectfn = 'HR', # hazard rate
                          detectpar = list(sigma = 1, z = 3.52)))*876.3692)^2
# HR50open = 766 588.5 m2 = 7.70 km2