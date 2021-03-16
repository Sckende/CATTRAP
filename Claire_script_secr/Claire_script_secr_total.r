rm(list = ls())
getwd()
setwd("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr")

# Both sessions
# captfile <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capturetot.txt"
# captfile_cor <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capturetot_cor.txt"
# 
# trapfile <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/traptot.txt"
# IMPOSSIBLE TO USE DUE TO NON-INDEPENDANCE BTW SESSION

#### Import data ####
# Session "open" only
capt_open <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capture.txt"
trap_open <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap.txt"

# Session "closed" only
capt_closed <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capture1.txt"
trap_closed <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap1.txt"

# Data check
count.fields(capt_closed)
count.fields(trap_closed)
count.fields(capt_open)
count.fields(trap_open)


d_open <- secr::read.capthist(capt_open, trap_open)
summary(d_open, terse = TRUE)

#d_open2 <- secr::reduce(d_open, dropunused = FALSE) #drop repeat detections


d_closed <- secr::read.capthist(capt_closed, trap_closed)
summary(d_closed, terse = TRUE)

##### Mask and buffer adding ####
### Open session - buffer = 1000
Ha <- maptools::readShapeSpatial('Habitat')
ovtrap_open <- secr::traps(d_open)
ovmask_open <- secr::make.mask(ovtrap_open,
                               buffer = 1000,
                               type = "trapbuffer",
                               poly = Ha,
                               keep.poly = FALSE)
ovmask_open <- secr::addCovariates(ovmask_open, Ha)

### Closed session - buffer = 400
Ha <- maptools::readShapeSpatial ('Habitat')
ovtrap_closed <- secr::traps(d_closed)
ovmask_closed <- secr::make.mask(ovtrap_closed,
                           buffer = 400,
                           type = "trapbuffer",
                           poly = Ha,
                           keep.poly = FALSE)
ovmask_closed <- secr::addCovariates(ovmask_closed, Ha)

#### Models testing ####

# g0 = detection probability
# sigma = 
# D = density

#######################
#### Open session ####
#####################

# Null model
fit0_open <- secr::secr.fit(d_open,
                      mask = ovmask_open,
                      model = D ~ 1)
# Detection probability constant across animals, occasions and detectors
fit1_open <- secr::secr.fit(d_open,
                            mask = ovmask_open,
                            model = g0 ~ 1)
# Detection probability varies with individual behaviour = learning reponse (b)
fit2_open <- secr::secr.fit(d_open,
                            mask = ovmask_open,
                            model = g0 ~ b)
# learn reponse affects both g0 and sigma
fit3_open <- secr::secr.fit(d_open,
                            mask = ovmask_open,
                            model = list(g0 ~ b, sigma ~ b))
# Detection probability varies with individuals learning and time (pour savoir si les cages deviennent attractante seulement au bout de plusieurs jours après installation)
fit4_open <- secr::secr.fit(d_open,
                            mask = ovmask_open,
                            model = list(g0 ~ b + T))

##########################
#### Closed session ####
#######################
# Null model
fit0_closed <- secr::secr.fit(d_closed,
                        mask = ovmask_closed,
                        model = D ~ 1)
# Detection probability constant across animals, occasions and detectors
fit1_closed <- secr::secr.fit(d_closed,
                              mask = ovmask_closed,
                              model = g0 ~ 1)
# Detection probability varies with individual behaviour = learning reponse (b)
fit2_closed <- secr::secr.fit(d_closed,
                              mask = ovmask_closed,
                              model = g0 ~ b)
# learn reponse affects both g0 and sigma
fit3_closed <- secr::secr.fit(d_closed,
                              mask = ovmask_closed,
                              model = list(g0 ~ b, sigma ~ b))
# Detection probability varies with individuals learning and time (pour savoir si les cages deviennent attractante seulement au bout de plusieurs jours après installation)
fit4_closed <- secr::secr.fit(d_closed,
                              mask = ovmask_closed,
                              model = list(g0 ~ b + T))

#### COMPARAISON DE TOUS LES MODELES ####
AIC(fit0_open, fit1_open, fit2_open, fit3_open)
AIC(fit0_closed, fit1_closed, fit2_closed, fit3_closed)


#### Distance maximale parcourue ####
MMDM(d, min.recapt = 1, full = FALSE, userdist = NULL, mask = mask)

MMDM(d1, min.recapt = 1, full = FALSE, userdist = NULL, mask = mask)