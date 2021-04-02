#-----------------------------#
#### Multi-session models ####
#---------------------------#

rm(list = ls())
# captfile <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capturetot.txt"
# captfile_cor <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capturetot_cor.txt"
capt <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Capture.txt"

# trapfile <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/traptot.txt"



trapfile1 <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_OPEN.txt"
trapfile2 <- "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/DATA_pour_David/Trap_MULTI-SESSION_CLOSED.txt"

# Data check
count.fields(capt)
count.fields(trapfile1)
count.fields(trapfile2)

c <- read.delim(capt, h=F)
t1 <- read.delim(trapfile1, h = F)
t2 <- read.delim(trapfile2, h = F)

unique(c$V4[c$V1 == "OPEN"]) %in% unique(t1$V1)
unique(c$V4[c$V1 == "CLOSED"]) %in% unique(t2$V1)

d <- secr::read.capthist(capt, c(trapfile2, trapfile1)) # WARNING ! Enter trap files in the same order than factor(session) -> first, "CLOSED", then "OPEN"
summary(d, terse = TRUE)

# Fitting models
fit0 <- secr::secr.fit(d,
                       mask = NULL,
                       model = D ~ 1)
# Detection probability constant across animals, occasions and detectors
fit1 <- secr::secr.fit(d,
                       mask = NULL,
                       model = g0 ~ 1)
# Detection probability varies with individual behaviour = learning reponse (b)
fit2 <- secr::secr.fit(d,
                       mask = NULL,
                       model = g0 ~ b)
# learn reponse affects both g0 and sigma
fit3 <- secr::secr.fit(d,
                       mask = NULL,
                       model = list(g0 ~ b, sigma ~ b))
# Detection probability varies with individuals learning and time (pour savoir si les cages deviennent attractante seulement au bout de plusieurs jours après installation)
fit4 <- secr::secr.fit(d,
                       mask = NULL,
                       model = list(g0 ~ b + T))
# Session
fit5 <- secr::secr.fit(d,
                       mask = NULL,
                       model = list(D ~ session, g0 ~ session, sigma ~ session))

# Session
fit6 <- secr::secr.fit(d,
                       mask = NULL,
                       model = list(D ~ 1, g0 ~ session, sigma ~ session))

##### COMPARAISON DE TOUS LES MODELES ####
AIC(fit0, fit1, fit2, fit3, fit4, fit5, fit6)


#------------------------------------#
#### Rajouter les mask et buffer ####
#----------------------------------#

Ha <- maptools::readShapeSpatial("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/Habitat")
ovtrap <-secr::traps(d)
ovmask <- secr::make.mask(ovtrap,
                    buffer = 1000,
                    type = "trapbuffer",
                    poly = Ha,
                    keep.poly = FALSE)
ovmask <- secr::addCovariates(ovmask, Ha)

# Fitting models
fit0 <- secr::secr.fit(d,
                       mask = ovmask,
                       model = D ~ 1)
# Detection probability constant across animals, occasions and detectors
fit1 <- secr::secr.fit(d,
                       mask = ovmask,
                       model = g0 ~ 1)
# Detection probability varies with individual behaviour = learning reponse (b)
fit2 <- secr::secr.fit(d,
                       mask = ovmask,
                       model = g0 ~ b)
# learn reponse affects both g0 and sigma
fit3 <- secr::secr.fit(d,
                       mask = ovmask,
                       model = list(g0 ~ b, sigma ~ b))
# Detection probability varies with individuals learning and time (pour savoir si les cages deviennent attractante seulement au bout de plusieurs jours après installation)
fit4 <- secr::secr.fit(d,
                       mask = ovmask,
                       model = list(g0 ~ b + T))
# Session
fit5 <- secr::secr.fit(d,
                       mask = ovmask,
                       model = list(D ~ session, g0 ~ session, sigma ~ session))

# Session
fit6 <- secr::secr.fit(d,
                       mask = ovmask,
                       model = list(D ~ 1, g0 ~ session, sigma ~ session))

##### COMPARAISON DE TOUS LES MODELES ####
AIC(fit0, fit1, fit2, fit3, fit4, fit5, fit6)
