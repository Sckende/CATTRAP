##SCRIPT pour les données captures tests appats#########
library(secr)
library(maptools)
setwd("~/Desktop/CMR/ANALYSES R")

##1########## importer les fichiers CMR et TEST separés et tot.....
###CMR
a<-read.delim("a.txt",sep=" ",header=F)
b<-read.delim("b.txt",sep=" ",header=F)
###TEST
a1<-read.delim("a1.txt",sep=" ",header=F)
b1<-read.delim("b1.txt",sep=" ",header=F)
###tot
atot<-read.delim("atot.txt",sep=" ",header=F)
btot<-read.delim("btot.txt",sep=" ",header=F)



##2###### créer d #####
#créer l'objet d pour CMR (avec a et b)
read.capthist("a.txt","b.txt",detector ='count',noccasions=30)->d
#créer l'objet d1 pour TEST (avec a1 et b1)
read.capthist("a1.txt","b1.txt",detector ='count',noccasions=33)->d1
#créer l'objet dtot pour total (avec atot et btot)
read.capthist("atot.txt","btot.txt",detector ="count",noccasions=63)->dtot


##3##### Rajouter les mask et buffer
###Pour CMR, d avec buffer=1000
Ha<- readShapeSpatial ('Habitat')
ovtrap <-traps(d)
ovmask <- make.mask(ovtrap, buffer = 1000, type = "trapbuffer",poly = Ha, keep.poly = FALSE)
ovmask <- addCovariates(ovmask, Ha)

###Pour TEST, d1 avec buffer=400
Ha<- readShapeSpatial ('Habitat')
ovtrap <-traps(d1)
ovmask <- make.mask(ovtrap, buffer = 400, type = "trapbuffer",poly = Ha, keep.poly = FALSE)
ovmask <- addCovariates(ovmask, Ha)

###Pour TOTAL, dtot avec buffer=1000
Ha<- readShapeSpatial ('Habitat')
ovtrap <-traps(dtot)
ovmask <- make.mask(ovtrap, buffer = 1000, type = "trapbuffer",poly = Ha, keep.poly = FALSE)
ovmask <- addCovariates(ovmask, Ha)

##4###### TESTS DES MODELES
#########TESTS MODELES NULS
###Pour CMR
fit.0 <- secr.fit(d, mask = ovmask, model = D ~ 1)
###Pour TEST
fit.01 <- secr.fit(d1, mask = ovmask, model = D ~ 1)
###Pour TOT
fit.0tot <- secr.fit(dtot, mask = ovmask, model = D ~ 1)

##########TESTS MODELES DENSITE VARIE EN FONCTION DES SESSIONS POUR TOT
fit.stot <- secr.fit(dtot, mask = ovmask, model = D ~ session)

#############MODELE TEST DE LA DENSITE EN FONCTION DE L'HABITAT POUR TOT
fit.Dforest <- secr.fit(dtot, mask = ovmask, model = D ~ NOM)

###########Test Modèle proba de détection is constant across animals, occasions and detecors
###Pour CMR
fit.1 <- secr.fit(d, mask = ovmask, model = g0 ~ 1)
###Pour TEST
fit.11 <- secr.fit(d1, mask = ovmask, model = g0 ~ 1)
###Pour TOT
fit.1tot <- secr.fit(dtot, mask = ovmask, model = g0 ~ 1)

###########Test Modèle proba de détection fonction du comportement = learning reponse (b)
###Pour CMR
fit.2 <- secr.fit(d, mask = ovmask, model = g0 ~ b)
###Pour TEST
fit.21 <- secr.fit(d1, mask = ovmask, model = g0 ~ b)
###Pour TOT
fit.2tot <- secr.fit(dtot, mask = ovmask, model = g0 ~ b)

###########Test Modèle learn reponse affects both g0 and sigma
###Pour CMR
fit.3 <- secr.fit(d, mask = ovmask, model = list(g0 ~ b, sigma ~ b))
###Pour TEST
fit.31 <- secr.fit(d1, mask = ovmask, model = list(g0 ~ b, sigma ~ b))
###Pour TOT
fit.3tot <- secr.fit(dtot, mask = ovmask, model = list(g0 ~ b, sigma ~ b))

############Modèle learned reponse in g0 combined with trend over occasions (pour savoir si les cages deviennent attractante seulement au bout de plusieurs jours après installation)
###Pour CMR
fit.4 <- secr.fit(d, mask = ovmask, model = list(g0 ~ b + T))
###Pour TEST
fit.41 <- secr.fit(d1, mask = ovmask, model = list(g0 ~ b + T))
###Pour TOT
fit.4tot <- secr.fit(dtot, mask = ovmask, model = list(g0 ~ b + T))


############Modèle variation densité en fonction du temps pour TOT
fit.5tot <- secr.fit(dtot, mask = ovmask, model = list(D~t))



##############COMPARAISON DE TOUS LES MODELES
AIC(fit.0,fit.01,fit.0tot,fit.stot,fit.Dforest,fit.1,fit.11,fit.1tot,fit.2,fit.21,fit.2tot,fit.3,fit.31,fit.3tot,fit.4,fit.41,fit.4tot,fit.5tot)


#### sauver l'environnement
save.image("CMR.RData")


############Distance maximale parcourue
MMDM(d, min.recapt = 1, full = FALSE, userdist = NULL, mask = mask)

MMDM(d1, min.recapt = 1, full = FALSE, userdist = NULL, mask = mask)