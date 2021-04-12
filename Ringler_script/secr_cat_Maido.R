### CMR chats Maido

# 2 sessions, 30 jours, 20/21 cameras traps
# CLOSED = foret, hos sentier
# OPEN = sentiers 


library(secr)
load(".RData")


### secr models
cat_maido<-read.capthist(file.choose(),c(file.choose(),file.choose()),
		fmt="trapID",
		#covnames=c("sex","age","group"),
		#trapcovnames=c("device"),
		detector='proximity'
		)
cat_maido<-shareFactorLevels(cat_maido)


# halfnormal dfn
Mcat01<-secr.fit(cat_maido, model = list(D~session,g0~1,sigma~1),detectfn=0,CL=F, buffer=3000, verify=F)
# D ~ 0.18, 0.15
# g0 ~ 0,07
# sigma ~ 900

# hazard dfn
Mcat02<-secr.fit(cat_maido, model = list(D~session,g0~1,sigma~1,z~1),detectfn=1,CL=F, buffer=3000, verify=F)
# D ~ 0.21, 0.17
# g0 ~ 0,08
# sigma ~ 820

Mcat04<-secr.fit(cat_maido, model = list(D~1,g0~1,sigma~1,z~1),detectfn=1,CL=F, buffer=3000, verify=F)
# D ~ 0.19/km2
# g0 ~ 0,08
# sigma ~ 820m
# z ~ 3.88
# Home range 95% and 50%
HR95<-3.14*((circular.r(p=0.95,detectfn='HR',detectpar=list(sigma=1,z=3.88)))*820)^2
# HR95 = 7.9 km2
HR50<-3.14*((circular.r(p=0.5,detectfn='HR',detectpar=list(sigma=1,z=3.88)))*820)^2
# HR50 = 0.7 km2

Mcat05<-secr.fit(cat_maido, model = list(D~1,g0~session,sigma~1,z~1),detectfn=1,CL=F, buffer=3000, verify=F)


Mcat06<-secr.fit(cat_maido, model = list(D~1,g0~1,sigma~session,z~1),detectfn=1,CL=F, buffer=3000, verify=F)
# D ~ 0.19
# g0 ~ 0,08
# sigma ~ 820

Mcat07<-secr.fit(cat_maido, model = list(D~1,g0~session,sigma~session,z~1),detectfn=1,CL=F, buffer=3000, verify=F)

Mcat08<-secr.fit(cat_maido, model = list(D~1,g0~1,sigma~session,z~session),detectfn=1,CL=F, buffer=3000, verify=F)

Mcat09<-secr.fit(cat_maido, model = list(D~1,g0~1,sigma~1,z~session),detectfn=1,CL=F, buffer=3000, verify=F)


AIC(Mcat01,Mcat02,Mcat04,Mcat05,Mcat06,Mcat07,Mcat08,Mcat09)
# best model...Mcat04
# graph proba de detection vs distance au centre d'activité/domaine vital
plot(Mcat04,xval=0:2000)


