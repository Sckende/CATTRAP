#### Exploration & analysis of trapped cats ####

cat_trap <- read.table("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Catch_rate/CATTRAP_Bilan_2010-2020.txt", h = T, dec = ",", sep = "\t")
dim(cat_trap)
summary(cat_trap)

for(i in 1:6){
print(table(cat_trap[,i], useNA = "always"))
}


tapply(cat_trap$trap_cat, cat_trap$season, sum)
tapply(cat_trap$trap_cat, cat_trap$season_year, sum)
tapply(cat_trap$trap_night, cat_trap$season_year, sum)

# Keeping the CAGE type only
cat_trap <- cat_trap[cat_trap$dispo_type == "CAGE",]

# AV2M applied the new protocol only from 2019
table(cat_trap$structure, cat_trap$season_year)


require(dplyr)
rate <- cat_trap %>%
  dplyr::group_by(season_year) %>%
  dplyr::summarise(total_cat_trap = sum(trap_cat), total_trap_night = sum(trap_night), catch_rate = sum(trap_cat)/sum(trap_night))

plot(rate$season_year, rate$catch_rate, bty = "n")

rate$season_number <- 1:nrow(rate)
rate$protocol <- c(rep("old", 6), rep("new", 4))

#### EXPLO ANALYSIS ####
library("lme4") # For generalised linear models
library("visreg") # Vizualisation of model effects
library("DHARMa") # For simulations
library("AICcmodavg") # For AIC comparison
library("car") # For the Anova command


m1 <- stats::glm(rate$total_cat_trap ~ rate$season_number,
                family = poisson(),
                offset = rate$total_trap_night)
summary(m1)

m2 <- stats::lm(rate$total_cat_trap ~ rate$protocol,
                offset = rate$total_trap_night)
summary(m2)

m3 <- stats::lm(rate$total_cat_trap ~ rate$season_number + rate$protocol,
                offset = rate$total_trap_night)
summary(m3)

m4 <- stats::lm(rate$total_cat_trap ~ rate$season_number + rate$season_number^2,
                offset = rate$total_trap_night)
summary(m4)

AICcmodavg::aictab(m1, m2, m3, m4)


??gam
gam1 <- mgcv::gam(rate$total_cat_trap ~ s(rate$season_number),
          family = poisson(),
          offset = log(rate$total_trap_night),
          method = "REML")
summary(gam1)
plot(gam1, page = 1, all.terms = TRUE, residuals = TRUE)



lm1 <- stats::glm(rate$total_cat_trap ~ rate$season_number,
                  family = poisson(),
                  offset = log(rate$total_trap_night))
summary(lm1)
plot(gam1, page = 1, all.terms = TRUE, residuals = TRUE)
