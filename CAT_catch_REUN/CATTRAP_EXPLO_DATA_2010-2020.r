#### Exploration & analysis of trapped cats data ####
rm(list = ls())

cat <- read.table("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Catch_rate/CATTRAP_Bilan_2010-2020.txt", h = T, dec = ",", sep = "\t")
dim(cat)
summary(cat)

# Keeping the CAGE type only
cat_trap <- cat[cat$dispo_type == "CAGE",]

#### EXPLORATION ####
for(i in c(2, 4:7)){
print(table(cat_trap[,i], useNA = "always"))
}

# ---- Exploration of captured cat number ---- 
tapply(cat_trap$trap_cat, cat_trap$season, sum)
tapply(cat_trap$trap_cat, cat_trap$season_year, sum)
tapply(cat_trap$trap_night, cat_trap$season_year, sum)


# AV2M applied the new protocol only from 2019
table(cat_trap$structure, cat_trap$season_year)

# ---- Computation of raw capture rate/season_year ----
require(dplyr)
rate <- cat_trap %>%
  dplyr::group_by(season_year) %>%
  dplyr::summarise(total_cat_trap = sum(trap_cat), total_trap_night = sum(trap_night), catch_rate = sum(trap_cat)/sum(trap_night))

barplot(rate$catch_rate, bty = "n", col = rainbow(length(rate$season_year)), names.arg = rate$season_year)

# ---- Categorization of protocol type 'old' vs. 'new' ----
# rate$season_number <- 1:nrow(rate)
# rate$protocol <- c(rep("old", 6), rep("new", 4))
# ***WARNING*** - Be carefull because application year of new protocol was different depending on the structure


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

#### GLOBAL SPATIAL VISUALISATION ####

# ---- Choosing a color range ----
rainb_col <- rainbow(n = length(unique(cat_trap$site_code)))
cat_trap$site_color <- rainb_col[as.numeric(as.factor(cat_trap$site_code))]

# ---- Data explo ----
count <- table(cat_trap$site_code)
barplot(count, col = rainb_col[as.numeric(as.factor(unique(cat_trap$site_code)))])

# ---- Data visualization ----
plot(cat_trap$lon,
     cat_trap$lat,
     pch = 20,
     col = rainb_col[as.numeric(as.factor(cat_trap$site_code))],
     cex = 2)

#legend("topright", legend = unique(cat_trap$site_code), col = rainb_col[unique(cat_trap$site_code_number)], pch = 20)

points(cat_trap$lon[cat_trap$site_code == "GBR"],
     cat_trap$lat[cat_trap$site_code == "GBR"],
     pch = 20,
     #col = rainb_col[cat_trap$site_code_number[cat_trap$site_code == "AZO"]]
     col = "black")

#### GBR SPATIAL VISUALISATION ####

# ---- Data subset ---- 
# with site_code = "GBR"
GBR_trap <- cat_trap[cat_trap$site_code == "GBR",]

# ---- Working on localities ---- 
table(GBR_trap$local, useNA = 'always')

# ----  Cleaning data ---- 
GBR_trap$local[stringr::str_detect(GBR_trap$local,
                             'Piste for') == TRUE] <- 'Piste forestiere Maido'

# ---- Different color for each locality ---- 
unique(GBR_trap$local)
rainb_colGBR <-rainbow(length(unique(GBR_trap$local)))

# ---- Visualization ---- 
plot(GBR_trap$lon,
     GBR_trap$lat,
     pch = 20,
     col = rainb_colGBR[as.numeric(as.factor(GBR_trap$local))],
     cex = 2)
points(GBR_trap$lon[GBR_trap$local == "Piste de La Glacière"],
       GBR_trap$lat[GBR_trap$local == "Piste de La Glacière"],
       pch = 20,
       col = "black") # Here problem with category of localities for each GPS points

# *** ==> necessity of delimitation with a polygon delimited the Maido area ***
# Use the shapefile !!!

#### EXTRACTION OF GRAND BENARRE DATABASE ####

# write.table(GBR_trap, 'C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Catch_rate/CATTRAP_GBR_data.txt', sep = '\t', dec = '.')
