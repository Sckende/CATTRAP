##### Exploration of cat trap data from 2010 to 2013 #####
# Multiple origins: Parc National, SEOR, AV2M
rm(list = ls())

#### DATA TOTAL CAPTURE PER TRAP ####
tot_trap <- read.table("C:/Users/Etudiant/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/DATA/BDD_CHAT_MODIFIEE/CHAT_bilan_Captures_par_cage_2010-2013.txt", sep = "\t", h = T, dec = ",")
summary(tot_trap)

head(tot_trap)
dim(tot_trap)

length(unique(tot_trap$tot_trap_name))
unique(tot_trap$hab_type)

# Modification of habitat type name
hab_long <- unique(tot_trap$hab_type)
hab_court <- c("VEHA",
               "Tamarinaie",
               "FMASV",
               "FTMSV",
               "Cryptomeria",
               "TFTMSV",
               "TFSS")

for(i in 1:length(unique(tot_trap$hab_type))){
  print(hab_long[i])
  print(hab_court[i])
  
  tot_trap$hab_type[tot_trap$hab_type == hab_long[i]] <- hab_court[i]
}

# [1] "Végétation éricoïde de haute altitude" = "VEHA"
# [1] "Tamarinaie cultivée" = "Tamarinaie"
# [1] "Forêt de moyenne altitude des fonds de cirque sous le vent" = "FMASV"
# [1] "Forêt tropicale de montagne sous le vent (et formations pionnières associées)" = "FTMSV"
# [1] "Cryptomeria" = "Cryptomeria"
# [1] "Taches de forêt tropicale de montagne sous le vent" = "TFTMSV"
# [1] "Taches de forêt semi-sèche (dans fourrés secs anthropiques)" = "TFSS"


t <- tapply(tot_trap$trap_night, tot_trap$year, sum)
tc <- tapply(tot_trap$trapped_cat, tot_trap$year, sum)

rate <- tc/t

plot(rate)

#### IDEM MAIS AVEC DATA DE NAIS ####
rm(list = ls())

#### DATA TOTAL CAPTURE PER TRAP ####
cat_trap <- read.table("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Catch_rate/CAT_trapped_2010_2018.txt", sep = "\t", h = T, dec = ",")
summary(cat_trap)
