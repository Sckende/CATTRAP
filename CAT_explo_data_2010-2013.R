##### Exploration of cat trap data from 2010 to 2013 #####
# Multiple origins: Parc National, SEOR, AV2M


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


#### DATA CAPTURE ####
cat_trap <- read.table("C:/Users/Etudiant/Desktop/SMAC/Projet_publi/2-CHAT_optimis_piegeage_genet/DATA/BDD_CHAT_MODIFIEE/CHAT_Captures_2010-2013.txt", sep = "\t", h = T, dec = ",")
summary(cat_trap)
