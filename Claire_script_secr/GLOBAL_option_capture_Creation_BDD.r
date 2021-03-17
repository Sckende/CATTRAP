#### DATA for testing the global session option ####
# Script for building data from the Naïs database

rm(list = ls())

#### Capture file creation ####
cap <- read.delim("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Nais_script_secr/atot.txt", h = F, sep = "")


# Replace "Markocc" by "habitat"
cap2 <- cap[-1,]
cap2[, 5][cap2[, 1] == "CMR"] <- "OPEN"
cap2[, 5][cap2[, 1] == "TEST"] <- "CLOSED"

# Modif for only one session
cap2[,1] <- "GLOBAL"

# Adding first row
class(cap2)

names(cap2) <- c("SESSION", "ID", "OCC", "DETECTOR", "HABITAT")

write.table(cap2, "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capture_GLOBAL_option.txt", sep = "", row.names = F)

# *** !!! Don't forget to add the # at the beginning of the first row in the .txt file !!! *** #


#### Trap file creation ####
rm(list = ls())

trap <- read.delim("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Nais_script_secr/btot.txt", h = F, sep = "")
