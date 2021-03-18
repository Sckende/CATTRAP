#### DATA for testing the global session option ####
# Script for building data from the Naïs database

rm(list = ls())

#### Capture file creation ####
cap <- read.delim("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Nais_script_secr/atot.txt", h = F, sep = "")


# Replace "Markocc" by "habitat"
cap2 <- cap[-1,]
cap2[, 5][cap2[, 1] == "CMR"] <- "OPEN"
cap2[, 5][cap2[, 1] == "TEST"] <- "CLOSED"

# Adding the "b" letter for camera used in closed habitat
cap2$V4[cap2$V1 == "TEST"] <- paste(cap2$V4[cap2$V1 == "TEST"], "b", sep = "")

# Modif for only one session
cap2[,1] <- "GLOBAL"

# Adding first row
class(cap2)

names(cap2) <- c("SESSION", "ID", "OCC", "Detector", "HABITAT")

#write.table(cap2, "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/capture_GLOBAL_option.txt", sep = "", row.names = F)

# *** !!! Don't forget to add the # at the beginning of the first row in the .txt file !!! *** #


#### Trap file creation version 1 ####
rm(list = ls())

trap <- read.delim("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap_GLOBAL_option2.txt", h = F, sep = "\t")

trap$V7 <- paste(trap$V5, trap$V6, sep = "")
trap <- trap[, -c(5,6)]
trap$V7[1] <- "usage"

#write.table(trap, "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap_GLOBAL_option3.txt", col.names = F, row.names = F)

# *** !!! Don't forget to add the # at the beginning of the first row in the .txt file !!! *** #

#### Trap file creation version 2 ####
rm(list = ls())

trap <- read.delim("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap_GLOBAL_option.txt", h = F, sep = "\t")

# To verify the number of trap occasions
nchar(trap$V4)
nchar(trap$V5)


trap$V7 <- paste(trap$V4, trap$V5, sep = "")
nchar(trap$V7)
trap$V7[1] <- "usage"

trap <- trap[, -c(4, 5)]

#write.table(trap, "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap_GLOBAL_option4.txt", col.names = F, row.names = F)

# *** !!! Don't forget to add the # at the beginning of the first row in the .txt file !!! *** #

#### Trap file creation version 3 ####
rm(list = ls())

trap <- read.delim("C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap_GLOBAL_option.txt", h = F, sep = "\t")

# To verify the number of trap occasions
nchar(trap$V4)
nchar(trap$V5)


trap$V7 <- paste(trap$V4, trap$V5, sep = "")
nchar(trap$V7)
trap$V7[1] <- "usage"

trap <- trap[, c(1:3, 7)]

#write.table(trap, "C:/Users/Etudiant/Desktop/SMAC/GITHUB/CATTRAP/Claire_script_secr/trap_GLOBAL_option5.txt", col.names = F, row.names = F)

# *** !!! Don't forget to add the # at the beginning of the first row in the .txt file !!! *** #