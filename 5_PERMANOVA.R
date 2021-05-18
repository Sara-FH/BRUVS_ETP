###############################################################################################################
# Title: PERMANOVA MaxN
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################

# Loading libraries -------------------------------------------------------

{library(vegan)
library(pairwiseAdonis)
  }

# PERMANOVA FishCom -------------------------------------------------------

#PERMANOVA Main effects and interactions
perm1 <- adonis(Mat_FishCom ~ Protection_status*Coastal_Oceanic*Location, 
                data = PCO_FishCom, method = "bray", permutations = 9999)
perm1

#Betadispersion Protection status
dispersion <- betadisper(Dist_FishCom, group = PCO_FishCom$Protection_status)
beta_perm1 <- permutest(dispersion, permutations = 9999)
beta_perm1
#Non-Significant

#Betadispersion Coastal_Oceanic
dispersion <- betadisper(Dist_FishCom, group = PCO_FishCom$Coastal_Oceanic)
beta_perm2 <- permutest(dispersion, permutations = 9999)
beta_perm2
#Non-significant

#Betadispersion Location
dispersion <- betadisper(Dist_FishCom, group = PCO_FishCom$Location)
beta_perm3 <- permutest(dispersion, permutations = 9999)
beta_perm3
#Significant

#pairwise PERMANOVA Protection status
pair_adonis <- pairwise.adonis(Dist_FishCom, PCO_FishCom$Protection_status, perm = 9999)
pair_adonis
#All significantly different from each other, High, Medium, Low

#pairwise PERMANOVA Coastal_Oceanic
pair_adonis2 <- pairwise.adonis(Dist_FishCom, PCO_FishCom$Coastal_Oceanic, perm = 9999)
pair_adonis2
#Significantly different from each other, Coastal, Oceanic

#pairwise PERMANOVA Location
pair_adonis <- pairwise.adonis(Dist_FishCom, PCO_FishCom$Location, perm = 9999)
pair_adonis
#All significantly different from each other except GSF and PNM


# PERMANOVA Predators -------------------------------------------------------

#PERMANOVA Main effects and interactions
perm1 <- adonis(Mat_Predators ~ Protection_status*Coastal_Oceanic*Location, 
                data = PCO_Predators, method = "bray", permutations = 9999)
perm1
#Significant

#Betadispersion Protection status
dispersion <- betadisper(Dist_Predators, group = PCO_Predators$Protection_status)
beta_perm1 <- permutest(dispersion, permutations = 9999)
beta_perm1
#Non-Significant

#Betadispersion Coastal_Oceanic
dispersion <- betadisper(Dist_Predators, group = PCO_Predators$Coastal_Oceanic)
beta_perm2 <- permutest(dispersion, permutations = 9999)
beta_perm2
#Significant

#Betadispersion Location
dispersion <- betadisper(Dist_Predators, group = PCO_Predators$Location)
beta_perm3 <- permutest(dispersion, permutations = 9999)
beta_perm3
#Significant

#pairwise PERMANOVA Protection status
pair_adonis <- pairwise.adonis(Dist_Predators, PCO_Predators$Protection_status, perm = 9999)
pair_adonis
#All significantly different from each other, High, Medium, Low

#pairwise PERMANOVA Coastal_Oceanic
pair_adonis2 <- pairwise.adonis(Dist_Predators, PCO_Predators$Coastal_Oceanic, perm = 9999)
pair_adonis2
#Significantly different from each other, Coastal, Oceanic

#pairwise PERMANOVA Location
pair_adonis <- pairwise.adonis(Dist_Predators, PCO_Predators$Location, perm = 9999)
pair_adonis
#All significantly different from each other except GSF and PNM


# PERMANOVA Sharks -------------------------------------------------------

#PERMANOVA Main effects and interactions
perm1 <- adonis(Mat_Sharks ~ Protection_status*Coastal_Oceanic*Location, 
                data = PCO_Sharks, method = "bray", permutations = 9999)
perm1
#Significant

#Betadispersion Protection status
dispersion <- betadisper(Dist_Sharks, group = PCO_Sharks$Protection_status)
beta_perm1 <- permutest(dispersion, permutations = 9999)
beta_perm1
#Significant

#Betadispersion Coastal_Oceanic
dispersion <- betadisper(Dist_Sharks, group = PCO_Sharks$Coastal_Oceanic)
beta_perm2 <- permutest(dispersion, permutations = 9999)
beta_perm2
#Significant

#Betadispersion Location
dispersion <- betadisper(Dist_Sharks, group = PCO_Sharks$Location)
beta_perm3 <- permutest(dispersion, permutations = 9999)
beta_perm3
#Significant

#pairwise PERMANOVA Protection status
pair_adonis <- pairwise.adonis(Dist_Sharks, PCO_Sharks$Protection_status, perm = 9999)
pair_adonis
#All significantly different from each other, High, Medium, Low

#pairwise PERMANOVA Coastal_Oceanic
pair_adonis2 <- pairwise.adonis(Dist_Sharks, PCO_Sharks$Coastal_Oceanic, perm = 9999)
pair_adonis2
#Significantly different from each other, Coastal, Oceanic

#pairwise PERMANOVA Location
pair_adonis <- pairwise.adonis(Dist_Sharks, PCO_Sharks$Location, perm = 9999)
pair_adonis
#All significantly different from each other except GSF, PNM and Cano
