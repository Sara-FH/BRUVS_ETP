###############################################################################################################
# Title: PERMANOVA density and biomass
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################

# Loading libraries -------------------------------------------------------

{library(vegan)
library(pairwiseAdonis)
  }


# PERMANOVA density -------------------------------------------------------

#PERMANOVA Main effects and interactions
perm1 <- adonis(Den_mat ~ Location + Site, 
                data = Factors, method = "bray", permutations = 9999)
perm1

perm1 <- adonis(Den_mat ~ Location,
                data = Factors, method = "bray", permutations = 9999)
perm1

#Betadispersion Location
dispersion <- betadisper(Den_mat_dist, group = Factors$Location)
beta_perm1 <- permutest(dispersion, permutations = 9999)
beta_perm1
#Non-significant

#Betadispersion Site
dispersion <- betadisper(Den_mat_dist, group = Factors$Site)
beta_perm2 <- permutest(dispersion, permutations = 9999)
beta_perm2
#Non-significant

#Pairwise PERMANOVA to test which of the Locations differ significantly from each other
pair_adonis <- pairwise.adonis(Den_mat_dist, Factors$Location, perm = 9999)
pair_adonis

