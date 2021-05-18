# #PERMANOVA fish community and sharks

#Making data frame for PERMANOVA
Factors_NotSharks <- PCO_NotSharks %>% 
  #joining shark richness data
  left_join(RicSharks %>% select(Location, Replicate, Richness_loc) %>% unique(), 
            by = c("Replicate", "Location")) %>%
  #joining MaxNFish data
  left_join(MaxNSharks %>% select(Location, Replicate, MaxN_mean_loc) %>% unique(),
            by = c("Replicate", "Location"))


#PERMANOVA Main effects and interactions
perm1 <- adonis(Mat_NotSharks ~ MaxN_mean_loc + Richness_loc, 
                data = Factors_NotSharks, method = "bray", permutations = 9999)
perm1

#Betadispersion mean MaxN location
dispersion <- betadisper(Dist_NotSharks, group = Factors_NotSharks$MaxN_mean_loc)
beta_perm1 <- permutest(dispersion, permutations = 9999)
beta_perm1
#Significant

#Betadispersion mean richness location
dispersion <- betadisper(Dist_NotSharks, group = Factors_NotSharks$Richness_loc)
beta_perm2 <- permutest(dispersion, permutations = 9999)
beta_perm2
#Significant

library(pairwiseAdonis)

#Pairwise PERMANOVA Location
pair_adonis <- pairwise.adonis(Dist_NotSharks, Factors_NotSharks$Location, perm = 9999)
pair_adonis

