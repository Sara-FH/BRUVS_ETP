###############################################################################################################
# Title: PCO plot density and biomass
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

{library(tidyverse)
library(vegan)
library(ape) #For principal coordinates analysis
library(RColorBrewer)
library(ggrepel)
}


# PCO plot for density ----------------------------------------------------

#Making Matrix for density of species
Den_mat <- Density %>% 
  select(Site, ValidName, Location, MaxN_sp) %>% 
  #Using only unique values of density per species per site
  unique() %>%
  #Adding the Location to the site name (I split them later for the plot)
  unite(SiteLoc, Site, Location, sep = " ") %>%
  #Making the format right for the matrix
  pivot_wider(names_from = "ValidName", values_from = "MaxN_sp") %>% #Making the format right for the matrix
  arrange(SiteLoc) %>% #Arranging site names
  column_to_rownames("SiteLoc") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at the site.

#Checking ranges for transformations and no transformation
range(Den_mat)
range(Den_mat^0.5)
range(Den_mat^0.25)
#I have read online that I should get in the 0-10 range
#To achieve this I am doing a fourth root transformation
#Question - do you agree with the square root transformation - and the reason for doing it?

#Applying a 4th root transformation to matrix
Den_mat <- Den_mat^0.5

#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
Den_mat_dist <- vegdist(Den_mat, method = "bray")
#Create a PCoA (Principal Co-ordinates Analysis) plot
#returns matrix of scores scaled by eigenvalues
Den_mat_pco <- wcmdscale(Den_mat_dist, eig = TRUE, add = "lingoes") 
#Show plot
plot(Den_mat_pco, type = "points") #Add type points to remove labels

#Principal coordinate analysis and simple ordination plot
# Den_mat_pcoa <- pcoa(Den_mat_dist)
# #Biplot with arrows
# biplot(Den_mat_pcoa, Den_mat)
#Question - in finalizing this can be deleted

#binding PCO coordinates to dataframe
PCO_density <- as.data.frame(Den_mat_pco$points[,1:2])
PCO_density <- data.table::setDT(PCO_density, keep.rownames = TRUE)[]
PCO_density <- PCO_density %>% 
  rename(Site = rn, PCO1 = Dim1, PCO2 = Dim2) %>% 
  mutate(Location = case_when(endsWith(Site, "Malpelo15") ~ "Malpelo15",
                              endsWith(Site, "Malpelo18") ~ "Malpelo18",
                              endsWith(Site, "Revillagigedo") ~ "Revillagigedo")) %>% 
  #Removing location from site names
  mutate(Site = str_remove_all(Site, " Malpelo15| Malpelo18| Revillagigedo")) %>% 
  left_join(EnvVar,  by = c("Location", "Site")) #Adding environmental information for the sites


# Pearson Correlation Density PCO wcmdscale -----------------------------------------

#We extract scores from PCoA and calculate correlation with biomass matrix
SpCorr <- envfit(Den_mat_pco$points, Den_mat, permutations = 9999)
#Check out the scores to each dimension from the correlation calculated above 
scores(SpCorr, "vectors")
#Plot your PCoA
plot(Den_mat_pco$points, type = "p")
#Overlay species which have significant correlation (p <= 0.05)
plot(SpCorr, p.max = 0.05, col = "red")
#You can check the actual correlation coefficients (Pearson) using the line below
SpCorr$vectors$r

#correlation values in data frame
temp <- as.data.frame(SpCorr$vectors$r) %>% 
  #Rename column
  rename("r" = `SpCorr$vectors$r`) %>% 
  #Making species into column
  rownames_to_column(., var = "Species") %>% 
  #Remove species with correlation below 0.6
  filter(r > 0.5)

#Making character vector of species names for correlated species
Corr_species <- as.vector(temp$Species)

#Making character vector of short species names
Corr_short <- temp %>% 
  mutate(Genus = substr(Species, 1, 1)) %>% 
  mutate(Sp = word(Species, -1)) %>% 
  unite(Short_sp, Genus, Sp, sep = ". ")

#Remove unnecessary variable
rm(SpCorr, temp)


# PCO density with arrows -------------------------------------------------
#Function that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(Den_mat_pco, Den_mat) {
  
  # Keeping the species that has the largest arrows (from former PCO plot)
  Den_mat = Den_mat[ , Corr_species]
  
  n <- nrow(Den_mat)
  points.stand <- scale(Den_mat_pco$points)
  
  # Compute covariance of variables with all axes
  S <- cov(Den_mat, points.stand)
  
  # Select only positive eigenvalues
  pos_eigen = Den_mat_pco$eig[seq(ncol(S))]
  
  # Standardize value of covariance (see Legendre & Legendre 1998)
  U <- S %*% diag((pos_eigen/(n - 1))^(-0.5))
  colnames(U) <- colnames(Den_mat_pco$points)
  
  # Add values of covariances inside object
  Den_mat_pco$U <- U
  
  return(Den_mat_pco)
}

#computing arrows for species using the function compute_arrows
species_pco_arrows <- compute_arrows(Den_mat_pco, Den_mat)
#changing points to data.frame before putting it in ggplot2
species_pco_arrows$points <- as.data.frame(species_pco_arrows$points)

#making arrows smaller, so they fit better in the PCO
arrows_df <- as.data.frame(species_pco_arrows$U/20)
arrows_df$variable <- rownames(arrows_df)

#Naming arrows with short species names
arrows_df$variable <- Corr_short$Short_sp


# PCO plot density ----------------------------------------------------------------

#Making an anchor for the arrows
Anchor <- c(0.0, 0.3)

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Dim1 + Anchor[1])*K
Y2 <- (arrows_df$Dim2 + Anchor[2])*K

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_den <- ggplot(PCO_density) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(PCO1, PCO2, color = Location), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  # scale_shape_manual(values = c(21,24)) +
  # guides(color = guide_legend(order = 1), 
  #        shape = guide_legend(order = 2)) +
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  #Adding arrow labels
  geom_text_repel(data = arrows_df, aes(label = variable),
                   size = 7, fontface = "italic",
                   lineheight = 0.6,
                   x = X2, y = Y2,
                   box.padding = 0.5) +
  theme_classic() +
  #Adding percentages for the PCO axes
  xlab(paste0("PCO1 (", 
              as.character(as.numeric(format(round(Den_mat_pco$eig[1]/sum(Den_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  ylab(paste0("PCO2 (", 
              as.character(as.numeric(format(round(Den_mat_pco$eig[2]/sum(Den_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.85, 0.82), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 21),
        axis.text.x = element_text(color = "black", size = 23), 
        axis.text.y = element_text(color = "black", size = 23), 
        axis.title.x = element_text(color = "black", size = 25),
        axis.title.y = element_text(color = "black", size = 25), 
        axis.line.x = element_line(color = "black", size = 1), 
        axis.line.y = element_line(color = "black", size = 1), 
        axis.ticks = element_line(color = "black", size = 1.2), 
        axis.ticks.length = unit(0.2, "cm"))

PCO_den

#Saving PCO for biomass - method and management status
# ggsave("../Figures/PCO_bio_method_fishing.tiff", 
#        PCO_bio_1, device = "tiff", dpi = 300, width = 11, height = 10)


# PCO density plot with hull ----------------------------------------------

# Calculate the hulls for each group
hull_den <- PCO_density %>%
  group_by(Location) %>%
  slice(chull(PCO1, PCO2))

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_den2 <- ggplot(PCO_density) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(PCO1, PCO2, color = Location, shape = Location), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(15:17)) +
  # guides(color = guide_legend(order = 1), 
  #        shape = guide_legend(order = 2)) +
  #adding convex hull
  geom_polygon(data = hull_den,
               x = hull_den$PCO1, y = hull_den$PCO2,
               aes(fill = Location,
                   colour = Location),
               alpha = 0.3,
               show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  #Adding arrows
  geom_segment(data = arrows_df, 
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  #Adding arrow labels
  geom_text_repel(data = arrows_df, aes(label = variable),
                  size = 7, fontface = "italic",
                  lineheight = 0.6,
                  x = X2, y = Y2,
                  box.padding = 0.5) +
  theme_classic() +
  #Adding percentages for the PCO axes
  xlab(paste0("PCO1 (", 
              as.character(as.numeric(format(round(Den_mat_pco$eig[1]/sum(Den_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  ylab(paste0("PCO2 (", 
              as.character(as.numeric(format(round(Den_mat_pco$eig[2]/sum(Den_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.85, 0.85), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 21),
        axis.text.x = element_text(color = "black", size = 23), 
        axis.text.y = element_text(color = "black", size = 23), 
        axis.title.x = element_text(color = "black", size = 25),
        axis.title.y = element_text(color = "black", size = 25), 
        axis.line.x = element_line(color = "black", size = 1), 
        axis.line.y = element_line(color = "black", size = 1), 
        axis.ticks = element_line(color = "black", size = 1.2), 
        axis.ticks.length = unit(0.2, "cm"))

PCO_den2

#Remove unnecessary variables
rm(Den_mat_pco, PCO_den, PCO_density, species_pco_arrows, Anchor, 
   Corr_species, Corr_short, K, X2, Y2, compute_arrows, arrows_df, PCO_den2, hull_den)


# PCO plot for biomass --------------------------------------

#Making matrix for dissimilarity calculation biomass
Bio_mat <- Biomass %>%
  select(Site, ValidName, Location, Biomass_site_sp) %>% 
  #Making biomass_site_sp from g to kg
  mutate(Biomass_site_sp = Biomass_site_sp/1000) %>% #biomass in kg
  #Using only unique values of biomass per species per site
  unique() %>% 
  #Adding the Location to the site name (I split them later for the plot)
  unite(SiteLoc, Site, Location, sep = " ") %>% 
  #Making the format right for the matrix
  pivot_wider(names_from = "ValidName", values_from = "Biomass_site_sp") %>% 
  arrange(SiteLoc) %>% #Arranging site names
  column_to_rownames("SiteLoc") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at a site

#Checking ranges for transformations and no transformation
range(Bio_mat)
range(Bio_mat^0.5)
range(Bio_mat^0.25)

#Applying a 4th root transformation to biomass data - and making a matrix
Bio_mat <- Bio_mat^0.25 

#Calculating dissimilarity distance using vegan package, method Bray-Curtis
Bio_mat_dist <- vegdist(Bio_mat, method = "bray")
#Create a weighted Principal Coordinates Analysis plot
#returns matrix of scores scaled by eigenvalues
Bio_mat_pco <- wcmdscale(Bio_mat_dist, eig = TRUE, add = "lingoes")
#Show plot
plot(Bio_mat_pco, type = "points") #Add type points to remove labels

#Principal coordinate analysis and simple ordination plot
# Bio_mat_pcoa <- pcoa(Bio_mat_dist)
# #barplot of eigenvalues 1-10
# barplot(Bio_mat_pcoa$values$Relative_eig[1:10])
# #Biplot with arrows
# biplot(Bio_mat_pcoa, Bio_mat)
#The principal coordinates analysis from pcoa function from the Ape package is similar to the 
#weighted principal coordinates analysis plot from wcmdscale 

#binding PCO coordinates to dataframe
PCO_biomass <- as.data.frame(Bio_mat_pco$points[,1:2])
PCO_biomass <- data.table::setDT(PCO_biomass, keep.rownames = TRUE)[]
PCO_biomass <- PCO_biomass %>% 
  #Getting Locations from site name
  rename(Site = rn, PCO1 = Dim1, PCO2 = Dim2) %>% 
  mutate(Location = case_when(endsWith(Site, "Malpelo15") ~ "Malpelo15",
                              endsWith(Site, "Malpelo18") ~ "Malpelo18",
                              endsWith(Site, "Revillagigedo") ~ "Revillagigedo")) %>% 
  #Removing location from site names
  mutate(Site = str_remove_all(Site, " Malpelo15| Malpelo18| Revillagigedo")) %>% 
  left_join(EnvVar,  by = c("Location", "Site")) #Adding environmental information for the sites


# Pearson Correlation biomass PCO ------------------------------------------------

#We extract scores from PCoA and calculate correlation with biomass matrix
SpCorr <- envfit(Bio_mat_pco$points, Bio_mat, permutations = 9999)
#Check out the scores to each dimension from the correlation calculated above 
scores(SpCorr, "vectors")
#Plot your PCoA
plot(Bio_mat_pco$points, type = "p")
#Overlay species which have significant correlation (p <= 0.05)
plot(SpCorr, p.max = 0.05, col = "red")
#You can check the actual correlation coefficients (Pearson) using the line below
SpCorr$vectors$r

#correlation values in data frame
temp <- as.data.frame(SpCorr$vectors$r) %>% 
  #Rename column
  rename("r" = `SpCorr$vectors$r`) %>% 
  #Making species into column
  rownames_to_column(., var = "Species") %>% 
  #Remove species with correlation below 0.5
  filter(r > 0.5)

#Making character vector of species names for correlated species
Corr_species <- as.vector(temp$Species)

#Making character vector of short species names
Corr_short <- temp %>% 
  mutate(Genus = substr(Species, 1, 1)) %>% 
  mutate(Sp = word(Species, -1)) %>% 
  unite(Short_sp, Genus, Sp, sep = ". ")

#Remove unnecessary variable
rm(SpCorr, temp)

# Names on arrows in PCO plot --------------------------------------------------
#Function that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(Bio_mat_pco, Bio_mat) {
  
  #Question - change names - depending on pearson values
  #Keeping the species that has the largest arrows (from former PCO plot)
  Bio_mat = Bio_mat[ , Corr_species]
  
  n <- nrow(Bio_mat)
  points.stand <- scale(Bio_mat_pco$points)
  
  # Compute covariance of variables with all axes
  S <- cov(Bio_mat, points.stand)
  
  # Select only positive eigenvalues
  pos_eigen = Bio_mat_pco$eig[seq(ncol(S))]
  
  # Standardize value of covariance (see Legendre & Legendre 1998)
  U <- S %*% diag((pos_eigen/(n - 1))^(-0.5))
  colnames(U) <- colnames(Bio_mat_pco$points)
  
  # Add values of covariances inside object
  Bio_mat_pco$U <- U
  
  return(Bio_mat_pco)
}

#computing arrows for species using the function compute_arrows
species_pco_arrows <- compute_arrows(Bio_mat_pco, Bio_mat)
#changing points to data.frame before putting it in ggplot2
species_pco_arrows$points <- as.data.frame(species_pco_arrows$points)

#making arrows smaller, so they fit better in the PCO
arrows_df <- as.data.frame(species_pco_arrows$U/20)
arrows_df$variable <- rownames(arrows_df)

#Question - change names - depending on pearson values
#Naming arrows with short species names
arrows_df$variable <- Corr_short$Short_sp


# PCO plot biomass ----------------------------------------------------------------

#Making an anchor for the arrows
Anchor <- c(0.05, -0.35) #upper right corner

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Dim1 + Anchor[1])*K
Y2 <- (arrows_df$Dim2 + Anchor[2])*K

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_bio <- ggplot(PCO_biomass) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(PCO1, PCO2, color = Location), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  # scale_shape_manual(values = c(21,24)) +
  # guides(color = guide_legend(order = 1), 
  #        shape = guide_legend(order = 2)) +
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  #Adding arrow labels
  geom_text_repel(data = arrows_df, aes(label = variable),
                  size = 7, fontface = "italic",
                  lineheight = 0.6, 
                  x = X2, y = Y2, 
                  box.padding = 0.5, max.overlaps = Inf) + 
  #Changing axes and color
  # scale_x_continuous(breaks = seq(-0.6, 0.6, by = 0.3), limits = c(-0.6, 0.55)) +
  # scale_y_continuous(breaks = seq(-0.6, 0.6, by = 0.3), limits = c(-0.6, 0.4)) +
  theme_classic() +
  #Adding percentages for the PCO axes
  xlab(paste0("PCO1 (", 
              as.character(as.numeric(format(round(Bio_mat_pco$eig[1]/sum(Bio_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  ylab(paste0("PCO2 (", 
              as.character(as.numeric(format(round(Bio_mat_pco$eig[2]/sum(Bio_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.85, 0.9), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 21),
        axis.text.x = element_text(color = "black", size = 23), 
        axis.text.y = element_text(color = "black", size = 23), 
        axis.title.x = element_text(color = "black", size = 25),
        axis.title.y = element_text(color = "black", size = 25), 
        axis.line.x = element_line(color = "black", size = 1), 
        axis.line.y = element_line(color = "black", size = 1), 
        axis.ticks = element_line(color = "black", size = 1.2), 
        axis.ticks.length = unit(0.2, "cm"))

PCO_bio

#Saving PCO for biomass - method and management status
# ggsave("../Figures/PCO_bio_method_fishing.tiff", 
#        PCO_bio_1, device = "tiff", dpi = 300, width = 11, height = 10)


# PCO biomass plot with hull ----------------------------------------------

# Calculate the hulls for each group
hull_bio <- PCO_biomass %>%
  group_by(Location) %>%
  slice(chull(PCO1, PCO2))

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_bio2 <- ggplot(PCO_biomass) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(PCO1, PCO2, color = Location, shape = Location), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(15:17)) +
  # guides(color = guide_legend(order = 1), 
  #        shape = guide_legend(order = 2)) +
  #adding convex hull
  geom_polygon(data = hull_bio,
               x = hull_bio$PCO1, y = hull_bio$PCO2,
               aes(fill = Location,
                   colour = Location),
               alpha = 0.3,
               show.legend = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  geom_segment(data = arrows_df, #Adding arrows
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X2, yend = Y2),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8) +
  #Adding arrow labels
  geom_text_repel(data = arrows_df, aes(label = variable),
                   size = 7, fontface = "italic",
                   lineheight = 0.6, 
                   x = X2, y = Y2, 
                   box.padding = 0.5, max.overlaps = Inf) + 
  #Changing axes and color
  # scale_x_continuous(breaks = seq(-0.6, 0.6, by = 0.3), limits = c(-0.6, 0.55)) +
  # scale_y_continuous(breaks = seq(-0.6, 0.6, by = 0.3), limits = c(-0.6, 0.4)) +
  theme_classic() +
  #Adding percentages for the PCO axes
  xlab(paste0("PCO1 (", 
              as.character(as.numeric(format(round(Bio_mat_pco$eig[1]/sum(Bio_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  ylab(paste0("PCO2 (", 
              as.character(as.numeric(format(round(Bio_mat_pco$eig[2]/sum(Bio_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.85, 0.9), 
        legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.title = element_text(color = "black", size = 23),
        legend.text = element_text(color = "black", size = 21),
        axis.text.x = element_text(color = "black", size = 23), 
        axis.text.y = element_text(color = "black", size = 23), 
        axis.title.x = element_text(color = "black", size = 25),
        axis.title.y = element_text(color = "black", size = 25), 
        axis.line.x = element_line(color = "black", size = 1), 
        axis.line.y = element_line(color = "black", size = 1), 
        axis.ticks = element_line(color = "black", size = 1.2), 
        axis.ticks.length = unit(0.2, "cm"))

PCO_bio2


#Remove unnecessary variables
rm(Bio_mat_pco, PCO_bio, PCO_biomass, species_pco_arrows, Anchor, Corr_species,
   K, X2, Y2, compute_arrows, arrows_df, PCO_bio2, hull_bio, Corr_short)

# Preparing data for PERMANOVA --------------------------------------------

#Data frame for PERMANOVA Factors to be tested
Factors <- EnvVar %>% 
  #joining richness data
  left_join(Richness, by = c("Site", "Location")) %>% 
  #joining density data
  left_join(Density %>% select(-c(ValidName, MaxN, Record_duration, MaxN_sp, hours)), 
            by = c("Site", "Location")) %>% 
  #joining biomass data
  left_join(Biomass %>% select(Site, Location, Biomass_site, Record_duration) %>% unique(), 
            by = c("Site", "Location")) %>% 
  unique() %>% 
  mutate_if(is.character, as.factor) %>% 
  arrange(Site)
#Checking type of data for all columns in Factors
str(Factors)

#Site checking
x <- Biomass %>% select(Site) %>% unique()
y <- Density %>% select(Site) %>% unique()
z <- Factors %>% select(Site) %>% unique()
#Sites that is only in Density and Factors, meaning they are not in Length data
#BE3B4080416
#SO3B1020416
#both sites are from Revillagigedo
rm(x, y, z)
