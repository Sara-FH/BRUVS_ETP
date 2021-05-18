###############################################################################################################
# Title: PCO plot MaxN per hour for predatory fish
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

{library(tidyverse)
  library(vegan)
  # library(ape) #For principal coordinates analysis
  library(RColorBrewer)
  library(ggrepel)
  library(ggordiplots)
}


# PCO plot for MaxNFish ----------------------------------------------------

#Finding dummy value
temp <- MaxNPred %>% 
  select(Location, Replicate, ValidName, MaxN_sp) %>% 
  mutate(Dummy = MaxN_sp^0.5)

rm(temp)

#Making Matrix for MaxNFish of species
MaxN_mat <- MaxNPred %>% 
  select(Replicate, ValidName, Location, MaxN_sp) %>% 
  #Using only unique values of MaxNFish per species per Replicate
  unique() %>%
  #Adding the Location to the Replicate name (I split them later for the plot)
  unite(RepLoc, Replicate, Location, sep = " ") %>%
  #Making the format right for the matrix
  pivot_wider(names_from = "ValidName", values_from = "MaxN_sp") %>% #Making the format right for the matrix
  #Adding dummy species to all sites, to enable dissimilarity calculations for empty sites
  rename("Dummy" = "NA") %>% 
  mutate(Dummy = 0.66) %>% 
  arrange(RepLoc) %>% #Arranging Replicate names
  column_to_rownames("RepLoc") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at the Replicate.

#Checking ranges for transformations and no transformation
range(MaxN_mat)
range(MaxN_mat^0.5)
range(MaxN_mat^0.25)

#Applying a 4th root transformation to matrix
MaxN_mat <- MaxN_mat^0.5

#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
MaxN_mat_dist <- vegdist(MaxN_mat, method = "bray")

#Create a PCoA (Principal Co-ordinates Analysis) plot
#returns matrix of scores scaled by eigenvalues
MaxN_mat_pco <- wcmdscale(MaxN_mat_dist, eig = TRUE, add = "lingoes") 
#Show plot
plot(MaxN_mat_pco, type = "points") #Add type points to remove labels

#binding PCO coordinates to dataframe
PCO_MaxNFish <- as.data.frame(MaxN_mat_pco$points[,1:2])
PCO_MaxNFish <- data.table::setDT(PCO_MaxNFish, keep.rownames = TRUE)[]
PCO_MaxNFish <- PCO_MaxNFish %>% 
  rename(Replicate = rn, PCO1 = Dim1, PCO2 = Dim2) %>% 
  mutate(Location = case_when(endsWith(Replicate, "Malpelo") ~ "Malpelo",
                              endsWith(Replicate, "Revilla") ~ "Revilla",
                              endsWith(Replicate, "Cano") ~ "Cano", 
                              endsWith(Replicate, "Clip") ~ "Clip",
                              endsWith(Replicate, "GSF") ~ "GSF",
                              endsWith(Replicate, "PNM") ~ "PNM", 
                              endsWith(Replicate, "GMR") ~ "GMR")) %>% 
  #Removing location from Replicate names
  mutate(Replicate = str_remove_all(Replicate, " Malpelo| Revilla| Cano| Clip| PNM| GSF| GMR")) %>% 
  left_join(SiteInfo %>% select(-c(3:6)),  
            by = c("Location", "Replicate")) #Adding environmental information for the Replicates


# Pearson Correlation MaxNFish PCO wcmdscale -----------------------------------------

#We extract scores from PCoA and calculate correlation with biomass matrix
SpCorr <- envfit(MaxN_mat_pco$points, MaxN_mat, permutations = 9999)
#Check out the scores to each dimension from the correlation calculated above 
scores(SpCorr, "vectors")
#Plot your PCoA
plot(MaxN_mat_pco$points, type = "p")
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


# PCO MaxNFish with arrows -------------------------------------------------
#Function that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(MaxN_mat_pco, MaxN_mat) {
  
  # Keeping the species that has the largest arrows (from former PCO plot)
  MaxN_mat = MaxN_mat[ , Corr_species]
  
  n <- nrow(MaxN_mat)
  points.stand <- scale(MaxN_mat_pco$points)
  
  # Compute covariance of variables with all axes
  S <- cov(MaxN_mat, points.stand)
  
  # Select only positive eigenvalues
  pos_eigen = MaxN_mat_pco$eig[seq(ncol(S))]
  
  # Standardize value of covariance (see Legendre & Legendre 1998)
  U <- S %*% diag((pos_eigen/(n - 1))^(-0.5))
  colnames(U) <- colnames(MaxN_mat_pco$points)
  
  # Add values of covariances inside object
  MaxN_mat_pco$U <- U
  
  return(MaxN_mat_pco)
}

#computing arrows for species using the function compute_arrows
species_pco_arrows <- compute_arrows(MaxN_mat_pco, MaxN_mat)
#changing points to data.frame before putting it in ggplot2
species_pco_arrows$points <- as.data.frame(species_pco_arrows$points)

#making arrows smaller, so they fit better in the PCO
arrows_df <- as.data.frame(species_pco_arrows$U/20)
arrows_df$variable <- rownames(arrows_df)

#Naming arrows with short species names
arrows_df$variable <- Corr_short$Short_sp


# PCO plot MaxNFish ----------------------------------------------------------------

#Making an anchor for the arrows
Anchor <- c(0.92, -0.23)

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$Dim1 + Anchor[1])*K
Y2 <- (arrows_df$Dim2 + Anchor[2])*K

ordi <- gg_ordiplot(MaxN_mat_pco, groups = PCO_MaxNFish$Location, kind = "sd")

PCO_MaxNFish$Protection_status <- factor(PCO_MaxNFish$Protection_status, levels = c("Low", "Medium", "High"))

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_MaxN5 <- ggplot(PCO_MaxNFish) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(PCO1, PCO2, color = Location, shape = Protection_status), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(4, 2, 5)) +
  geom_path(data = ordi$df_ellipse, aes(x = x, y = y, colour = Group), size = 1) +
  guides(color = guide_legend(order = 1, title = "MPA", direction = "horizontal"),
         shape = guide_legend(order = 2, title = "Status", direction = "horizontal")) +
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
              as.character(as.numeric(format(round(MaxN_mat_pco$eig[1]/sum(MaxN_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  ylab(paste0("PCO2 (", 
              as.character(as.numeric(format(round(MaxN_mat_pco$eig[2]/sum(MaxN_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  scale_x_continuous(breaks = seq(-0.25, 0.75, by = 0.25), limits = c(-0.40, 0.90)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25), limits = c(-0.55, 0.55)) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.6, 0.9), 
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

PCO_MaxN5

#Saving PCO for biomass - method and management status
# ggsave("../Figures/PCO_bio_method_fishing.tiff", 
#        PCO_bio_1, device = "tiff", dpi = 300, width = 11, height = 10)



# PCO plot Coastal_Oceanic ------------------------------------------------

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_MaxN6 <- ggplot(PCO_MaxNFish) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(PCO1, PCO2, color = Location, shape = Coastal_Oceanic), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(4, 2)) +
  geom_path(data = ordi$df_ellipse, aes(x = x, y = y, colour = Group), size = 1) +
  guides(color = guide_legend(order = 1, title = "MPA", direction = "horizontal"),
         shape = guide_legend(order = 2, title = "Location", direction = "horizontal")) +
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
              as.character(as.numeric(format(round(MaxN_mat_pco$eig[1]/sum(MaxN_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  ylab(paste0("PCO2 (", 
              as.character(as.numeric(format(round(MaxN_mat_pco$eig[2]/sum(MaxN_mat_pco$eig), 3)))*100), 
              "% of total variation)")) +
  scale_x_continuous(breaks = seq(-0.25, 0.75, by = 0.25), limits = c(-0.40, 0.90)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25), limits = c(-0.55, 0.55)) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.6, 0.9), 
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

PCO_MaxN6

Plot5_6 <- ggarrange(PCO_MaxN5, NULL, PCO_MaxN6, widths = c(1, 0.1, 1), nrow = 1)

Plot5_6

#Saving PCO for biomass - method and management status
ggsave("Figures/PCO_Predators.tiff",
       Plot5_6, device = "tiff", dpi = 300, width = 21, height = 10)

# Data from Fish Community ------------------------------------------------

PCO_Predators <- PCO_MaxNFish

Mat_Predators <- MaxN_mat

Dist_Predators <- MaxN_mat_dist

# PCO MaxNFish plot with hull ----------------------------------------------

# # Calculate the hulls for each group
# hull_MaxN <- PCO_MaxNFish %>%
#   group_by(Location) %>%
#   slice(chull(PCO1, PCO2))
# 
# #Plotting biomass, method, fishing and arrows for species with largest biomass
# PCO_MaxN6 <- ggplot(PCO_MaxNFish) + 
#   #Adding color and shapes # shape = Zone, can be added when I have zonation ready
#   geom_point(aes(PCO1, PCO2, color = Location, shape = Location), stroke = 2, size = 4.5) + 
#   scale_color_brewer(palette = "Dark2") +
#   scale_shape_manual(values = c(0:6)) +
#   # guides(color = guide_legend(order = 1), 
#   #        shape = guide_legend(order = 2)) +
#   #adding convex hull
#   geom_polygon(data = hull_MaxN,
#                x = hull_MaxN$PCO1, y = hull_MaxN$PCO2,
#                aes(fill = Location,
#                    colour = Location),
#                alpha = 0.3,
#                show.legend = FALSE) +
#   scale_fill_brewer(palette = "Dark2") +
#   #Adding arrows
#   geom_segment(data = arrows_df, 
#                x = Anchor[1], y = Anchor[2],
#                mapping = aes(xend = X2, yend = Y2),
#                arrow = arrow(length = unit(2, "mm")), #Adding arrow head
#                size = 0.8) +
#   #Adding arrow labels
#   geom_text_repel(data = arrows_df, aes(label = variable),
#                   size = 7, fontface = "italic",
#                   lineheight = 0.6,
#                   x = X2, y = Y2,
#                   box.padding = 0.5) +
#   theme_classic() +
#   #Adding percentages for the PCO axes
#   xlab(paste0("PCO1 (", 
#               as.character(as.numeric(format(round(MaxN_mat_pco$eig[1]/sum(MaxN_mat_pco$eig), 3)))*100), 
#               "% of total variation)")) +
#   ylab(paste0("PCO2 (", 
#               as.character(as.numeric(format(round(MaxN_mat_pco$eig[2]/sum(MaxN_mat_pco$eig), 3)))*100), 
#               "% of total variation)")) +
#   #moving legend in plot and making box around it
#   theme(legend.position = c(0.85, 0.85), 
#         legend.box.background = element_rect(size = 0.7, linetype = "solid", colour ="black"), 
#         legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
#         legend.title = element_text(color = "black", size = 23),
#         legend.text = element_text(color = "black", size = 21),
#         axis.text.x = element_text(color = "black", size = 23), 
#         axis.text.y = element_text(color = "black", size = 23), 
#         axis.title.x = element_text(color = "black", size = 25),
#         axis.title.y = element_text(color = "black", size = 25), 
#         axis.line.x = element_line(color = "black", size = 1), 
#         axis.line.y = element_line(color = "black", size = 1), 
#         axis.ticks = element_line(color = "black", size = 1.2), 
#         axis.ticks.length = unit(0.2, "cm"))
# 
# PCO_MaxN6

#Remove unnecessary variables
rm(MaxN_mat_pco, PCO_MaxNFish, species_pco_arrows, Anchor, 
   Corr_species, Corr_short, K, X2, Y2, compute_arrows, arrows_df, hull_MaxN)

