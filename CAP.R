
#It is not a good idea to call a variable that has been produced by another script without providing
#some sort of description of where it is coming from. You have a lot of scripts and in a few months
#you may not remember what the workflow should be since the scripts are not all numbered - DFA

#Making non-shark matrix
NotSharks <- MaxN %>% 
  filter(Shark != "Shark" | is.na(Shark)) #Simply add to include NA cells to avoid joins


#Calculating MaxN per hour for each of the replicates and for species per replicate
MaxN_NotSharks <- NotSharks %>% 
  #Removing unnecessary columns
  select(!c(Site:Species, SpeciesName, MaxLgth_mm, Shark)) %>% 
  #calculating Record_duration in hours
  mutate(Record_duration_hrs = Record_duration/60) %>% 
  #MaxN per hour column
  mutate(MaxN_sp = MaxN/Record_duration_hrs)
  
#I do not understand why this section has so many calculations - What is all this about? - DFA
#Why are you not using the summarise option. It is hard to follow what is happening here - DFA

# Loading libraries -------------------------------------------------------

{library(tidyverse)
  library(vegan)
  # library(ape) #For principal coordinates analysis
  library(RColorBrewer)
  library(ggrepel)
  library(ggordiplots)
}


# PCO plot for MaxNFish ----------------------------------------------------

#Finding dummy value - What is this about? It is immediately deleted - DFA
temp <- MaxN_NotSharks %>% 
  select(Location, Replicate, ValidName, MaxN_sp)
#this shows the smallest value of MaxN which will also be the value for the dummy species

rm(temp)

#Making Matrix for MaxNFish of species
MaxN_mat <- MaxN_NotSharks %>% 
  select(Replicate, ValidName, Location, MaxN_sp) %>% 
  #Using only unique values of MaxNFish per species per Replicate
  unique() %>%
  #Adding the Location to the Replicate name (I split them later for the plot) - Why are you doing this? - DFA
  #The Location code already contains the location name - DFA
  #This is to make sure I have unique replicate names and keep the location information
  unite(RepLoc, Replicate, Location, sep = ":") %>%
  #Making the format right for the matrix
  pivot_wider(names_from = "ValidName", values_from = "MaxN_sp") %>% #Making the format right for the matrix
  #Adding dummy species to all sites, to enable dissimilarity calculations for empty sites
  mutate(Dummy = 0.66) %>% #Where has this number come from?
  #removing NA value for areas with no species
  select(!"NA") %>%
  arrange(RepLoc) %>% #Arranging Replicate names
  column_to_rownames("RepLoc") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at the Replicate.

#Checking ranges for transformations and no transformation
range(MaxN_mat)
range(MaxN_mat^0.5)
range(MaxN_mat^0.25)

#Applying a square root transformation to matrix
MaxN_mat <- MaxN_mat^0.5

#Calculating dissimilarity distance using vegan package, the default is Bray Curtis
MaxN_mat_dist <- vegdist(MaxN_mat, method = "bray") #This was commented out,
#Without this line, the chunk below does not work and nothing else in the script - DFA
# 
# library(BiodiversityR)

#Factors describing replicates
Factors <- as.data.frame(MaxN_mat) %>% 
  rownames_to_column(., var = "Replicate") %>% 
  select(1) %>% 
  separate(Replicate, c("Replicate", "Location"), sep = ":") %>% 
  mutate(Location = as.factor(Location))

#Perform CAP - Canonical Analysis of Principal Coordinates
MaxN_cap <- BiodiversityR::CAPdiscrim(MaxN_mat_dist ~ Location, data = Factors, 
                                      dist = "bray", axes = 4, m = 0, mmax = 10, add = FALSE, 
                                      permutations = 9999)
MaxN_cap

#CAP plot
plot1 <- ordiplot(MaxN_cap, type="none")
BiodiversityR::ordisymbol(plot1, Factors, "Location", legend=TRUE)
rm(plot1)

#binding PCO coordinates to dataframe
CAP <- as.data.frame(MaxN_cap$PCoA[,1:2]) %>% 
  rownames_to_column("Replicate") %>% 
  separate(Replicate, c("Replicate", "Location"), sep = ":") %>% 
  rename(CAP1 = V1, CAP2 = V2) %>% 
  left_join(SiteInfo %>% select(-c(3:6)), by = c("Location", "Replicate"))


# Pearson Correlation MaxNFish PCO wcmdscale -----------------------------------------

#Richness of sharks to add to envfit 
Ric <- RicSharks %>% #As above, where is this variable coming from? - DFA
  select(Replicate, Location, Ric_rep_hr) %>%
  group_by(Replicate, Location) %>%
  arrange(Replicate)

#MaxN of sharks to add to envfit
N <- MaxNSharks %>% #Again, where is this coming from? - DFA
  select(Replicate, Location, MaxN_rep) %>%
  group_by(Replicate, Location) %>%
  arrange(Replicate) %>%
  unique() %>%
  #transforming data similarly to species matrix
  mutate(MaxN_rep = MaxN_rep^0.5) #Why are you doing this? What similarity matrix are you referring to? - DFA

#Variables and species to check for correlation with axes
#I do not understand why this is being done? Why are you joining this to a matrix? - DFA
SpVar <- MaxN_mat %>%
  cbind(Ric_rep = Ric$Ric_rep_hr) %>%
  cbind(MaxN_rep = N$MaxN_rep)

#We extract scores from PCoA and calculate correlation with matrix
# SpCorr <- envfit(MaxN_cap$PCoA, SpVar, permutations = 999) #I am not sure why you are using a matrix here that
#contains information that is not relevant for the analysis. You are essentially correlating the data you used
#to create the PCoA. I do not understand what you are trying to achieve here. - DFA
#The rows in your email and code do not match so I will assume this is the shark correlation. I recommend you
#take some time to read more about the help section of the envfit function so you understand how it works.
#You got a lot of warnings after running the line above, so it is worth you read these to understand the outputs
#you are getting. What you want to do is correlate total log-abundance of (all) shark species and the species 
#richness of sharks only with the rest of the assemblage (I have said this before but you really need to read the
#methods in David's paper, it is all explained there. 
#You simply need you ordination results and the shark factors (abundance and species richness) to calculate the
#correlations. Any other data is irrelevant here. - DFA
SpCorr <- envfit(MaxN_cap$PCoA, data.frame(Ric_rep = Ric$Ric_rep_hr, MaxN_rep = N$MaxN_rep), permutations = 999)
#Now you do not get warnings of things potentially going wrong. It is really important you read warnings and errors
#to ensure you are getting results that make sense. - DFA
#It just occurred to me that you may have wanted to also correlate the other fish abundances too, similar to a SIMPER
#analysis, then I guess this could also work, although you may want to do some digging online to find out if this is 
#a good approach. - DFA

#Check out the scores to each dimension from the correlation calculated above 
scores(SpCorr, "vectors") #Now you get two scores only, which makes sense and it is exactly what you wanted - DFA
#Plot your PCoA
plot(MaxN_cap$PCoA, type = "p")
#Overlay species which have significant correlation (p <= 0.05)
plot(SpCorr, p.max = 0.05, col = "red")
#You can check the actual correlation coefficients (Pearson) using the line below
SpCorr$vectors$r
#The result is that these correlations are both significant, but species richness has a low correlation (~0.27) and
#MaxN has a medium association (~0.55) - DFA

#correlation values in data frame
temp <- as.data.frame(SpCorr$vectors$r) %>% 
  #Rename column
  rename("r" = `SpCorr$vectors$r`) %>% 
  #Making species into column
  rownames_to_column(., var = "Species") %>% 
  #Remove species with correlation below 0.5
  filter(r > 0.6)

#Making character vector of species names for correlated species
Corr_species <- as.vector(temp$Species)

#Making character vector of short species names
Corr_short <- temp %>% 
  mutate(Genus = substr(Species, 1, 1)) %>% 
  mutate(Sp = word(Species, -1)) %>% 
  unite(Short_sp, Genus, Sp, sep = ". ")

#Variables for red arrow
temp2 <- as.data.frame(SpCorr$vectors$r) %>% 
  #Rename column
  rename("r" = `SpCorr$vectors$r`) %>% 
  #Making species into column
  rownames_to_column(., var = "Species") %>% 
  #Keeping only MaxN and richness of sharks for arrows
  filter(Species == "MaxN_rep" | Species == "Ric_rep")

#Making character vector of species names for correlated species
Corr_species2 <- as.vector(temp2$Species)

#Changing names for the red arrows
Corr_short2 <- temp2 %>% 
  mutate(Short_sp = recode(Species, "MaxN_rep" = "MaxN", "Ric_rep" = "Richness"))

#Remove unnecessary variable
rm(SpCorr, temp, temp2)

#####################################################
#I cannot see where the shark data is being correlated with the fish assemblages
#If you see Figure 7 in David's 2018 paper, you can see that he has two red dotted lines
#One labeled Log(N+1) which refers to the total log-abundance of (all) shark species
#and the other one labeled S which refers to the species richness of sharks only
#If you want to do this analysis, you need to perform the above calculations on shark
#data only and then correlate these values to the PCoA using something envfit.
#The first full paragraph in page 8 (labelled 80 in the pdf) includes more details on
#how this analysis was done. - DFA
#####################################################
#Now I have added the Shark MaxN and richness to the envfit in line 107
#I am preparing the data in the lines above that from 86 to 104.
#Is it the correct way I am doing things now?
#####################################################


# PCO MaxNFish with arrows -------------------------------------------------
#Function that computes arrows from a pcoa and a species matrix
compute_arrows <-  function(MaxN_cap, SpVar) {
  
  # Keeping the species that has the largest arrows (from former PCO plot)
  SpVar = SpVar[ , Corr_species]
  
  n <- nrow(SpVar)
  points.stand <- scale(MaxN_cap$PCoA)
  
  # Compute covariance of variables with all axes
  S <- cov(SpVar, points.stand)
  
  # Select only positive eigenvalues
  pos_eigen = MaxN_cap$manova$Eigenvalues[seq(ncol(S))]
  
  # Standardize value of covariance (see Legendre & Legendre 1998)
  U <- S %*% diag((pos_eigen/(n - 1))^(-0.5))
  colnames(U) <- colnames(MaxN_cap$PCoA)
  
  # Add values of covariances inside object
  MaxN_cap$U <- U
  
  return(MaxN_cap)
}

#computing arrows for species using the function compute_arrows
species_pco_arrows <- compute_arrows(MaxN_cap, SpVar)
#changing points to data.frame before putting it in ggplot2
species_pco_arrows$PCoA <- as.data.frame(species_pco_arrows$PCoA)

#making arrows smaller, so they fit better in the PCO
arrows_df <- as.data.frame(species_pco_arrows$U/20)
arrows_df$variable <- rownames(arrows_df)

#Naming arrows with short species names
arrows_df$variable <- Corr_short$Short_sp

# PCO red arrows for MaxN and richness of sharks --------------------------------------
#Function that computes arrows from a pcoa and a species matrix
compute_arrows2 <-  function(MaxN_cap, SpVar) {
  
  # Keeping the species that has the largest arrows (from former PCO plot)
  SpVar = SpVar[ , Corr_species2]
  
  n <- nrow(SpVar)
  points.stand <- scale(MaxN_cap$PCoA)
  
  # Compute covariance of variables with all axes
  S <- cov(SpVar, points.stand)
  
  # Select only positive eigenvalues
  pos_eigen = MaxN_cap$manova$Eigenvalues[seq(ncol(S))]
  
  # Standardize value of covariance (see Legendre & Legendre 1998)
  U <- S %*% diag((pos_eigen/(n - 1))^(-0.5))
  colnames(U) <- colnames(MaxN_cap$PCoA)
  
  # Add values of covariances inside object
  MaxN_cap$U <- U
  
  return(MaxN_cap)
}

#computing arrows for species using the function compute_arrows
species_pco_arrows2 <- compute_arrows2(MaxN_cap, SpVar)
#changing points to data.frame before putting it in ggplot2
species_pco_arrows2$PCoA <- as.data.frame(species_pco_arrows2$PCoA)

#making arrows smaller, so they fit better in the PCO
arrows_df2 <- as.data.frame(species_pco_arrows2$U/20)
arrows_df2$variable <- rownames(arrows_df2)

#Naming arrows with short species names
arrows_df2$variable <- Corr_short2$Short_sp


# PCO plot MaxNFish ----------------------------------------------------------------

#Making an anchor for the arrows
Anchor <- c(0.8, 0.17)

#Constant adjusting the size of vectors
K <- 1 #not actually necessary, as it is currently 1, but good for playing around with the code

#define other coordinates for arrows
X2 <- (arrows_df$V1 + Anchor[1])*K
Y2 <- (arrows_df$V2 + Anchor[2])*K

#define other coordinates for arrows
X3 <- (arrows_df2$V1 + Anchor[1])*K
Y3 <- (arrows_df2$V2 + Anchor[2])*K

ordi <- gg_ordiplot(MaxN_cap, groups = CAP$Location, kind = "sd")

CAP$Protection_status <- factor(CAP$Protection_status, 
                                         levels = c("Low", "Medium", "High"))

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_MaxN7 <- ggplot(CAP) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(CAP1, CAP2, color = Location, shape = Protection_status), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(4, 2, 5)) +
  geom_path(data = ordi$df_ellipse, aes(x = x, y = y, colour = Group), size = 1) +
  guides(color = guide_legend(order = 1, title = "MPA"),
         shape = guide_legend(order = 2, title = "Status")) +
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
  #Adding red arrows
  geom_segment(data = arrows_df2, 
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X3, yend = Y3),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8, color = "red") +
  #Adding red arrow labels
  geom_text_repel(data = arrows_df2, aes(label = variable),
                  size = 7, fontface = "bold", color = "red",
                  lineheight = 0.6,
                  x = X3, y = Y3,
                  box.padding = 0.5) +
  theme_classic() +
  #Adding percentages for the PCO axes
  xlab(paste0("CAP1 (",
              as.character(as.numeric(format(
                round(MaxN_cap$manova$Eigenvalues[1]/sum(MaxN_cap$manova$Eigenvalues), 3)))*100),
              "% of total variation)")) +
  ylab(paste0("CAP2 (",
              as.character(as.numeric(format(
                round(MaxN_cap$manova$Eigenvalues[2]/sum(MaxN_cap$manova$Eigenvalues), 3)))*100),
              "% of total variation)")) +
  scale_x_continuous(breaks = seq(-0.4, 0.8, by = 0.2), limits = c(-0.4, 0.8)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.2), limits = c(-0.4, 0.4)) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.33), 
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

PCO_MaxN7

paste0("CAP1 (",
       as.character(as.numeric(format(
         round(MaxN_cap$manova$Eigenvalues[1]/sum(MaxN_cap$manova$Eigenvalues), 3)))*100),
       "% of total variation)")

paste0("CAP2 (",
       as.character(as.numeric(format(
         round(MaxN_cap$manova$Eigenvalues[2]/sum(MaxN_cap$manova$Eigenvalues), 3)))*100),
       "% of total variation)")

#Saving PCO for biomass - method and management status
# ggsave("Figures/PCOA_noSharks.tiff",
#        PCO_MaxN7, device = "tiff", dpi = 300, width = 11, height = 10)


# PCO plot Coastal_Oceanic ------------------------------------------------

#Plotting biomass, method, fishing and arrows for species with largest biomass
PCO_MaxN8 <- ggplot(CAP) + 
  #Adding color and shapes # shape = Zone, can be added when I have zonation ready
  geom_point(aes(CAP1, CAP2, color = Location, shape = Coastal_Oceanic), stroke = 2, size = 4.5) + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(4, 2)) +
  geom_path(data = ordi$df_ellipse, aes(x = x, y = y, colour = Group), size = 1) +
  guides(color = guide_legend(order = 1, title = "MPA"),
         shape = guide_legend(order = 2, title = "Status")) +
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
  #Adding red arrows
  geom_segment(data = arrows_df2, 
               x = Anchor[1], y = Anchor[2],
               mapping = aes(xend = X3, yend = Y3),
               arrow = arrow(length = unit(2, "mm")), #Adding arrow head
               size = 0.8, color = "red") +
  #Adding red arrow labels
  geom_text_repel(data = arrows_df2, aes(label = variable),
                  size = 7, fontface = "bold", color = "red",
                  lineheight = 0.6,
                  x = X3, y = Y3,
                  box.padding = 0.5) +
  theme_classic() +
  #Adding percentages for the PCO axes
  xlab(paste0("CAP1 (",
              as.character(as.numeric(format(
                round(MaxN_cap$manova$Eigenvalues[1]/sum(MaxN_cap$manova$Eigenvalues), 3)))*100),
              "% of total variation)")) +
  ylab(paste0("CAP2 (",
              as.character(as.numeric(format(
                round(MaxN_cap$manova$Eigenvalues[2]/sum(MaxN_cap$manova$Eigenvalues), 3)))*100),
              "% of total variation)")) +
  scale_x_continuous(breaks = seq(-0.4, 0.8, by = 0.2), limits = c(-0.4, 0.8)) +
  scale_y_continuous(breaks = seq(-0.4, 0.4, by = 0.2), limits = c(-0.4, 0.4)) +
  #moving legend in plot and making box around it
  theme(legend.position = c(0.9, 0.33), 
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

PCO_MaxN8

Plot7_8 <- ggarrange(PCO_MaxN7, NULL, PCO_MaxN8, widths = c(1, 0.1, 1), nrow = 1)

Plot7_8

#Saving PCO for biomass - method and management status
ggsave("Figures/PCO_NoSharks.tiff",
       Plot7_8, device = "tiff", dpi = 300, width = 21, height = 10)


# Data from Fish Community ------------------------------------------------

PCO_NotSharks <- PCO_MaxNFish

Mat_NotSharks <- MaxN_mat

Dist_NotSharks <- MaxN_mat_dist


#Remove unnecessary variables
rm(MaxN_mat_pco, PCO_MaxNFish, species_pco_arrows, Anchor, 
   Corr_species, Corr_short, K, X2, Y2, compute_arrows, arrows_df, hull_MaxN, ordi)
