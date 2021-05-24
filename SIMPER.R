
# SIMPER Predators --------------------------------------------------------

#Making Matrix
MaxN_mat <- MaxNPred %>% 
  select(Replicate, ValidName, Location, MaxN_sp) %>% 
  #Using only unique values of MaxN per species per Replicate
  unique() %>%
  #Adding the Location to the Replicate name (I split them later for the plot)
  unite(RepLoc, Replicate, Location, sep = " ") %>%
  #Making the format right for the matrix
  pivot_wider(names_from = "ValidName", values_from = "MaxN_sp") %>%
  #Adding dummy species to all sites, to enable dissimilarity calculations for empty sites
  rename("Dummy" = "NA") %>% 
  mutate(Dummy = 0.66) %>% 
  arrange(RepLoc) %>% #Arranging Replicate names
  column_to_rownames("RepLoc") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at the Replicate.

#Square root transformation
MaxN_mat <- MaxN_mat^0.5

#Data frame for SIMPER
df_pred <- as.data.frame(MaxN_mat)
df_pred <- df_pred %>% 
  rownames_to_column(var = "Replicate") %>% 
  select(Replicate) %>% 
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

#SIMPER predatory species protection status
(simPred <- with(df_pred, simper(MaxN_mat, Protection_status)))
summary(simPred)

#Making data frames for protection status comparisons
sim <- summary(simPred)
PredML <- as.data.frame(sim$Medium_Low)
PredML <- PredML %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)

PredMH <- as.data.frame(sim$High_Medium)
PredMH <- PredMH %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)

PredHL <- as.data.frame(sim$High_Low)
PredHL <- PredHL %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)

#SIMPER predatory species province
(simPred2 <- with(df_pred, simper(MaxN_mat, Coastal_Oceanic)))
summary(simPred2)

#Making data frames for province
sim2 <- summary(simPred2)
PredCO <- as.data.frame(sim2$Oceanic_Coastal)
PredCO <- PredCO %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)


# SIMPER Sharks --------------------------------------------------------

#Making Matrix
MaxN_mat <- MaxNSharks %>% 
  select(Replicate, ValidName, Location, MaxN_sp) %>% 
  #Using only unique values of MaxN per species per Replicate
  unique() %>%
  #Adding the Location to the Replicate name (I split them later for the plot)
  unite(RepLoc, Replicate, Location, sep = " ") %>%
  #Making the format right for the matrix
  pivot_wider(names_from = "ValidName", values_from = "MaxN_sp") %>%
  #Adding dummy species to all sites, to enable dissimilarity calculations for empty sites
  rename("Dummy" = "NA") %>% 
  mutate(Dummy = 0.66) %>% 
  arrange(RepLoc) %>% #Arranging Replicate names
  column_to_rownames("RepLoc") %>% #Making a column into row names for the matrix
  as.matrix() %>% 
  replace_na(0) #Putting 0 instead of NA, when the species was not observed at the Replicate.
#Transformation not necessary

#Data frame for SIMPER
df_sharks <- as.data.frame(MaxN_mat)
df_sharks <- df_sharks %>% 
  rownames_to_column(var = "Replicate") %>% 
  select(Replicate) %>% 
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

#SIMPER predatory species protection status
(simSharks <- with(df_sharks, simper(MaxN_mat, Protection_status)))
summary(simSharks)

#Making data frames for protection status comparisons
sim <- summary(simSharks)
SharksML <- as.data.frame(sim$Medium_Low)
SharksML <- SharksML %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)

SharksMH <- as.data.frame(sim$High_Medium)
SharksMH <- SharksMH %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)

SharksHL <- as.data.frame(sim$High_Low)
SharksHL <- SharksHL %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)

#SIMPER predatory species province
(simSharks2 <- with(df_sharks, simper(MaxN_mat, Coastal_Oceanic)))
summary(simSharks2)

#Making data frames for province
sim2 <- summary(simSharks2)
SharksCO <- as.data.frame(sim2$Oceanic_Coastal)
SharksCO <- SharksCO %>% 
  mutate(contribution = round(c(cumsum[1], diff(cumsum))*100, digits = 2)) %>% 
  filter(ratio > 1 & contribution > 3)

