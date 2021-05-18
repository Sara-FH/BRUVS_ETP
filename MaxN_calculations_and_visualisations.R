###############################################################################################################
# Title: MaxN Calculations and visualisations
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################

# MaxN calculations all fish -------------------------------------------

#Calculating MaxN per hour for each of the replicates and for specie per replicate
MaxNFish <- MaxN %>% 
  #Selecting columns to be used
  select(Replicate, Family, Genus, Species, ValidName, MaxN, Record_duration, Location, TrophicLevel) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #MaxN per hour column
  mutate(MaxN_hr = MaxN/hours) %>%
  #group by replicate
  group_by(Replicate) %>% 
  #Calculating MaxN per replicate per hour
  mutate(MaxN_rep = sum(MaxN_hr)) %>% #MaxN per replicate per hour
  #group by Replicate and ValidName
  group_by(Replicate, ValidName) %>% 
  #Calculating MaxN per species per replicate
  mutate(MaxN_sp = MaxN_hr) %>% 
  #group by location
  group_by(Location) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_tot_loc = sum(MaxN_hr)) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_mean_loc = mean(MaxN_hr)) %>%
  #Calculating standard errors
  mutate(SE = sd(MaxN_hr)/sqrt(length(MaxN_hr))) %>%
  #Joining protection status and coastal_oceanic
  left_join(SiteInfo %>% select(-c(3:6)), by = c("Location", "Replicate")) %>% 
  ungroup()


# Visualisations all fish ----------------------------------------

#Barplot of species richness per hour for locations
P01 <- ggplot(MaxNFish, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of individuals/hour (all fish)") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P01

#Barplot of species richness per hour for locations
P02 <- ggplot(MaxNFish, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of individuals/hour (all fish)") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P02


# MaxN calculations FishCom -------------------------------------------

#Calculating MaxN per hour for each of the replicates and for specie per replicate
MaxNCom <- FishCom %>% 
  #Selecting columns to be used
  select(Replicate, Family, Genus, Species, ValidName, MaxN, Record_duration, Location, TrophicLevel) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #MaxN per hour column
  mutate(MaxN_hr = MaxN/hours) %>%
  #group by replicate
  group_by(Replicate) %>% 
  #Calculating MaxN per replicate per hour
  mutate(MaxN_rep = sum(MaxN_hr)) %>% #MaxN per replicate per hour
  #group by Replicate and ValidName
  group_by(Replicate, ValidName) %>% 
  #Calculating MaxN per species per replicate
  mutate(MaxN_sp = MaxN_hr) %>% 
  #group by location
  group_by(Location) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_tot_loc = sum(MaxN_hr)) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_mean_loc = mean(MaxN_hr)) %>%
  #Calculating standard errors
  mutate(SE = sd(MaxN_hr)/sqrt(length(MaxN_hr))) %>%
  #Joining protection status and coastal_oceanic
  left_join(SiteInfo %>% select(-c(3:6)), by = c("Location", "Replicate")) %>% 
  ungroup()


# Visualisations FishCom ----------------------------------------

#Barplot of species richness per hour for locations
P1 <- ggplot(MaxNCom, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of individuals/hour") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P1

#Barplot of species richness per hour for locations
P2 <- ggplot(MaxNCom, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of individuals/hour") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P2


# MaxN calculations Predators -------------------------------------------

#Calculating MaxN per hour for each of the replicates and for specie per replicate
MaxNPred <- Predators %>% 
  #Selecting columns to be used
  select(Replicate, Family, Genus, Species, ValidName, MaxN, Record_duration, Location, TrophicLevel) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #MaxN per hour column
  mutate(MaxN_hr = MaxN/hours) %>%
  #group by replicate
  group_by(Replicate) %>% 
  #Calculating MaxN per replicate per hour
  mutate(MaxN_rep = sum(MaxN_hr)) %>% #MaxN per replicate per hour
  #group by Replicate and ValidName
  group_by(Replicate, ValidName) %>% 
  #Calculating MaxN per species per replicate
  mutate(MaxN_sp = MaxN_hr) %>% 
  #group by location
  group_by(Location) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_tot_loc = sum(MaxN_hr)) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_mean_loc = mean(MaxN_hr)) %>%
  #Calculating standard errors
  mutate(SE = sd(MaxN_hr)/sqrt(length(MaxN_hr))) %>%
  #Joining protection status and coastal_oceanic
  left_join(SiteInfo %>% select(-c(3:6)), by = c("Location", "Replicate")) %>% 
  ungroup()


# Visualisations Predators ------------------------------------------------

#Barplot of species richness per hour for locations
P3 <- ggplot(MaxNPred, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  labs(y = "Mean number of predatory fish/hour") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P3

#Barplot of species richness per hour for locations
P4 <- ggplot(MaxNPred, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  labs(y = "Mean number of predatory fish/hour") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P4


# MaxN calculations Sharks -------------------------------------------

#Calculating MaxN per hour for each of the replicates and for specie per replicate
MaxNSharks <- Sharks %>% 
  #Selecting columns to be used
  select(Replicate, Family, Genus, Species, ValidName, MaxN, Record_duration, Location, TrophicLevel) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #MaxN per hour column
  mutate(MaxN_hr = MaxN/hours) %>%
  #group by replicate
  group_by(Replicate) %>% 
  #Calculating MaxN per replicate per hour
  mutate(MaxN_rep = sum(MaxN_hr)) %>% #MaxN per replicate per hour
  #group by Replicate and ValidName
  group_by(Replicate, ValidName) %>% 
  #Calculating MaxN per species per replicate
  mutate(MaxN_sp = MaxN_hr) %>% 
  #group by location
  group_by(Location) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_tot_loc = sum(MaxN_hr)) %>%
  #Calculating TOTAL MaxN per location
  mutate(MaxN_mean_loc = mean(MaxN_hr)) %>%
  #Calculating standard errors
  mutate(SE = sd(MaxN_hr)/sqrt(length(MaxN_hr))) %>%
  #Joining protection status and coastal_oceanic
  left_join(SiteInfo %>% select(-c(3:6)), by = c("Location", "Replicate")) %>% 
  ungroup()


# Visualisations Sharks ---------------------------------------------------

#Barplot of species richness per hour for locations
P5 <- ggplot(MaxNSharks, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  labs(y = "Mean number of shark species/hour") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P5

#Barplot of species richness per hour for locations
P6 <- ggplot(MaxNSharks, aes(x = Location, y = MaxN_mean_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  labs(y = "Mean number of shark species/hour") +
  geom_errorbar(aes(ymin = MaxN_mean_loc-SE, ymax = MaxN_mean_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P6


# Combining MaxN plots ------------------------------------------------

library(ggpubr)

MaxNPlot <- ggarrange(P01, P02, P1, P2, P3, P4, P5, P6, ncol = 2, nrow = 4)

MaxNPlot

#Saving PCO for biomass - method and management status
ggsave("Figures/MeanMaxN.tiff",
       MaxNPlot, device = "tiff", dpi = 300, width = 21, height = 10)

#Remove unnecessary variables
rm(P01, P02, P1, P2, P3, P4, P5, P6)
