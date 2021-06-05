###############################################################################################################
# Title: Richness calculations and visualisations
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Visualisation small predators -------------------------------------------

library(ggrepel)

#Small predators data frame for visualisation
SPredators <- MaxN %>%
  filter(TrophicLevel >= 3.75) %>% 
  select(ValidName, MaxLgth_mm, TrophicLevel) %>% 
  filter(MaxLgth_mm <= 500) %>% 
  unique()

#Visualising small predators
ggplot(data = SPredators, aes(x = MaxLgth_mm, y = TrophicLevel)) +
  geom_point() +
  geom_vline(xintercept = 300, linetype="longdash", 
             color = "red", size=1) +
  geom_label_repel(aes(label = ValidName),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   min.segment.length = unit(0, 'lines'),
                   #nudge_y = 0.07,
                   segment.color = 'black') +
  scale_x_continuous(limits = c(0, 510)) +
  theme_classic()

rm(SPredators)

# Species richness calculations all fish -------------------------------------------

#Calculating species richness from MaxN
Richness <- MaxN %>% 
  #Selecting columns to be used
  select(Replicate, ValidName, MaxN, Location, Record_duration) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #group by site
  group_by(Replicate) %>% 
  #Calculating richness per site
  mutate(Richness_rep = length(unique(na.omit(ValidName)))) %>%
  #Calculating richness per site
  mutate(Ric_rep_hr = length(unique(na.omit(ValidName)))/hours) %>%
  #group by location
  group_by(Location) %>% 
  #Calculating Total richness per location
  mutate(Ric_total_loc = length(unique(na.omit(ValidName)))) %>%
  #Calculating MEAN richness per location
  mutate(Richness_loc = mean(na.omit(Richness_rep))) %>%
  #Calculating standard errors
  mutate(SE_loc = sd(Richness_rep)/sqrt(length(Richness_rep))) %>%
  #Removing unnecessary columns
  select(-c(ValidName, MaxN)) %>% 
  unique() %>% 
  ungroup()

RicPlots <- Richness %>% 
  select(1:6) %>% 
  group_by(Location) %>% 
  mutate(Mean_ric_loc = mean(Ric_rep_hr)) %>% 
  mutate(SE = sd(Ric_rep_hr)/sqrt(length(Ric_rep_hr))) %>% 
  left_join(SiteInfo %>% select(-c(3:6)), by = "Location") %>% 
  ungroup()


# Visualisations all fish ----------------------------------------

#Barplot of species richness per hour for locations
P01 <- ggplot(RicPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of species/hour (all fish)") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P01

#Barplot of species richness per hour for locations
P02 <- ggplot(RicPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of species/hour (all fish)") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P02


# Species richness calculations FishCom -------------------------------------------

#Calculating species richness from MaxN
RicCom <- FishCom %>% 
  #Selecting columns to be used
  select(Replicate, ValidName, MaxN, Location, Record_duration) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #group by site
  group_by(Replicate) %>% 
  #Calculating richness per site
  mutate(Richness_rep = length(unique(na.omit(ValidName)))) %>%
  #Calculating richness per site
  mutate(Ric_rep_hr = length(unique(na.omit(ValidName)))/hours) %>%
  #group by location
  group_by(Location) %>% 
  #Calculating Total richness per location
  mutate(Ric_total_loc = length(unique(na.omit(ValidName)))) %>%
  #Calculating MEAN richness per location
  mutate(Richness_loc = mean(na.omit(Richness_rep))) %>%
  #Calculating standard errors
  mutate(SE_loc = sd(Richness_rep)/sqrt(length(Richness_rep))) %>%
  #Removing unnecessary columns
  select(-c(ValidName, MaxN)) %>% 
  unique() %>% 
  ungroup()

ComPlots <- RicCom %>% 
  select(1:6) %>% 
  group_by(Location) %>% 
  mutate(Mean_ric_loc = mean(Ric_rep_hr)) %>% 
  mutate(SE = sd(Ric_rep_hr)/sqrt(length(Ric_rep_hr))) %>% 
  left_join(SiteInfo %>% select(-c(3:6)), by = "Location") %>% 
  ungroup()


# Visualisations FishCom ----------------------------------------

#Barplot of species richness per hour for locations
P1 <- ggplot(ComPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of species/hour") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P1

#Barplot of species richness per hour for locations
P2 <- ggplot(ComPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  #facet_grid(Protection_status~., scales = "free", space = "free") +
  labs(y = "Mean number of species/hour") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P2


# Visualisations predators ------------------------------------------------

#Calculating species richness from MaxN
RicPred <- Predators %>% 
  #Selecting columns to be used
  select(Replicate, ValidName, MaxN, Location, Record_duration) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #group by site
  group_by(Replicate) %>% 
  #Calculating richness per site
  mutate(Richness_rep = length(unique(na.omit(ValidName)))) %>%
  #Calculating richness per site
  mutate(Ric_rep_hr = length(unique(na.omit(ValidName)))/hours) %>%
  #group by location
  group_by(Location) %>% 
  #Calculating Total richness per location
  mutate(Ric_total_loc = length(unique(na.omit(ValidName)))) %>%
  #Calculating MEAN richness per location
  mutate(Richness_loc = mean(na.omit(Richness_rep))) %>%
  #Calculating standard errors
  mutate(SE_loc = sd(Richness_rep)/sqrt(length(Richness_rep))) %>%
  #Removing unnecessary columns
  select(-c(ValidName, MaxN)) %>% 
  unique() %>% 
  ungroup()

PredPlots <- RicPred %>% 
  select(1:6) %>% 
  group_by(Location) %>% 
  mutate(Mean_ric_loc = mean(Ric_rep_hr)) %>% 
  mutate(SE = sd(Ric_rep_hr)/sqrt(length(Ric_rep_hr))) %>% 
  left_join(SiteInfo %>% select(-c(3:6)), by = "Location") %>% 
  ungroup()


#Barplot of species richness per hour for locations
P3 <- ggplot(PredPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  labs(y = "Mean number of predatory species/hour") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P3

#Barplot of species richness per hour for locations
P4 <- ggplot(PredPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  labs(y = "Mean number of predatory species/hour") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P4


# Visualisations sharks ------------------------------------------------

#Calculating species richness from MaxN
RicSharks <- Sharks %>% 
  #Selecting columns to be used
  select(Replicate, ValidName, MaxN, Location, Record_duration) %>%
  #Recode NA in Record_duration to 90 min - Are we sure they are meant to be 90 minutes in length? - DFA
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #group by site
  group_by(Replicate) %>% 
  #Calculating richness per site
  # mutate(Richness_rep = length(unique(na.omit(ValidName)))) %>%
  mutate(Richness_rep = n(), #n() counts rows based on groups defined under group_by()
        #Calculating richness per site
         Ric_rep_hr = Richness_rep/hours) %>% 
  # mutate(Ric_rep_hr = length(unique(na.omit(ValidName)))/hours) %>% #there is no need to use mutate twice
  #or to calculate the same value twice, just use the column previously created - DFA
  #group by location
  group_by(Location) %>% 
  #Calculating Total richness per location
  mutate(Ric_total_loc = length(unique(na.omit(ValidName)))) %>%
  #Calculating MEAN richness per location
  mutate(Richness_loc = mean(na.omit(Richness_rep))) %>%
  #Calculating standard errors
  mutate(SE_loc = sd(Richness_rep)/sqrt(length(Richness_rep))) %>%
  #Removing unnecessary columns
  select(-c(ValidName, MaxN)) %>% 
  unique() %>% 
  ungroup()

SharkPlots <- RicSharks %>% 
  select(1:6) %>% 
  group_by(Location) %>% 
  mutate(Mean_ric_loc = mean(Ric_rep_hr)) %>% 
  mutate(SE = sd(Ric_rep_hr)/sqrt(length(Ric_rep_hr))) %>% 
  left_join(SiteInfo %>% select(-c(3:6)), by = "Location") %>% 
  ungroup()


#Barplot of species richness per hour for locations
P5 <- ggplot(SharkPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Protection_status, levels=c("Low","Medium","High"))~., 
             scales = "free", space = "free") +
  labs(y = "Mean number of shark species/hour") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P5

#Barplot of species richness per hour for locations
P6 <- ggplot(SharkPlots, aes(x = Location, y = Mean_ric_loc, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_grid(factor(Coastal_Oceanic)~., scales = "free", space = "free") +
  labs(y = "Mean number of shark species/hour") +
  geom_errorbar(aes(ymin = Mean_ric_loc-SE, ymax = Mean_ric_loc+SE, width = 0.5), color = "grey20") +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "none")
P6


# Combining richness plots ------------------------------------------------

library(ggpubr)

RicPlot <- ggarrange(P01, P02, P1, P2, P3, P4, P5, P6, ncol = 2, nrow = 4)

RicPlot

#Saving PCO for biomass - method and management status
ggsave("Figures/MeanRichness.tiff",
       RicPlot, device = "tiff", dpi = 300, width = 21, height = 10)

#Remove unecessary variables
rm(P01, P02, P1, P2, P3, P4, P5, P6, RicPlots, ComPlots, PredPlots, SharkPlots)


