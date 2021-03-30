###############################################################################################################
# Title: Species richness, density and biomass calculations
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

library(tidyverse)



# MaxN updating species ---------------------------------------------------

#Using join to keep correct names of species in DOVS data
MaxN <- MaxN %>% 
  left_join(NewFishDB %>% select(SpeciesName, ValidName_new, a_new, b_new, LengthType_new, LenLenRatio_new), 
            by = c("SpeciesName" = "SpeciesName")) %>% 
  #Combine validNames from old and new FishDB data in updated ValidName
  mutate(ValidName_upd = coalesce(ValidName, ValidName_new)) %>% 
  #Combine a from old and new FishDB data in updated a
  mutate(a_upd = coalesce(a, a_new)) %>% 
  #Combine b from old and new FishDB data in updated b
  mutate(b_upd = coalesce(b, b_new)) %>% 
  #Combine LengthType from old and new FishDB data in updated LengthType
  mutate(LengthType_upd = coalesce(LengthType, LengthType_new)) %>% 
  #Combine LenLenRatio from old and new FishDB data in updated LenLenRatio
  mutate(LenLenRatio_upd = coalesce(as.numeric(LenLenRatio), LenLenRatio_new)) %>% 
  #Removing former FishDB data to keep updated columns
  select(-c(ValidName, ValidName_new, a, a_new, b, b_new, LengthType, LengthType_new, LenLenRatio, 
            LenLenRatio_new)) %>% 
  #Renaming columns to original names
  rename(ValidName = ValidName_upd, 
         a = a_upd, 
         b = b_upd, 
         LengthType = LengthType_upd, 
         LenLenRatio = LenLenRatio_upd)

#Checking
MaxN %>% select(SpeciesName, ValidName, a, b, LengthType, LenLenRatio) %>% 
  filter(is.na(ValidName)) %>% unique()
#Question - more species to add to new FishDB

# Calculating species richness -----------------------

Length %>% summarise(Number = sum(Number))
MaxN %>% summarise(MaxN = sum(MaxN))


#Calculating species richness
Richness <- Length %>% 
  #Selecting columns to be used
  select(Site, ValidName, Number, Location) %>%
  #group by site
  group_by(Site) %>% 
  #Calculating richness per site
  mutate(Richness_site = length(unique(na.omit(ValidName)))) %>%
  #group by location
  group_by(Location) %>% 
  #Calculating richness per location
  mutate(Richness_loc = length(unique(na.omit(ValidName)))) %>%
  #Removing unnecessary columns
  select(-c(ValidName, Number)) %>% 
  unique() %>% 
  ungroup()

#Barplot of species richness per site
ggplot(Richness, aes(x = Site, y = Richness_site, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(~Location, scales = "free_x", space = "free_x") +
  labs(x = "Sites", y = "Species/Site") +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#Calculating species richness in DOVS
Richness2 <- MaxN %>% 
  #Selecting columns to be used
  select(Site, ValidName, MaxN, Location) %>%
  #group by site
  group_by(Site) %>% 
  #Calculating richness per site
  mutate(Richness_site = length(unique(na.omit(ValidName)))) %>%
  #group by location
  group_by(Location) %>% 
  #Calculating richness per location
  mutate(Richness_loc = length(unique(na.omit(ValidName)))) %>%
  #Removing unnecessary columns
  select(-c(ValidName, MaxN)) %>% 
  unique() %>% 
  ungroup()

#Question - more species in MaxN than in Length??
#Barplot of species richness per site
ggplot(Richness2, aes(x = Site, y = Richness_site, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(~Location, scales = "free_x", space = "free_x") +
  labs(x = "Sites", y = "Species/Site") +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Calculating density -----------------------------------------------------

#Question - some sites do not have record_duration?? Right now I am setting them to 90 min

#Calculating MaxN per hour for each of the sites and for specie per site
Density <- MaxN %>% 
  #Selecting columns to be used
  select(Site, ValidName, MaxN, Record_duration, Location) %>%
  #Recode NA in Record_duration to 90 min
  mutate(Record_duration = replace_na(Record_duration, 90)) %>% 
  #calculating column with number of hours
  mutate(hours = Record_duration/60) %>% 
  #group by site
  group_by(Site) %>% 
  #Calculating MaxN per site per hour
  mutate(MaxN_site = sum(MaxN)/hours) %>% #MaxN per site per hour
  #group by Site and ValidName
  group_by(Site, ValidName) %>% 
  #Calculating MaxN per species per site
  mutate(MaxN_sp = sum(MaxN)/hours) %>% #MaxN per sp per site per hour
  ungroup()

#Barplot of species richness per site
ggplot(Density, aes(x = Site, y = MaxN_site, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(~Location, scales = "free_x", space = "free_x") +
  labs(x = "Sites", y = "MaxN/hr") +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Calculating biomass -----------------------------------------------------

#Question - some sites do not have record_duration?? Should Biomass be made per hour?

#Calculating biomass per species for each of the sites
Biomass <- Length %>% 
  #Removing individuals where we do not have length
  filter(! is.nan(Length_cm)) %>% 
  #Removing species that are only identified to genus level #Question - is this correct?
  filter(! endsWith(ValidName, "sp")) %>% 
  #Selecting columns to be used
  select(Site, ValidName, Length_cm, Number, Record_duration, Location, a, b, LengthType, 
         LenLenRatio) %>%
  #The fish biomass equation is W = a*L^b, therefore first transform the length to Fork Length 
  #then apply the exponent b and finally multiply by a.
  #The equation is therefore: a*((LenLenRatio*Length_cm)^b)
  mutate(Biomass = a*((LenLenRatio*Length_cm)^b)) %>% #Biomass for 1 individual in gram (all N are = 1)
  #group by site and validName
  group_by(Site, ValidName) %>% 
  #Calculating biomass per site per species
  mutate(Biomass_site_sp = sum(Biomass)) %>% 
  #group by site
  group_by(Site) %>% 
  #Calculating biomass per site
  mutate(Biomass_site = sum(Biomass))


#Barplot of species richness per site
ggplot(Biomass, aes(x = Site, y = Biomass_site/1000, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(~Location, scales = "free_x", space = "free_x") +
  labs(x = "Sites", y = "Biomass in kg/Site") +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
  


rm(FishDB, NewFishDB, NonFish)


