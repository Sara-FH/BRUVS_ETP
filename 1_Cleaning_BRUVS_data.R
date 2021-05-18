###############################################################################################################
# Title: Cleaning BRUVS data from ETP sites
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

{library(tidyverse)
}

#Note - remove record_duration from MaxN, then add at the end from SiteInfo

# Loading MaxN data from ETP sites ---------------------------------------------

#Load MaxN data from sites and remove unnecessary columns

#Malpelo 2015
Malpelo15_MaxN <- openxlsx::read.xlsx("Data/Malpelo_2015_MaxN.xlsx") %>% 
  select(-c(1:5, 7:9, 12:17, 21, 23)) %>% 
  #Rename columns with . in name
  rename("BRUVS_type" = "Bottom/Pelagic",
         "Replicate" = "OpCode") %>% 
  #Remove ? in site names
  mutate(Site = recode(Site, 
                       "Tres Mosqueteros?" = "Tres Mosqueteros")) %>% 
  #Keep only sites with bottom BRUVS (Bottom or Bottom!!!), removing pelagic BRUVS
  filter(BRUVS_type == "Bottom" | BRUVS_type == "Bottom!!!") %>% 
  #Drop column with BRUVS_type
  select(-c("BRUVS_type")) %>% 
  #Adding column for Island
  mutate(Location = "Malpelo")

#Malpelo 2018
Malpelo18_MaxN <- openxlsx::read.xlsx("Data/Malpelo_2018_MaxN.xlsx") %>% 
  select(-c(1:5, 7:9, 12:17, 21, 23)) %>% 
  #Rename columns with . in name
  rename("BRUVS_type" = "Bottom/Pelagic",
         "Replicate" = "OpCode") %>% 
  #Recode site name NA to Pared Naufrago (because they are in the wrong column)
  mutate(Site = replace_na(Site, "Pared Naufrago")) %>% 
  #Remove BRUVS_type as the only data was the site name which was wrong
  select(-c("BRUVS_type")) %>%
  #Adding column for Island
  mutate(Location = "Malpelo")

#Revillagigedo 2016
Revilla16_MaxN <- openxlsx::read.xlsx("Data/Revillagigedo_2016_MaxN.xlsx") %>% 
  select(-c(1:5, 7:10, 12:17, 21, 23)) %>% 
  #Rename columns with . in name
  rename("Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "Revilla")
  
#Clipperton 2016
Clip16_MaxN <- openxlsx::read.xlsx("Data/Clipperton_2016_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 11, 13)) %>%
  #Adding column for Location
  mutate(Location = "Clip") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA)


#Parque Nacional Machalilla
PNM19_MaxN <- openxlsx::read.xlsx("Data/Machalilla_2019_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 11, 13)) %>%
  #Adding column for Location
  mutate(Location = "PNM") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA) %>% 
  #Remove replicates with too bad visibility (cannot see bait canister)
  filter(! (Replicate == "PNM15_P4_20190409" | Replicate == "PNM21_P1_20190509"))

#Galera San Francisco Marine Reserve
GSF19_MaxN <- openxlsx::read.xlsx("Data/GSF_2019_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 11, 13)) %>%
  #Adding column for Location
  mutate(Location = "GSF") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA) %>% 
  #Remove replicates with too bad visibility (cannot see bait canister)
  filter(! (Replicate == "GSF7repeat_P1_20190831" | Replicate == "GSF11_P4_20190829"))

#Caño Island
Cano19_MaxN <- openxlsx::read.xlsx("Data/Cano_2019_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 11, 13)) %>%
  #Adding column for Location
  mutate(Location = "Cano") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA) %>% 
  #Recoding wrongly named replicates
  mutate(Replicate = recode(Replicate, 
                            "IC6200319B2-2" = "IC6200319B2", 
                            "IC4180319B2-2" = "IC4180319B2", 
                            "IC5190319B2-2" = "IC5190319B2"))

#Darwin Island, Galapagos Marine Reserve
Darwin16_MaxN <- openxlsx::read.xlsx("Data/Darwin_2016_MaxN.xlsx") %>% 
  select(-c(1:5, 7:9, 12:17, 21, 23)) %>%
  #Adding column for Location
  mutate(Location = "GMR") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom" | `Bottom/Pelagic` == "B") %>% 
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)

#Darwin Island, Galapagos Marine Reserve
Wolf16_MaxN <- openxlsx::read.xlsx("Data/Wolf_2016_MaxN.xlsx") %>% 
  select(-c(1:5, 7:9, 12:17, 21, 23)) %>%
  #Adding column for Location
  mutate(Location = "GMR") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom" | `Bottom/Pelagic` == "B") %>%
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)

#Darwin Island, Galapagos Marine Reserve
Darwin17_MaxN <- openxlsx::read.xlsx("Data/Darwin_2017_MaxN.xlsx") %>% 
  select(-c(1:5, 7:9, 12:16, 20, 22)) %>%
  #Adding column for Location
  mutate(Location = "GMR") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom") %>% 
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)

#Darwin Island, Galapagos Marine Reserve
Wolf17_MaxN <- openxlsx::read.xlsx("Data/Wolf_2017_MaxN.xlsx") %>% 
  select(-c(1:5, 7:9, 12:17, 21, 23)) %>%
  #Adding column for Location
  mutate(Location = "GMR") %>% 
  #Rename OpCode to replicates, because it is the unique replicate name
  rename("Replicate" = "OpCode") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom") %>%
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)


# Loading Length data from ETP sites ---------------------------------------------

#Load Length data from sites and remove unnecessary columns

#Malpelo 2015
Malpelo15_Length <- openxlsx::read.xlsx("Data/Malpelo_2015_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17:19, 22:24, 25:27, 31, 33:35)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "BRUVS_type" = "Bottom/Pelagic", 
         "Replicate" = "OpCode") %>%
  #Remove ? in site names
  mutate(Site = recode(Site, 
                       "Tres Mosqueteros?" = "Tres Mosqueteros")) %>%
  #Keep only sites with bottom BRUVS (Bottom or Bottom!!!), removing pelagic BRUVS
  filter(BRUVS_type == "Bottom" | BRUVS_type == "Bottom!!!") %>% 
  #Drop column with BRUVS_type
  select(-c("BRUVS_type")) %>%
  #Adding column for Island
  mutate(Location = "Malpelo")

#Malpelo 2018
Malpelo18_Length <- openxlsx::read.xlsx("Data/Malpelo_2018_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17:19, 22:24, 25:27, 31, 33:35)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)",
         "BRUVS_type" = "Bottom/Pelagic", 
         "Replicate" = "OpCode") %>% 
  #Recode site name NA to Pared Naufrago (because they are in the wrong column)
  mutate(Site = replace_na(Site, "Pared Naufrago")) %>% 
  #Remove BRUVS_type as the only data was the site name which was wrong
  select(-c("BRUVS_type")) %>% 
  #Adding column for Island
  mutate(Location = "Malpelo")

#Revillagigedo 2016
Revilla16_Length <- openxlsx::read.xlsx("Data/Revillagigedo_2016_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17:20, 22:24, 25:27, 31, 33:37)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "Revilla")

#Clipperton 2016
Clip16_Length <- openxlsx::read.xlsx("Data/Clipperton_2016_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 21, 23:25)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "Clip") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA)

#Parque Nacional Machalilla
PNM19_Length <- openxlsx::read.xlsx("Data/Machalilla_2019_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 21, 23:25)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "PNM") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA) %>% 
  #Remove replicates with too bad visibility (cannot see bait canister)
  filter(! (Replicate == "PNM15_P4_20190409" | Replicate == "PNM21_P1_20190509"))

##Galera San Francisco Marine Reserve
GSF19_Length <- openxlsx::read.xlsx("Data/GSF_2019_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 21, 23:25)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "GSF") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA) %>% 
  #Remove replicates with too bad visibility (cannot see bait canister)
  filter(! (Replicate == "GSF7repeat_P1_20190831" | Replicate == "GSF11_P4_20190829"))

##Caño Island
Cano19_Length <- openxlsx::read.xlsx("Data/Cano_2019_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 21, 23:25)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "Cano") %>% 
  #Adding column for site - Question can be changed when missing site is figured out
  mutate(Site = NA)

##Darwin Island, Galapagos Marine Reserve
Darwin16_Length <- openxlsx::read.xlsx("Data/Darwin_2016_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17:19, 22:27, 31, 33:37)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "GMR") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom") %>%
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)

##Wolf Island, Galapagos Marine Reserve
Wolf16_Length <- openxlsx::read.xlsx("Data/Wolf_2016_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17:19, 22:27, 31, 33:37)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "GMR") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom") %>%
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)

##Darwin Island, Galapagos Marine Reserve
Darwin17_Length <- openxlsx::read.xlsx("Data/Darwin_2017_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17:19, 22:26, 30, 32:34)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "GMR") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom") %>%
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)

##Wolf Island, Galapagos Marine Reserve
Wolf17_Length <- openxlsx::read.xlsx("Data/Wolf_2017_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17:19, 22:27, 31, 33:35)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Replicate" = "OpCode") %>% 
  #Adding column for Island
  mutate(Location = "GMR") %>% 
  #Removing pelagic BRUVS
  filter(`Bottom/Pelagic` == "Bottom") %>%
  #Remove column Bottom/Pelagic
  select(-`Bottom/Pelagic`)


# Adding missing individuals ----------------------------------------------

#Checking for species only in length and not in MaxN
Malpelo15_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Malpelo15_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Malpelo18_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Malpelo18_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Cano19_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Cano19_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Clip16_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Clip16_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Darwin16_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Darwin16_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Wolf16_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Wolf16_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

GSF19_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(GSF19_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

PNM19_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(PNM19_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Revilla16_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Revilla16_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Darwin17_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Darwin17_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

Wolf17_Length %>% select(Family, Genus, Species) %>% unique() %>% 
  anti_join(Wolf17_MaxN %>% select(Family, Genus, Species) %>% unique()) %>% 
  filter(! is.na(Species)) %>% filter(! Species == "sp")

#temporary data frame for missing species to Malpelo18 MaxN after checking the videos
temp <- Malpelo18_Length %>% 
  filter(Family == "Carcharhinidae" &	Genus == "Carcharhinus" & Species == "limbatus") %>% 
  rbind(Malpelo18_Length %>% 
          filter(Family == "Haemulidae" & Genus == "Haemulon" & Species == "sexfasciatum")) %>% 
  select(5:11) %>% 
  #ordering columns
  select(1, 2, 7, everything()) %>% 
  #renaming number to MaxN
  rename("MaxN" = "Number")

#Adding missing species to Malpelo18 MaxN after checking the videos
Malpelo18_MaxN <- Malpelo18_MaxN %>% 
  rbind(temp)

#temporary data frame for missing species to Revilla16 MaxN after checking the videos
temp <- Revilla16_Length %>% 
  filter(Family == "Chaetodontidae" &	Genus == "Prognathodes" & Species == "falcifer") %>% 
  rbind(Revilla16_Length %>% 
          filter(Family == "Carcharhinidae" & Genus == "Carcharhinus" & Species == "obscurus")) %>% 
  select(5:11) %>% 
  #ordering columns
  select(1, 2, 7, everything()) %>% 
  #MaxN per site
  group_by(Replicate, Species) %>% 
  mutate(MaxN = sum(Number)) %>% 
  unique() %>% 
  #removing number
  select(-Number) %>% 
  ungroup()

#Adding missing species to Revilla16 MaxN after checking the videos
Revilla16_MaxN <- Revilla16_MaxN %>% 
  rbind(temp)

#removing temporary variable
rm(temp)


# Load supplementary data -------------------------------------------------

#Loading MPA status and oceanic or coastal location of MPA
MPA_status <- openxlsx::read.xlsx("Data/MPA_status.xlsx")

#Site Info Clipperton
SiteInfo <- openxlsx::read.xlsx("Data/SiteInfo.xlsx") %>% 
  left_join(MPA_status, by = "Location")

#Access to the Fish data set with correct names, Maximum length and Trophicl level
FishDB <- openxlsx::read.xlsx("Data/FishDB_ETP.xlsx")
#Database updated in May 2021

#Vector containing names of non fish species
NonFish <- c("Eretmochelys imbricata", "Chelonia mydas", "Zalophus wollebaeki", 
             "Phalacrocorax harrisi", "Cardisoma crassum", "Panulirus gracilis",
             "Scyllarides astori", "Spheniscus mendiculus", "Arctocephalus galapagoensis",
             "Lepidochelys olivacea", "Cheloniidae sp", "Delphinidae sp")

# Combining MaxN and Length datasets and cleaning data --------------------

#Combining MaxN datasets
MaxN <- Malpelo15_MaxN %>% 
  rbind(Malpelo18_MaxN) %>% 
  rbind(Revilla16_MaxN) %>% 
  rbind(Clip16_MaxN) %>% 
  rbind(PNM19_MaxN) %>% 
  rbind(GSF19_MaxN) %>%
  rbind(Cano19_MaxN) %>% 
  rbind(Darwin16_MaxN) %>% 
  rbind(Wolf16_MaxN) %>% 
  rbind(Darwin17_MaxN) %>% 
  rbind(Wolf17_MaxN) %>%
  #Recode sp1-4 to sp
  mutate(Species = recode(Species, 
                          "sp1" = "sp", 
                          "sp2" = "sp", 
                          "sp3" = "sp",
                          "sp4" = "sp")) %>% 
  #Removing spaces in Genus and Species columns
  mutate(Genus = str_trim(Genus, side = "both"),
         Species = str_trim(Species, side = "both"),
         #Empty Species rows to sp
         Species = case_when(is.na(Species) ~ "sp",
                           TRUE ~ Species)) %>% 
  #Creating new column for the combined species name
  #If Genus is not empty, the new column will join Genus and Species
  mutate(SpeciesName = case_when(!is.na(Genus) ~ paste(Genus, Species, sep = " "),
                                 #If Genus is empty, then join Species and Family
                                 is.na(Genus) ~ paste(Family, Species, sep = " "))) %>% 
  #Remove columns with no family, genus, species, because they are sync points
  filter(!(is.na(Family) & is.na(Genus) & Species == "sp")) %>% 
  #Using join to keep correct names of species in DOVS data
  left_join(FishDB %>% select(ScientificName, ValidName, MaxLgth_mm, TrophicLevel, Shark),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Now we remove non fish species
  filter(!ValidName %in% NonFish) %>% #Chelonia mydas removed
  #Recode misspelled Family name "Tetradontiadae" to "Tetraodontiadae"
  mutate(Family = recode(Family, 
                         "Tetradontiadae" = "Tetraodontiadae"))

#temporary data frame for empty replicates
temp <- SiteInfo %>% 
  select(-c(Longitude, Latitude)) %>% 
  anti_join(MaxN, by = c("Location", "Replicate"))

#Keeping rows with highest MaxN for each species, duplicates due to species corrections
MaxN <- MaxN %>% 
  group_by(Replicate, ValidName) %>%
  unique() %>% 
  top_n(1, abs(MaxN)) %>% 
  ungroup() %>% 
  #Adding record_duration for replicates
  left_join(SiteInfo %>% select(-c(Longitude, Latitude, MaxN)), by = c("Location", "Replicate")) %>% 
  #Adding completely empty sites
  bind_rows(temp)

#Combining Length datasets
Length <- Malpelo15_Length %>% 
  rbind(Malpelo18_Length) %>% 
  rbind(Revilla16_Length) %>% 
  rbind(Clip16_Length) %>% 
  rbind(PNM19_Length) %>% 
  rbind(GSF19_Length) %>%
  rbind(Cano19_Length) %>% 
  rbind(Darwin16_Length) %>% 
  rbind(Wolf16_Length) %>%
  rbind(Darwin17_Length) %>% 
  rbind(Wolf17_Length) %>%
  #Recode sp1-4 to sp
  mutate(Species = recode(Species, 
                          "sp1" = "sp", 
                          "sp2" = "sp", 
                          "sp3" = "sp",
                          "sp4" = "sp")) %>% 
  #Removing spaces in Genus and Species columns
  mutate(Genus = str_trim(Genus, side = "both"),
         Species = str_trim(Species, side = "both"),
         #Empty Species rows to sp
         Species = case_when(is.na(Species) ~ "sp",
                             TRUE ~ Species)) %>% 
  #Creating new column for the combined species name
  #If Genus is not empty, the new column will join Genus and Species
  mutate(SpeciesName = case_when(!is.na(Genus) ~ paste(Genus, Species, sep = " "),
                                 #If Genus is empty, then join Species and Family
                                 is.na(Genus) ~ paste(Family, Species, sep = " "))) %>% 
  #Remove columns with no family, genus, species, because they are sync points
  filter(!(is.na(Family) & is.na(Genus) & Species == "sp")) %>% 
  #There are only one species present within the ETP for these 11 genera
  #Recode the 11 genera to specific species
  mutate(SpeciesName = recode(SpeciesName, 
                            "Dermatolepis sp" = "Dermatolepis dermatolepis", 
                            "Triaenodon sp" = "Triaenodon obesus", 
                            "Balistes sp" = "Balistes polylepis", 
                            "Sufflamen sp" = "Sufflamen verres", 
                            "Forcipiger sp" = "Forcipiger flavissimus", 
                            "Paranthias sp" = "Paranthias colonus", 
                            "Manta sp" = "Manta birostris", 
                            "Johnrandallia sp" = "Johnrandallia nigrirostris", 
                            "Aulostomus sp" = "Aulostomus chinensis", 
                            "Zanclus sp" = "Zanclus cornutus", 
                            "Canthidermis sp" = "Canthidermis maculata")) %>% 
  #Using join to keep correct names of species
  left_join(FishDB %>% select(ScientificName, ValidName, MaxLgth_mm, TrophicLevel, Shark),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Now we remove non fish species
  filter(!ValidName %in% NonFish) %>% #4 Chelonia mydas removed
  mutate(Length_mm = as.numeric(Length_mm)) #from character to numeric values


# Questions about MaxN and Length -----------------------------------------

#Question - are all BRUVS from Malpelo 2018 bottom? Because they are NA values in the data, and Malpelo 2015 
#did have pelagic BRUVS. So just double checking.

#Question - there are 29 unique genera measured where we do not have species name, do we need these?
Length %>% select(Family, Genus, Species, SpeciesName) %>% arrange(Family, Genus, Species) %>% 
         filter(endsWith(SpeciesName, "sp")) %>% unique() %>% count()

#Question - there are 270 individuals which are only categorized to genus level, do we need these?
Length %>% select(Family, Genus, Species, SpeciesName) %>% arrange(Family, Genus, Species) %>% 
  filter(endsWith(SpeciesName, "sp")) %>% count()


# Checking species names --------------------------------------------------

#Checking rows with value in SpeciesName but not in ValidName
Length %>% select(SpeciesName, ValidName) %>% filter(is.na(ValidName)) %>% unique()

#Cheking what species are in the data, but not in the FishDB
MaxN %>% select(Family, Genus, Species, SpeciesName, ValidName) %>% filter(is.na(ValidName)) %>% unique()
  

#Remove unnecessary variables
rm(Malpelo15_MaxN, Malpelo15_Length, Malpelo18_MaxN, Malpelo18_Length, Revilla16_MaxN, 
   Revilla16_Length, Clip16_MaxN, Clip16_Length, PNM19_MaxN, PNM19_Length, GSF19_MaxN, 
   GSF19_Length, Cano19_MaxN, Cano19_Length, Darwin16_MaxN, Darwin16_Length, Wolf16_MaxN, 
   Wolf16_Length, Darwin17_MaxN, Darwin17_Length, Wolf17_MaxN, Wolf17_Length)


# Splitting data into trophic levels --------------------------------------

#Small predators
Small <- MaxN %>%
  filter(TrophicLevel >= 3.75) %>% 
  filter(MaxLgth_mm < 300)

#Predatory fishes and sharks, trophic category Carnivore and apex predators
Predators <- MaxN %>%
  filter(TrophicLevel >= 3.75) %>% 
  anti_join(Small)

#Fish community
FishCom <- MaxN %>% 
  filter(TrophicLevel < 3.75) %>% 
  bind_rows(Small)

#Sharks
Sharks <- MaxN %>%
  filter(Shark == "Shark")

#Fish missing from the FishDB
MissingFish <- MaxN %>% 
  filter(is.na(TrophicLevel))
#Question - note to self: These are individuals identified to Family or Genus level

#Remove unnecessary variables
rm(MissingFish, Small)


# Adding empty sites ------------------------------------------------------


#temporary data frame with sites empty for fish with trophic level < 3.75
temp <- SiteInfo %>% 
  select(-c(Latitude, Longitude)) %>% 
  anti_join(FishCom, by = c("Location", "Replicate"))

#Adding sites empty for fish with trophic level < 3.75
FishCom <- FishCom %>% 
  bind_rows(temp)

#temporary data frame with sites empty for predators
temp <- SiteInfo %>% 
  select(-c(Latitude, Longitude)) %>% 
  anti_join(Predators, by = c("Location", "Replicate"))

#Adding sites empty for predators
Predators <- Predators %>% 
  bind_rows(temp)

#temporary data frame with sites empty for sharks
temp <- SiteInfo %>% 
  select(-c(Latitude, Longitude)) %>% 
  anti_join(Sharks, by = c("Location", "Replicate"))

#Adding sites empty for sharks
Sharks <- Sharks %>% 
  bind_rows(temp)

rm(temp)
