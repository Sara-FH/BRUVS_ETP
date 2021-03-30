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

# Loading data from ETP sites ---------------------------------------------

#Load MaxN data from sites and remove unnecessary columns

Malpelo15_MaxN <- openxlsx::read.xlsx("Data/Malpelo_2015_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 12:13, 15, 21, 23)) %>% 
  #Rename columns with . in name
  rename("Depth_ft" = "Depth.(ft)", 
         "BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Remove ? in site names
  mutate(Site = recode(Site, 
                       "Tres Mosqueteros?" = "Tres Mosqueteros")) %>% 
  #Recode BRUV_type to remove !
  mutate(BRUVS_type = recode(BRUVS_type, 
                             "Bottom!!!" = "Bottom")) %>% 
  #Recode record_duration time to min
  mutate(Record_duration = recode(Record_duration, 
                                  "1.3" = 90, 
                                  "1.30.00" = 90)) %>% 
  #Depth from feet to meters
  mutate(Depth = Depth_ft/3.2808) %>% 
  select(-Depth_ft) %>% 
  #Adding column for Island
  mutate(Location = "Malpelo15")

Malpelo18_MaxN <- openxlsx::read.xlsx("Data/Malpelo_2018_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 12:13, 15, 21, 23)) %>% 
  #Rename columns with . in name
  rename("BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.Duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Recode site name NA to Pared Naufrago
  mutate(Site = replace_na(Site, "Pared Naufrago")) %>% 
  #Remove site name from BRUVS_type column 
  mutate(BRUVS_type = na_if(BRUVS_type, "Pared Naufrago")) %>% 
  #Recode record_duration time to minutes
  mutate(Record_duration = recode(Record_duration, 
                                  "0.0625" = 90)) %>% 
  #Adding column for Island
  mutate(Location = "Malpelo18")

Revillagigedo16_MaxN <- openxlsx::read.xlsx("Data/Revillagigedo_2016_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 12:13, 15, 21, 23)) %>% 
  #Site names are not unique, therefore Opcode is used instead
  mutate(Site = OpCode) %>% #Question - can be changed when site names are updated
  #Rename columns with . in name
  rename("BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Recode record_duration time to min
  mutate(Record_duration = recode(Record_duration, 
                                  "1.30.00" = 90, 
                                  "1.10.00" = 70, 
                                  "1.30.00`" = 90, 
                                  "01.30.00" = 90)) %>% 
  #Adding column for Island
  mutate(Location = "Revillagigedo")

#Load MaxN data from sites and remove unnecessary columns

Malpelo15_Length <- openxlsx::read.xlsx("Data/Malpelo_2015_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 22:23, 25, 31, 33:35)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)", 
         "Depth_ft" = "Depth", 
         "BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.duration",
         "Bottom_type" = "Bottom.type") %>%
  #Remove ? in site names
  mutate(Site = recode(Site, 
                       "Tres Mosqueteros?" = "Tres Mosqueteros")) %>%
  #Recode record_duration time to minutes
  mutate(Record_duration = recode(Record_duration, 
                                  "1.3" = 90, 
                                  "1.30.00" = 90)) %>% 
  #Depth from feet to meters
  mutate(Depth = Depth_ft/3.2808) %>% 
  select(-Depth_ft) %>% 
  #Adding column for Island
  mutate(Location = "Malpelo15")

Malpelo18_Length <- openxlsx::read.xlsx("Data/Malpelo_2018_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 22:23, 25, 31, 33:35)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)",
         "BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.Duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Recode site name NA to Pared Naufrago
  mutate(Site = replace_na(Site, "Pared Naufrago")) %>% 
  #Remove site name from BRUVS_type column 
  mutate(BRUVS_type = na_if(BRUVS_type, "Pared Naufrago")) %>% 
  #Adding column for Island
  mutate(Location = "Malpelo18")

Revillagigedo16_Length <- openxlsx::read.xlsx("Data/Revillagigedo_2016_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 22:23, 25, 31, 33:37)) %>% 
  #Site names are not unique, therefore Opcode is used instead
  mutate(Site = OpCode) %>% #Question - can be changed when site names are updated
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision_mm" = "Precision.(mm)", 
         "RMS_mm" = "RMS.(mm)", 
         "Range_mm" = "Range.(mm)",
         "BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Recode record_duration time to min
  mutate(Record_duration = recode(Record_duration, 
                                  "1.30.00" = 90, 
                                  "1.10.00" = 70, 
                                  "1.30.00`" = 90, 
                                  "01.30.00" = 90)) %>% 
  #Adding column for Island
  mutate(Location = "Revillagigedo")

#Access to the Fish data set with correct names and a/b variables to calculate biomass
FishDB <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv")
#Database updated in January 2021

#Vector containing names of non fish species
NonFish <- c("Eretmochelys imbricata", "Chelonia mydas", "Zalophus wollebaeki", 
             "Phalacrocorax harrisi", "Cardisoma crassum", "Panulirus gracilis",
             "Scyllarides astori", "Spheniscus mendiculus", "Arctocephalus galapagoensis",
             "Lepidochelys olivacea")

# Combining MaxN and Length datasets and cleaning data --------------------

#Combining MaxN datasets
MaxN <- Malpelo15_MaxN %>% 
  rbind(Malpelo18_MaxN) %>% 
  rbind(Revillagigedo16_MaxN) %>% 
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
  left_join(FishDB %>% select(ScientificName, ValidName, a, b, LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Now we remove non fish species
  filter(!ValidName %in% NonFish) #4 Chelonia mydas removed

#Question - Do we keep these individuals?
MaxN %>% select(Family, Genus, Species, SpeciesName) %>% arrange(Family, Genus, Species) %>% 
  filter(Species == "sp") %>% unique()

#Combining Length datasets
Length <- Malpelo15_Length %>% 
  rbind(Malpelo18_Length) %>% 
  rbind(Revillagigedo16_Length) %>% 
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
  left_join(FishDB %>% select(ScientificName, ValidName, a, b, LengthType, LenLenRatio),
            by = c("SpeciesName" = "ScientificName")) %>% 
  #Now we remove non fish species
  filter(!ValidName %in% NonFish) %>% #4 Chelonia mydas removed
  mutate(Length_mm = as.numeric(Length_mm)) #from character to numeric values
  
#Question - Do we keep these individuals?
Length %>% select(Family, Genus, Species, SpeciesName) %>% arrange(Family, Genus, Species) %>% 
  filter(endsWith(SpeciesName, "sp")) %>% unique()

#Question - there are 24 unique genera measured where we do not have species name, should I use 
#mean length for that genus instead? Or should we delete these from the biomass analysis?
Length %>% select(Family, Genus, Species, SpeciesName) %>% arrange(Family, Genus, Species) %>% 
         filter(endsWith(SpeciesName, "sp")) %>% unique() %>% count()

#Question - there are 249 individuals which are only categorized to genus level, do we exclude them from
#the biomass analysis? what about abundance?
Length %>% select(Family, Genus, Species, SpeciesName) %>% arrange(Family, Genus, Species) %>% 
  filter(endsWith(SpeciesName, "sp")) %>% count()


# Checking species names --------------------------------------------------

#Checking rows with value in SpeciesName but not in ValidName
Length %>% select(SpeciesName, ValidName) %>% filter(is.na(ValidName)) %>% unique()

#Loading Extra species to add to FishDB data
NewFishDB <- openxlsx::read.xlsx("Data/NewSpecies_FishDB.xlsx")

#Using join to keep correct names of species in data
Length <- Length %>% 
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
Length %>% select(SpeciesName, ValidName, a, b, LengthType, LenLenRatio) %>% 
  filter(is.na(a)) %>% unique()


MaxN %>% select(Family, Genus, Species) %>% filter(Species == "sp") %>% unique()
#Carcharhinidae Carcharhinus      sp - 13 species
#Kyphosidae     Kyphosus      sp - 5 species
#Carangidae         <NA>      sp - 35 species
#Carangidae       Caranx      sp - 6 species
#Lutjanidae     Lutjanus      sp - 9 species

Length %>% select(Family, Genus, Species) %>% filter(Species == "sp") %>% unique()
#Labridae   Halichoeres      sp - 12 species
#Carcharhinidae  Carcharhinus      sp - 13 species
#Lutjanidae          <NA>      sp - 12 species
#Lutjanidae     Lutjanus      sp - 9 species
#Carangidae       Caranx      sp - 6 species
#Carangidae         <NA>      sp - 35 species
#Tripterygiidae  Lepidonectes      sp - 3 species
#Balistidae  Xanthichthys      sp - 2 species
#Acanthuridae    Acanthurus      sp - 5 species
#Serranidae  Dermatolepis      sp - 1 species: Dermatolepis dermatolepis
#Dasyatididae      Dasyatis      sp - 2 species
#Carcharhinidae    Triaenodon      sp - 1 species: Triaenodon obesus
#Balistidae      Balistes      sp - 1 species: Balistes polylepis
#Haemulidae   Anisotremus      sp - 4 species
#Carangidae       Seriola      sp - 3 species
#Balistidae     Sufflamen      sp - 1 species: Sufflamen verres
#Serranidae   Epinephelus      sp - 5 species
#Chaetodontidae    Forcipiger      sp - 1 species: Forcipiger flavissimus
#Acanthuridae     Prionurus      sp - 2 species
#Pomacanthidae   Holacanthus      sp - 3 species
#Serranidae    Paranthias      sp - 1 species: Paranthias colonus
#Mobulidae         Manta      sp - 1 species: Manta birostris
#Kyphosidae     Kyphosus      sp - 5 species
#Chaetodontidae Johnrandallia      sp - 1 species: Johnrandallia nigrirostris
#Tetradontiadae      Arothron      sp - 3 species
#Monocanthidae      Aluterus      sp - 2 species
#Labridae    Thalassoma      sp - 5 species
#Scaridae        Scarus      sp - 4 species
#Labridae      Bodianus      sp - 2 species
#Fistulariidae    Fistularia      sp - 2 species
#Aulostomidae    Aulostomus      sp - 1 species: Aulostomus chinensis
#Zanclidae       Zanclus      sp - 1 species: Zanclus cornutus
#Ophichthidae   Quassiremus      sp - 2 species
#Serranidae Cephalopholis      sp - 2 species
#Balistidae  Canthidermis      sp - 1 species: Canthidermis maculata

#Question: should I check for individual sites e.g. Malpelo to see if only one species is present in
#that area?

#There are only one species present within the ETP for the following 11 Genera: 
#Dermatolepis - Dermatolepis dermatolepis 
#Triaenodon - Triaenodon obesus 
#Balistes - Balistes polylepis
#Sufflamen - Sufflamen verres 
#Forcipiger - Forcipiger flavissimus 
#Paranthias - Paranthias colonus
#Manta - Manta birostris 
#Johnrandallia - Johnrandallia nigrirostris
#Aulostomus - Aulostomus chinensis
#Zanclus - Zanclus cornutus
#Canthidermis - Canthidermis maculata



# Environmental data ------------------------------------------------------

#Environmental variables for sites
EnvVar <- Malpelo15_MaxN %>% 
  select("Location", "Site", "Depth", "Comment", "BRUVS_type", "Record_duration", 
         "SST", "Bottom_type") %>% 
  unique() %>% 
  rbind(Malpelo18_MaxN %>% 
          select("Location", "Site", "Depth", "Comment", "BRUVS_type", "Record_duration", 
                 "SST", "Bottom_type") %>% 
          unique()) %>% 
  rbind(Revillagigedo16_MaxN %>% 
          select("Location", "Site", "Depth", "Comment", "BRUVS_type", "Record_duration", 
                 "SST", "Bottom_type") %>% 
          unique()) %>% 
  #Removing double site from Malpelo18 with NA for depth
  filter(! (Site == "Bahia Junior" & is.na(Depth)))
#Question - Bahia Junior in Malpelo18 has two depths, NA and 25, should the NA entry be deleted?
#Question - in Revillagigedo, some of the samples have the same site names, but different values for
#environmental variables --> replicate site names?! 
  

#Remove unnecessary variables
rm(Malpelo15_MaxN, Malpelo15_Length, Malpelo18_MaxN, Malpelo18_Length, Revillagigedo16_MaxN, 
   Revillagigedo16_Length)

