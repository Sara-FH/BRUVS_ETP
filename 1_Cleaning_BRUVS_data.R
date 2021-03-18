###############################################################################################################
# Title: Cleaning BRUVS data from ETP sites
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

{library(tidyverse)
library(chron)}

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
  #Recode record_duration time to same format
  mutate(Record_duration = recode(Record_duration, 
                                  "1.3" = "1:30:00", 
                                  "1.30.00" = "1:30:00")) %>% 
  #Make record_duration into actual time cell
  mutate(Record_duration = chron(times = Record_duration))

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
  #Make record_duration into actual time cell
  mutate(Record_duration = chron(times = Record_duration))

Revillagigedo16_MaxN <- openxlsx::read.xlsx("Data/Revillagigedo_2016_MaxN.xlsx") %>% 
  select(-c(1:5, 7, 12:13, 15, 21, 23)) %>% 
  #Rename columns with . in name
  rename("BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Recode record_duration time to same format
  mutate(Record_duration = recode(Record_duration, 
                                  "1.30.00" = "1:30:00", 
                                  "1.10.00" = "1:10:00", 
                                  "1.30.00`" = "1:30:00", 
                                  "01.30.00" = "1:30:00")) %>% 
  #Make record_duration into actual time cell
  mutate(Record_duration = chron(times = Record_duration))

#Load MaxN data from sites and remove unnecessary columns

Malpelo15_Length <- openxlsx::read.xlsx("Data/Malpelo_2015_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 22:23, 25, 31, 33:35)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision" = "Precision.(mm)", 
         "RMS" = "RMS.(mm)", 
         "Range" = "Range.(mm)", 
         "Depth_ft" = "Depth", 
         "BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.duration",
         "Bottom_type" = "Bottom.type") %>%
  #Remove ? in site names
  mutate(Site = recode(Site, 
                       "Tres Mosqueteros?" = "Tres Mosqueteros")) %>%
  #Recode record_duration time to same format
  mutate(Record_duration = recode(Record_duration, 
                                  "1.3" = "1:30:00", 
                                  "1.30.00" = "1:30:00")) %>% 
  #Make record_duration into actual time cell
  mutate(Record_duration = chron(times = Record_duration))

Malpelo18_Length <- openxlsx::read.xlsx("Data/Malpelo_2018_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 22:23, 25, 31, 33:35)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision" = "Precision.(mm)", 
         "RMS" = "RMS.(mm)", 
         "Range" = "Range.(mm)",
         "BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.Duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Recode site name NA to Pared Naufrago
  mutate(Site = replace_na(Site, "Pared Naufrago")) %>% 
  #Remove site name from BRUVS_type column 
  mutate(BRUVS_type = na_if(BRUVS_type, "Pared Naufrago")) %>% 
  #Make record_duration into actual time cell
  mutate(Record_duration = chron(times = Record_duration))

Revillagigedo16_Length <- openxlsx::read.xlsx("Data/Revillagigedo_2016_Length.xlsx") %>% 
  select(-c(1:5, 10:15, 17, 22:23, 25, 31, 33:37)) %>% 
  #Rename columns with . in name
  rename("Length_mm" = "Length.(mm)", 
         "Precision" = "Precision.(mm)", 
         "RMS" = "RMS.(mm)", 
         "Range" = "Range.(mm)",
         "BRUVS_type" = "Bottom/Pelagic",
         "Record_duration" = "Record.duration",
         "Bottom_type" = "Bottom.type") %>% 
  #Recode record_duration time to same format
  mutate(Record_duration = recode(Record_duration, 
                                  "1.30.00" = "1:30:00", 
                                  "1.10.00" = "1:10:00", 
                                  "1.30.00`" = "1:30:00", 
                                  "01.30.00" = "1:30:00")) %>% 
  #Make record_duration into actual time cell
  mutate(Record_duration = chron(times = Record_duration))


# Cleaning data -----------------------------------------------------------

Malpelo15_MaxN <- Malpelo15_MaxN %>%
  #Depth from feet to meters
  mutate(Depth = Depth_ft/3.2808) %>% 
  select(-Depth_ft) %>% 
  #Remove columns with no family, genus, species, because they are sync points
  filter(!(is.na(Family) & is.na(Genus) & is.na(Species)))

Malpelo15_Length <- Malpelo15_Length %>%
  #Depth from feet to meters
  mutate(Depth = Depth_ft/3.2808) %>% 
  select(-Depth_ft)

Malpelo18_Length <- Malpelo18_Length %>% 
  #Making length numeric
  mutate(Length_mm = as.numeric(Length_mm)) %>% 
  #Remove columns with no family, genus, species, because they are sync points
  filter(!(is.na(Family) & is.na(Genus) & is.na(Species)))

Revillagigedo16_MaxN <- Revillagigedo16_MaxN %>% 
  #Remove columns with no family, genus, species, because they are sync points
  filter(!(is.na(Family) & is.na(Genus) & is.na(Species)))

Revillagigedo16_Length <- Revillagigedo16_Length %>% 
  #Remove columns with no family, genus, species, because they are sync points
  filter(!(is.na(Family) & is.na(Genus) & is.na(Species)))


# Environmental data ------------------------------------------------------

#Environmental variables for sites
EnvVar <- Malpelo15_MaxN %>% 
  select("Site", "Depth", "Comment", "BRUVS_type", "Record_duration", 
         "SST", "Bottom_type") %>% 
  unique() %>% 
  mutate(Location = "Malpelo15") %>% 
  rbind(Malpelo18_MaxN %>% 
          select("Site", "Depth", "Comment", "BRUVS_type", "Record_duration", 
                 "SST", "Bottom_type") %>% 
          unique() %>% 
          mutate(Location = "Malpelo18")) %>% 
  rbind(Revillagigedo16_MaxN %>% 
          select("Site", "Depth", "Comment", "BRUVS_type", "Record_duration", 
                 "SST", "Bottom_type") %>% 
          unique() %>% 
          mutate(Location = "Revillagigedo16"))


#Histograms illustrating MaxN and length for sites

ggplot(data = Malpelo15_MaxN, aes(x = MaxN)) +
  geom_histogram()

ggplot(data = Malpelo15_Length, aes(x = Length_mm)) +
  geom_histogram()

ggplot(data = Malpelo18_MaxN, aes(x = MaxN)) +
  geom_histogram()

ggplot(data = Malpelo18_Length, aes(x = Length_mm)) +
  geom_histogram()

ggplot(data = Revillagigedo16_MaxN, aes(x = MaxN)) +
  geom_histogram()

ggplot(data = Revillagigedo16_Length, aes(x = Length_mm)) +
  geom_histogram()
