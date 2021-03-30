###############################################################################################################
# Title: Unmeasured undividuals
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

library(tidyverse)


# Unmeasured undividuals --------------------------------------------------
#Quality Control
#Prior to calculating biomass we need our DOVS measurements to meet two requirements
#1. RMS <= 50 #Question - should we change RMS? 
Length %>% filter(RMS_mm > 20) %>% count()
Length %>% filter(RMS_mm > 50) %>% count()
Length %>% filter(Precision_mm > Length_mm*0.1) %>% count()
Length %>% filter(Precision_mm > Length_mm*0.1) %>% filter(RMS_mm > 20) %>% count()
Length %>% filter(Precision_mm > Length_mm*0.1) %>% filter(RMS_mm > 50) %>% count()
#2. Precision <= 10% estimated Length

#To live up to the precision requirements, I have in the following removed length measurements
#that do not live up to the length requirements. Afterwards I have replaced them with average lengths 
#for each species, based on the length measurements with good precision

#Removing lengths that have bad precision (worse than 10% of length)
#Length
x <- Length %>% 
  #When precision is worse than 10% of length, the length is removed
  mutate(Length_mm = ifelse(!Precision_mm < Length_mm*0.1, NA, Length_mm)) %>% 
  #and when there are no lengths, I remove the precision as well
  mutate(Precision_mm = ifelse(is.na(Length_mm), 0, Precision_mm)) %>% 
  #I also remove RMS, when the length is removed
  mutate(RMS_mm = ifelse(is.na(Length_mm), 0, RMS_mm)) %>% 
  uncount(., Number) %>% #uncount from tidyverse to expand N to one row per individual
  #N column is lost in uncount, so it is added here
  mutate(Number = 1) %>% 
  #group Site and ValidName
  group_by(Site, ValidName) %>% 
  #Making column with mean length of species per site
  mutate(Length_site = mean(Length_mm, na.rm = TRUE)) %>%
  #group Locaiton and ValidName
  group_by(Location, ValidName) %>% 
  #Making column with mean length of species per location
  mutate(Length_location = mean(Length_mm, na.rm = TRUE)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(Length_mm, Length_site)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(New_length, Length_location)) %>% 
  #changing order of columns
  select(Site, Length_mm, Length_site, Length_location, New_length, everything()) %>% 
  #remove old length columns
  select(-c(Length_mm, Length_site, Length_location)) %>% 
  #rename length column
  rename(Length_mm = New_length) %>% 
  ungroup()

#These are the 21 individuals that cannot get a length based on other individuals in the dataset
NoLength <- x %>% filter(is.nan(Length_mm))
rm(NoLength)

temp <- x %>% filter(RMS_mm >= 50)
rm(temp)

#Length corrections for both precision and RMS
Length <- Length %>% 
  #When precision is worse than 10% of length, the length is removed
  mutate(Length_mm = ifelse(!Precision_mm < Length_mm*0.1, NA, Length_mm)) %>% 
  #When RMS is worse than 50, length is removed
  mutate(Length_mm = ifelse(!RMS_mm < 50, NA, Length_mm)) %>%
  #and when there are no lengths, I remove the precision as well
  mutate(Precision_mm = ifelse(is.na(Length_mm), 0, Precision_mm)) %>% 
  #I also remove RMS, when the length is removed
  mutate(RMS_mm = ifelse(is.na(Length_mm), 0, RMS_mm)) %>% 
  uncount(., Number) %>% #uncount from tidyverse to expand N to one row per individual
  #N column is lost in uncount, so it is added here
  mutate(Number = 1) %>% 
  #group Site and ValidName
  group_by(Site, ValidName) %>% 
  #Making column with mean length of species per site
  mutate(Length_site = mean(Length_mm, na.rm = TRUE)) %>%
  #group Locaiton and ValidName
  group_by(Location, ValidName) %>% 
  #Making column with mean length of species per location
  mutate(Length_location = mean(Length_mm, na.rm = TRUE)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(Length_mm, Length_site)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(New_length, Length_location)) %>% 
  #changing order of columns
  select(Site, Length_mm, Length_site, Length_location, New_length, everything()) %>% 
  #remove old length columns
  select(-c(Length_mm, Length_site, Length_location)) %>% 
  #rename length column
  rename(Length_mm = New_length) %>% 
  ungroup()

z <- x %>% filter(is.nan(Length_mm)) #24
zz <- Length %>% filter(is.nan(Length_mm)) #33 (but this is the correct way)
rm(x, z, zz)

#Quality control
#1. RMS <= 50
#2. Precision <= 10% estimated Length
#In the following no data is removed, as we check that RMS is below 50 and precision is no more 
#than 10% of length
Length <- Length %>% filter(RMS_mm <= 50) %>% #Because of blurry video RMS is higher than 20
  #We change the units of the lengths from mm to cm prior to biomass calculation
  mutate(Length_mm = Length_mm/10) %>% 
  #Now we rename the column to avoid confusion
  rename("Length_cm"="Length_mm") %>% 
  #We will drop columns we do not need
  select(-c(Precision_mm, RMS_mm, Range_mm))



# Code for lengths based on fishing status --------------------------------

  #Question: I don't have fishing status yet for all sites
  #grouping species and fishing status
  group_by(ValidName, Fishing_status) %>% 
  #Making column with mean length per fishing status
  mutate(Length_fishing = mean(Length_mm, na.rm = TRUE)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(Length_mm, Length_site)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(New_length, Length_fishing)) %>% 
  #changing order of columns
  select(Site, Period, Length_mm, Length_site, Length_fishing, New_length, everything()) %>% 
  #remove old length columns
  select(-c(Length_mm, Length_site, Length_fishing)) %>% 
  #rename length column
  rename(Length_mm = New_length) %>% 
  ungroup()



