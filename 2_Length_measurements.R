###############################################################################################################
# Title: Unmeasured undividuals
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

library(tidyverse)


# Checking Large individuals ----------------------------------------------

#Identifying individuals that are too small in DOVS data
TooLarge <- Length %>% #Using data from DOVS
  filter(Length_mm > MaxLgth_mm*1.2) %>% #filtering individuals more than 20% larger than Max registered
  #Keeping only individuals with precision better than 10%
  filter(Precision_mm < Length_mm*0.1) %>%
  #Keeping only individuals with RMS lower than 30
  filter(RMS_mm < 30) %>% 
  #Make length, precision and RMS to zero
  mutate(Length_mm = NA) %>% 
  mutate(Precision_mm = 0) %>% 
  mutate(RMS_mm = 0)
  
#Question - should the size be set differently than 20% larger than max registered?
#these 57 individuals are too large and therefore their lengths are removed to become an average

#Removing individuals that are too large
Length <- Length %>% 
  #Removing data from too large individuals
  anti_join(., TooLarge[,4:16]) %>% 
  #Adding too large individuals, but with no length, precision and RMS
  rbind(TooLarge)


#Remove unnecessary variables
rm(TooLarge)

# Protection status added -------------------------------------------------

# Length<- Length %>% 
#   left_join(MPA_status, by = c("Location" = "Name_year"))

# Unmeasured individuals --------------------------------------------------
#Quality Control
#Prior to calculating biomass we need our DOVS measurements to meet two requirements
#1. RMS <= 30 #Question - should we change RMS? 
Length %>% filter(RMS_mm > 20) %>% count()
Length %>% filter(RMS_mm > 30) %>% count()
Length %>% filter(RMS_mm > 50) %>% count()
Length %>% filter(Precision_mm > Length_mm*0.1) %>% count()
Length %>% filter(Precision_mm > Length_mm*0.1) %>% filter(RMS_mm > 20) %>% count()
Length %>% filter(Precision_mm > Length_mm*0.1) %>% filter(RMS_mm > 30) %>% count()
Length %>% filter(Precision_mm > Length_mm*0.1) %>% filter(RMS_mm > 50) %>% count()
#2. Precision <= 10% estimated Length

#To live up to the precision requirements, I have in the following removed length measurements
#that do not live up to the length requirements. Afterwards I have replaced them with average lengths 
#for each species, based on the length measurements with good precision

#Length corrections for both precision and RMS
Length <- Length %>% 
  #When precision is worse than 10% of length, the length is removed
  mutate(Length_mm = ifelse(!Precision_mm < Length_mm*0.1, NA, Length_mm)) %>% 
  #When RMS is worse than 30, length is removed
  mutate(Length_mm = ifelse(!RMS_mm < 30, NA, Length_mm)) %>%
  #and when there are no lengths, I remove the precision as well
  mutate(Precision_mm = ifelse(is.na(Length_mm), 0, Precision_mm)) %>% 
  #I also remove RMS, when the length is removed
  mutate(RMS_mm = ifelse(is.na(Length_mm), 0, RMS_mm)) %>% 
  uncount(., Number) %>% #uncount from tidyverse to expand N to one row per individual
  #N column is lost in uncount, so it is added here
  mutate(Number = 1) %>% 
  #group Replicate and ValidName
  group_by(Replicate, ValidName) %>% 
  #Making column with mean length of species per site
  mutate(Length_rep = mean(Length_mm, na.rm = TRUE)) %>%
  #group Location and ValidName
  group_by(Location, ValidName) %>% 
  #Making column with mean length of species per location
  mutate(Length_location = mean(Length_mm, na.rm = TRUE)) %>% 
  #group by protection status and ValidName
  group_by(Protection_status, ValidName) %>% 
  #Making column with mean length of species per location
  mutate(Length_status = mean(Length_mm, na.rm = TRUE)) %>% 
  #Combine lengths in New_length
  mutate(New_length = coalesce(Length_mm, Length_rep)) %>% 
  mutate(New_length = coalesce(New_length, Length_location)) %>%
  mutate(New_length = coalesce(New_length, Length_status)) %>%
  #remove old length columns
  select(-c(Length_mm, Length_rep, Length_location, Length_status)) %>%
  #changing order of columns
  select(Site, ValidName, New_length, everything()) %>% 
  #rename length column
  rename(Length_mm = New_length) %>% 
  ungroup()

#NA values in length_mm column
Length %>% filter(is.na(Length_mm)) %>% count()
#26 with no length

#No length value
temp <- Length %>% filter(is.na(Length_mm))
rm(temp)


#Quality control
#1. RMS <= 30
#2. Precision <= 10% estimated Length
#In the following no data is removed, as we check that RMS is below 50 and precision is no more 
#than 10% of length
Length <- Length %>% filter(RMS_mm <= 30) %>% #Because of blurry video RMS is higher than 20
  #We change the units of the lengths from mm to cm prior to biomass calculation
  # mutate(Length_mm = Length_mm/10) %>% 
  # #Now we rename the column to avoid confusion
  # rename("Length_cm"="Length_mm") %>% 
  #We will drop columns we do not need
  select(-c(Precision_mm, RMS_mm, Range_mm))


