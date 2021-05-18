
# Species checking MaxN and Length data -----------------------------------

#Checking species names from MaxN and Length data

#Species from MaxN data
MaxN_sp <- MaxN %>% select(Family, Genus, Species, ValidName) %>% unique() %>% 
  mutate(Data = "MaxN")

#Species from Length data
Length_sp <- Length %>% select(Family, Genus, Species, ValidName) %>% unique() %>% 
  mutate(Data = "Length")

#Dataframe keeping only ValidName in Length and not in MaxN
df <- Length_sp[ !Length_sp$ValidName %in% MaxN_sp$ValidName ,]

#Dataframe of different species removing those identified to sp
df1 <- df %>% filter(! Species == "sp")

#Dataframe of different species removing those with genera also in MaxN
df2 <- df[ !df$Genus %in% MaxN_sp$Genus ,]
#This was a double check to see what genera were different, however it does not catch C. obscurus and df1 
#therefore has the complete number of species found in Length but not in MaxN

#Removing used dataframes
rm(df, df1, df2)

# Checking what species are missing from FishDB ---------------------------

FishDB_sp <- FishDB %>% select(Family, Genus, ValidName) %>% unique() %>% 
  mutate(Data = "FishDB")

#Dataframe keeping only ValidName in MaxN that are not in Fish
df <- MaxN_sp[ !MaxN_sp$ValidName %in% FishDB_sp$ValidName ,]

#Removing used dataframes
rm(MaxN_sp, Length_sp, FishDB_sp, df)

