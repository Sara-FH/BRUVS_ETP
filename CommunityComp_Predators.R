
# Libraries ---------------------------------------------------------------

{
  library(tidyverse)
  library(ggplot2)
  library(cowplot)
}

# Predators and protection level ---------------------------------------------

temp <- data.frame("High", "Carcharhinidae", 0, 1, NA) %>% 
  rename("Protection_status" = 1, "Family" = 2, "Prop" = 3, "N" = 4, "Sum" = 5)

#Making counts data frame for predator community based on family
CountsPred <- MaxNPred %>%
  select(Replicate, Location, Family, MaxN_sp, Protection_status, Coastal_Oceanic) %>% 
  drop_na(Family) %>% 
  group_by(Protection_status, Family) %>% 
  mutate(Mean_sp_status = mean(MaxN_sp, na.rm = T)) %>% 
  select(-c(Replicate, Location, MaxN_sp, Protection_status)) %>% 
  unique() %>% 
  group_by(Protection_status) %>% 
  mutate(Sum = sum(Mean_sp_status)) %>%  
  mutate(Prop = Mean_sp_status/Sum*100) %>% 
  mutate(Family = replace(Family, Prop < 3, "Other Families")) %>% 
  group_by(Protection_status, Family) %>% 
  summarise(Prop = sum(Prop, na.rm = T), N = n(), Sum) %>%
  unique() %>% 
  ungroup() %>% 
  rbind(temp) %>% 
  mutate(Sum = round(Sum, digits = 1)) %>% 
  mutate(Family = factor(Family)) %>% 
  mutate(Protection_status = factor(Protection_status, levels = c("Low", "Medium", "High"))) %>% 
  mutate(Family = fct_reorder(Family, N)) %>% 
  #Cumulative prop
  group_by(Protection_status) %>% 
  mutate(CumProp = cumsum(Prop)) %>% 
  ungroup()

#remove temp
rm(temp)

#Colors
mycolors <- c("#004949","#009292", "#999933", "#ff6db6", "#490092","#006ddb","#6db6ff",
              "#b6dbff", "#920000", "#924900", "#db6d00", "#66A61E", "#E6AB02", 
              "#AA4499", "#117733", "#888888")

#Graph shark community by status
p1 <- ggplot(CountsPred, aes(x = Protection_status, y = Prop, fill = Family))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color = "black", size = 1)+
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 106), expand = c(0.02, 0.02))+
  scale_fill_manual(values = mycolors)+
  coord_flip()+
  labs(x = "Protection status", fill = "Shark Species")+
  geom_text(x = CountsPred$Protection_status, y = 104, color = "black",
            label = ifelse(CountsPred$CumProp >= 99, CountsPred$Sum, ""), vjust = 0.4, size = 4.5)+
  facet_grid(Protection_status~., scale = "free", space = "free")+ 
  theme_bw()+
  theme(strip.text.y = element_text(size = 14, color = "black", face = "bold"),
        axis.title.y = element_text(family = "sans", size = 16, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "sans", color = "black", size = 14), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(family = "sans", face = "bold.italic", size = 14),
        legend.title = element_text(family = "sans", face = "bold", size = 16),
        axis.ticks.length.x = unit(0.25, "cm"))+
  guides(color = guide_legend(override.aes = list(size = 3)),
         fill = guide_legend(title.position = "top"))

p1

#Merging the two plots into one panel
p1leg <- get_legend(p1)

# Predators coastal or oceanic -----------------------------------------------

#Making counts data frame for predator community based on family
CountsPred <- MaxNPred %>%
  select(Replicate, Location, Family, MaxN_sp, Protection_status, Coastal_Oceanic) %>% 
  drop_na(Family) %>% 
  group_by(Coastal_Oceanic, Family) %>% 
  mutate(Mean_sp_CO = mean(MaxN_sp, na.rm = T)) %>% 
  select(-c(Replicate, Location, MaxN_sp, Protection_status)) %>% 
  unique() %>% 
  group_by(Coastal_Oceanic) %>% 
  mutate(Sum = sum(Mean_sp_CO)) %>%  
  mutate(Prop = Mean_sp_CO/Sum*100) %>% 
  mutate(Family = replace(Family, Prop < 3, "Other Families")) %>% 
  group_by(Coastal_Oceanic, Family) %>% 
  summarise(Prop = sum(Prop, na.rm = T), N = n(),Sum) %>%
  unique() %>% 
  ungroup() %>% 
  mutate(Sum = round(Sum, digits = 1)) %>% 
  mutate(Family = factor(Family)) %>% 
  mutate(Family = fct_reorder(Family, N)) %>% 
  #Cumulative prop
  group_by(Coastal_Oceanic) %>% 
  mutate(CumProp = cumsum(Prop)) %>% 
  ungroup()


#Colors
mycolors <- c("#004949","#009292", "#006ddb","#6db6ff", "#db6d00","#66A61E",
              "#AA4499", "#117733", "#888888")

#Graph shark community by status
p2 <- ggplot(CountsPred, aes(x = Coastal_Oceanic, y = Prop, fill = Family))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color = "black", size = 1)+
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 104.5), expand = c(0.02, 0.02))+
  scale_fill_manual(values = mycolors)+
  #scale_fill_fish_d(option = "Coris_gaimard")+
  coord_flip()+
  labs(x = "Province")+
  geom_text(x = CountsPred$Coastal_Oceanic, y = 103.5, color = "black",
            label = ifelse(CountsPred$CumProp >= 99, CountsPred$Sum, ""), vjust = 0.4, size = 4.5)+
  facet_grid(Coastal_Oceanic~., scale = "free", space = "free")+ 
  theme_bw()+
  theme(strip.text.y = element_text(size = 14, color = "black", face = "bold"),
        axis.title.y = element_text(family = "sans", size = 16, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "sans", color = "black", size = 14), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.ticks.length.x = unit(0.25, "cm"))

p2


# Predators Location ---------------------------------------------------------

#Making counts data frame for predator community based on family
CountsPred <- MaxNPred %>%
  select(Replicate, Location, Family, MaxN_sp) %>% 
  drop_na(Family) %>% 
  group_by(Location, Family) %>% 
  mutate(Mean_sp_status = mean(MaxN_sp, na.rm = T)) %>% 
  select(-c(Replicate, Location, MaxN_sp, Location)) %>% 
  unique() %>% 
  group_by(Location) %>% 
  mutate(Sum = sum(Mean_sp_status)) %>%  
  mutate(Prop = Mean_sp_status/Sum*100) %>% 
  mutate(Family = replace(Family, Prop < 3, "Other Families")) %>% 
  group_by(Location, Family) %>% 
  summarise(Prop = sum(Prop, na.rm = T), N = n(), Sum) %>%
  unique() %>% 
  ungroup() %>% 
  mutate(Sum = round(Sum, digits = 1)) %>% 
  mutate(Family = factor(Family)) %>% 
  mutate(Location = factor(Location, levels = c("GSF", "PNM", "Cano", "Clip", 
                                                "Revilla", "GMR", "Malpelo"))) %>%
  mutate(Family = fct_reorder(Family, N)) %>% 
  #Cumulative prop
  group_by(Location) %>% 
  mutate(CumProp = cumsum(Prop)) %>% 
  ungroup() %>% 
  #Short names for MPAs
  mutate(Name = recode(Location, 
                       "Cano" = "CIBR", 
                       "Clip" = "CA", 
                       "Malpelo" = "MFFS", 
                       "Revilla" = "RNP"))

#Cano - CIBR - Cano Island Biological Reserve
#Clip - CA - Clipperton Atoll / Clipperton Island
#Malpelo Fauna and Flora Sanctuary - MFFS
#Revillagigedo national park - RNP

#Colors
mycolors <- c("#004949","#009292", "#999933", "#ff6db6", "#006ddb","#6db6ff",
              "#920000", "#db6d00", "#66A61E", "#E6AB02", "#AA4499", "#117733", 
              "#888888")

#Graph shark community by location
p3 <- ggplot(CountsPred, aes(x = Name, y = Prop, fill = Family))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color = "black", size = 1)+
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 102), expand = c(0.01,0.01))+
  scale_fill_manual(values = mycolors)+
  coord_flip()+
  labs(x = "Marine Protected Areas", y = "% total abundance")+
  geom_text(x = CountsPred$Name, y = 101.5, color = "black",
            label = ifelse(CountsPred$CumProp >= 99, CountsPred$Sum, ""), vjust = 0.4, size = 4.5)+
  facet_grid(Name~., scale = "free", space = "free")+
  theme_bw()+
  theme(strip.text.y = element_text(size = 14, color = "black", face = "bold"),
        axis.title = element_text(family = "sans", size = 16, face = "bold"),
        axis.text.x = element_text(family = "sans", color = "black", size = 14), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.ticks.length.x = unit(0.25, "cm"))

p3

# Combining graphs --------------------------------------------------------

CompPred <- ggarrange(plot_grid(p1leg, plot_grid(p1+theme(legend.position = "none"), p2, 
                                                 align = "hv", axis = "lb", labels = c("A", "B"),
                                                 rel_widths = c(1, 1), label_size = 20),
                                ncol = 1, rel_heights = c(0.5, 1.2, 2), label_size = 20,
                                p3, labels = c("", "", "C"), nrow = 3))
CompPred

#Saving PCO for biomass - method and management status
ggsave("Figures/CompPred.tiff",
       CompPred, device = "tiff", dpi = 300, width = 16, height = 10)


