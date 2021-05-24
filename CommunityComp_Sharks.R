
# Libraries ---------------------------------------------------------------

{
  library(tidyverse)
  library(ggplot2)
  library(cowplot)
}

# Sharks and protection level ---------------------------------------------

#Making counts data frame for predator community based on family
CountsSharks <- MaxNSharks %>%
  select(Replicate, Location, Family, ValidName, MaxN_sp, Protection_status, Coastal_Oceanic) %>% 
  drop_na(ValidName) %>% 
  group_by(Protection_status, ValidName) %>% 
  mutate(Mean_sp_status = mean(MaxN_sp, na.rm = T)) %>% 
  select(-c(Replicate, Location, MaxN_sp, Coastal_Oceanic)) %>% 
  unique() %>% 
  group_by(Protection_status) %>% 
  mutate(Sum = sum(Mean_sp_status)) %>%  
  mutate(Prop = Mean_sp_status/Sum*100) %>% 
  #mutate(ValidName = replace(ValidName, Prop < 5, "Other Families")) %>% 
  group_by(Protection_status, ValidName) %>% 
  mutate(N = length(ValidName)) %>% 
  ungroup() %>% 
  mutate(Sum = round(Sum, digits = 1)) %>% 
  mutate(ValidName = factor(ValidName)) %>% 
  mutate(Protection_status = factor(Protection_status, levels = c("Low", "Medium", "High"))) %>% 
  mutate(ValidName = fct_reorder(ValidName, N)) %>% 
  #Cumulative prop
  group_by(Protection_status) %>% 
  mutate(CumProp = cumsum(Prop)) %>% 
  ungroup()

#Graph shark community by status
p1 <- ggplot(CountsSharks, aes(x = Protection_status, y = Prop, fill = ValidName))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color = "black", size = 1)+
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 104), expand = c(0.02, 0.02))+
  scale_fill_manual(values = mycolors)+
  coord_flip()+
  labs(x = "Protection status", fill = "Shark Species")+
  geom_text(x = CountsSharks$Protection_status, y = 103, color = "black",
            label = ifelse(CountsSharks$CumProp >= 99, CountsSharks$Sum, ""), vjust = 0.4, size = 4.5)+
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

# Sharks coastal or oceanic -----------------------------------------------

#Making counts data frame for predator community based on family
CountsSharks <- MaxNSharks %>%
  select(Replicate, Location, Family, ValidName, MaxN_sp, Protection_status, Coastal_Oceanic) %>% 
  drop_na(ValidName) %>% 
  group_by(Coastal_Oceanic, ValidName) %>% 
  mutate(Mean_sp_CO = mean(MaxN_sp, na.rm = T)) %>% 
  select(-c(Replicate, Location, MaxN_sp, Protection_status)) %>% 
  unique() %>% 
  group_by(Coastal_Oceanic) %>% 
  mutate(Sum = sum(Mean_sp_CO)) %>%  
  mutate(Prop = Mean_sp_CO/Sum*100) %>% 
  #mutate(ValidName = replace(ValidName, Prop < 5, "Other Families")) %>% 
  group_by(Coastal_Oceanic, ValidName) %>% 
  mutate(N = length(ValidName)) %>% 
  ungroup() %>% 
  mutate(Sum = round(Sum, digits = 1)) %>% 
  mutate(ValidName = factor(ValidName)) %>% 
  mutate(ValidName = fct_reorder(ValidName, N)) %>% 
  #Cumulative prop
  group_by(Coastal_Oceanic) %>% 
  mutate(CumProp = cumsum(Prop)) %>% 
  ungroup()


#Colors
mycolors <- c("#004949","#009292","#ff6db6", "#490092","#006ddb","#6db6ff",
              "#b6dbff", "#920000","#924900","#db6d00","#66A61E", "#E6AB02")

#Graph shark community by status
p2 <- ggplot(CountsSharks, aes(x = Coastal_Oceanic, y = Prop, fill = ValidName))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color = "black", size = 1)+
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 104), expand = c(0.02, 0.02))+
  scale_fill_manual(values = mycolors)+
  #scale_fill_fish_d(option = "Coris_gaimard")+
  coord_flip()+
  labs(x = "Province")+
  geom_text(x = CountsSharks$Coastal_Oceanic, y = 103, color = "black",
            label = ifelse(CountsSharks$CumProp >= 99, CountsSharks$Sum, ""), vjust = 0.4, size = 4.5)+
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


# Sharks Location ---------------------------------------------------------

#Making counts data frame for predator community based on family
CountsSharks <- MaxNSharks %>%
  select(Replicate, Location, Family, ValidName, MaxN_sp, Protection_status, Coastal_Oceanic) %>% 
  drop_na(ValidName) %>% 
  group_by(Location, ValidName) %>% 
  mutate(Mean_sp_loc = mean(MaxN_sp, na.rm = T)) %>% 
  select(-c(Replicate, MaxN_sp)) %>% 
  unique() %>% 
  group_by(Location) %>% 
  mutate(Sum = sum(Mean_sp_loc)) %>%  
  mutate(Prop = Mean_sp_loc/Sum*100) %>% 
  #mutate(ValidName = replace(ValidName, Prop < 5, "Other Families")) %>% 
  group_by(Location, ValidName) %>% 
  mutate(N = length(ValidName)) %>% 
  ungroup() %>% 
  mutate(Sum = round(Sum, digits = 1)) %>% 
  mutate(ValidName = factor(ValidName)) %>%
  mutate(Location = factor(Location, levels = c("GSF", "PNM", "Cano", "Clip", 
                                                "Revilla", "GMR", "Malpelo"))) %>%
  mutate(ValidName = fct_reorder(ValidName, N)) %>% 
  #Cumulative prop
  group_by(Location) %>% 
  mutate(CumProp = cumsum(Prop)) %>% 
  #Short names for MPAs
  mutate(Name = recode(Location, 
                       "Cano" = "CIBR", 
                       "Clip" = "CA", 
                       "Malpelo" = "MFFS", 
                       "Revilla" = "RNP")) %>% 
  ungroup()

#Cano - CIBR - Cano Island Biological Reserve
#Clip - CA - Clipperton Atoll / Clipperton Island
#Malpelo Fauna and Flora Sanctuary - MFFS
#Revillagigedo national park - RNP


#Graph shark community by location
p3 <- ggplot(CountsSharks, aes(x = Name, y = Prop, fill = ValidName))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", color = "black", size = 1)+
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 102), expand = c(0.01,0.01))+
  scale_fill_manual(values = mycolors)+
  coord_flip()+
  labs(x = "Marine Protected Areas", y = "% total abundance")+
  geom_text(x = CountsSharks$Name, y = 101.5, color = "black",
            label = ifelse(CountsSharks$CumProp >= 99, CountsSharks$Sum, ""), vjust = 0.4, size = 4.5)+
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

CompSharks <- ggarrange(plot_grid(p1leg, plot_grid(p1+theme(legend.position = "none"), p2, 
                                                 align = "hv", axis = "lb", labels = c("A", "B"),
                                                 rel_widths = c(1, 1), label_size = 20),
                                ncol = 1, rel_heights = c(0.5, 1.2, 2), label_size = 20,
                                p3, labels = c("", "", "C"), nrow = 3))
CompSharks

#Saving PCO for biomass - method and management status
ggsave("Figures/CompSharks.tiff",
       CompSharks, device = "tiff", dpi = 300, width = 16, height = 10)

