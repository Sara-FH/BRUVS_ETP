###############################################################################################################
# Title: GAM
# Author: Sara Færch Hansen
# Assisting: 
# Version: 
# Date last updated: 
###############################################################################################################


# Loading libraries -------------------------------------------------------

{library(car)
library(MASS)
library(tidyverse)
library(mgcv)
  }


# Exploring data ----------------------------------------------------------

#Can be used for negative binomial - however, I think the data is poisson or quasi poisson
# Density <- Density %>% 
#   mutate(MaxN_site = round(MaxN_site, 0))

#histogram of data
hist(Factors$MaxN_site)

#range with different transformations
range(Density$MaxN_site)
range(Density$MaxN_site^0.5)
range(log10(Density$MaxN_site))
      
#normal distribution - a lot of points fall outside of dashed line
qqp(Density$MaxN_site, "norm")
#log normal distribution - also no 
qqp(Density$MaxN_site, "lnorm")
#negative binomial distribution - few points outside dashed line
nbinom <- fitdistr(Density$MaxN_site, "Negative Binomial") #Does not work
qqp(Density$MaxN_site, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) #Does not work
#Poisson distribution - also no
poisson <- fitdistr(Density$MaxN_site, "Poisson")
qqp(Density$MaxN_site, "pois", lambda = poisson$estimate)
#Gamma distribution
gamma <- fitdistr(Density$MaxN_site, "gamma")
qqp(Density$MaxN_site, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#From GLMM
binomial(link = "logit")
gaussian(link = "identity")
Gamma(link = "inverse")
inverse.gaussian(link = "1/mu^2")
poisson(link = "log")
quasi(link = "identity", variance = "constant")
quasibinomial(link = "logit")
quasipoisson(link = "log")



# GAM ---------------------------------------------------------------------


#Can be used for negative binomial - however, I think the data is poisson or quasi poisson
Factors2 <- Factors %>%
  mutate(MaxN_site = round(MaxN_site, 0))

#gaussian
gam <- gam(MaxN_site ~ Location + Bottom_type + Comment + s(Depth, k = 4), 
           data = Factors, family = gaussian, method = "REML")
summary(gam)

#gaussian using site as well - all variation explained. Very bad modelling of data.
gam1 <- gam(MaxN_site ~ Location + Site, 
           data = Factors, family = gaussian, method = "REML")
summary(gam1)
gam.check(gam1, rep = 500)
gratia::appraise(gam1, method = "simulate")
gratia::draw(gam1, scales = "fixed")


#quasipoisson
gam2 <- gam(MaxN_site ~ Location + Bottom_type + Comment + s(Depth, k = 4), 
           data = Factors, family = quasipoisson, method = "REML")
summary(gam2)

#negative binomial
gam3 <- gam(MaxN_site ~ Location + Bottom_type + Comment + s(Depth, k = 4), 
            data = Factors, family = nb, method = "REML")
summary(gam3)


#poisson
gam4 <- gam(MaxN_site ~ Location + Bottom_type + Comment + s(Depth, k = 4), 
            data = Factors2, family = poisson, method = "REML")
summary(gam4)
gam.check(gam4, rep = 500)
gratia::appraise(gam4, method = "simulate")
gratia::draw(gam4, scales = "fixed")

#zero inflated poisson
library(pscl)
m.zip <- zeroinfl(MaxN_site ~ Location + Bottom_type + Comment, 
                  data = Factors2) #Not zero inflated data.
summary(m.zip) #Not zero inflated data.

#Skewed data
gam5 <- gam(MaxN_site ~ Location + Bottom_type + Comment, 
            data = Factors2, family = Gamma(link = "inverse"), method = "REML")
summary(gam5)
gratia::appraise(gam5, method = "simulate")


#gam.check
gam.check(gam, rep = 500)

gam.check(gam2, rep = 500)

gam.check(gam3, rep = 500)

library(gratia)

gratia::appraise(gam, method = "simulate")

gratia::draw(gam, scales = "fixed")

gratia::appraise(gam2, method = "simulate")

gratia::draw(gam2, scales = "fixed")

AIC(gam, gam2)

AIC(gam, gam3)


# Predictions -------------------------------------------------------------

library(ggeffects)
library(fishualize)

pred.df <- as.data.frame(ggpredict(gam, terms = c("Location", "Bottom_type", "Comment", "Depth"))) %>%
  rename(Location = "x",
         Bottom_type = "group", 
         Slope = "facet", 
         Depth = "panel")

pred.plot <- ggplot(data = pred.df, aes(x = as.factor(Slope), y = predicted, color = Location, 
                                        fill = Location, group = Location)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                color = "black", position = position_dodge(width = 0.5)) +
  #geom_ribbon(aes(ymin=conf.low,ymax=conf.high, fill=Species), alpha=0.2, show.legend = F, colour = NA) +
  geom_point(aes(shape = Location), position = position_dodge(width = 0.5), size = 3, color = "black") +
  geom_jitter(data = Factors, aes(x = as.factor(Comment), y = MaxN_site, 
                                 color = Location, fill = Location, group = Location, shape = Location), 
              alpha = 0.25, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05)) +
  scale_shape_manual(values = 21:23) +
  scale_color_fish(option = "Valenciennea_strigata", discrete = T) +
  scale_fill_fish(option = "Valenciennea_strigata", discrete = T) +
  theme_bw() +
  theme(legend.position = "top") +
  ylab("MaxN / hour") +
  xlab("Location")
pred.plot

  


# Writing things to excel -------------------------------------------------



library(openxlsx)

#writing excel sheet
write.xlsx(EnvVar, "EnvVar.xlsx")
