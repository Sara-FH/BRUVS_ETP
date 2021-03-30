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
hist(Density$MaxN_site)

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

gam <- gam(MaxN_site ~ s(Location) + s(Site), data = Factors)




# Writing things to excel -------------------------------------------------



library(openxlsx)

#writing excel sheet
write.xlsx(EnvVar, "EnvVar.xlsx")
