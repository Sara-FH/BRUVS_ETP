#Zuur 2009 GLM


#MaxNFish new column
MaxNFish2 <- MaxNFish %>% 
  mutate(Dotplot = recode(Location, 
                          "Malpelo15" = 1, 
                          "Malpelo18" = 2,
                          "Revilla16" = 3,
                          "Clip16" = 4,
                          "PNM19" = 5,)) %>% 
  mutate(MaxN_rep = round(MaxN_rep, 0)) %>% 
  left_join(MPA_status %>% select(2:4), by = c("Location" = "Name_year"))

#Homogeneity of variance
dotchart(MaxNFish2$MaxN_rep,
         groups = factor(MaxNFish2$Dotplot),
         ylab = "Location", xlab = "MaxN per hour",
         main = "MaxN dotplot", pch = MaxNFish2$Dotplot)
#Data does not have homogeneity of variance

#Boxplot
boxplot(MaxN_rep ~ factor(Location),
        varwidth = TRUE, xlab = "MaxN per hour",
        main = "Boxplot of MaxN conditional on\
Location", ylab = "Location", data = MaxNFish2)

#Histogram - check for normality
hist(MaxNFish2$MaxN_rep)

#shapiro test for normality, significant = data does not follow normal distribution
shapiro.test(MaxNFish2$MaxN_rep)

#MaxN for glm, with replicates (to get overview)
MaxN_glm_full <- MaxNFish2 %>% 
  select(Replicate, Location, MaxN_rep, Protection_status, Coastal_Oceanic) %>% 
  unique()

#MaxN for glm with only necessary variables
MaxN_glm <- MaxNFish2 %>% 
  select(Location, MaxN_rep, Protection_status, Coastal_Oceanic) %>% 
  unique()


library(MASS)

#GLM
M1 <- glm(MaxN_rep ~ Location, family = poisson, data = MaxN_glm)
M2 <- glm.nb(MaxN_rep ~ Location, data = MaxN_glm_full)
summary(M1)
summary(M2)

#Analysis of deviance tables - for when I can use all factors: Location, Protection_status, Oceanic_Coastal
anova(M1, test = "Chi")
anova(M2, test = "Chi")

#Drop each term in turn and compare the full model with a nested model using the drop1 command
drop1(M1, test = "Chi")
drop1(M2, test = "Chi")


# Correlation of variables ------------------------------------------------

#Showing correlation between variables in the summary
summary(M1, corr = TRUE)
summary(M2, corr = TRUE)
#Correlation between coefficients should be below 0.7, Dormann et al. 2013

#Making the discrete variables from character to factor
MaxN_corr <- MaxN_glm %>% 
  select(-c(Replicate)) %>% 
  mutate(MaxN_rep = as.integer(MaxN_rep)) %>% 
  mutate(Location = recode(Location, 
                           "Malpelo15" = 1, 
                           "Malpelo18" = 2, 
                           "Revilla16" = 3, 
                           "Clip16" = 4, 
                           "PNM19" = 5)) %>% 
  mutate(Protection_status = recode(Protection_status, 
                                    "High" = 1, 
                                    "Medium" = 2, 
                                    "Low" = 3)) %>% 
  mutate(Coastal_Oceanic = recode(Coastal_Oceanic, 
                                  "Oceanic" = 1, 
                                  "Coastal" = 2)) 

library(GGally)
# Convert data to numeric
#corr <- data.frame(lapply(MaxN_glm, as.integer))
# Plot the graph
ggcorr(MaxN_corr, 
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

#Correlation of variables/fixed effects, variance inflation factor (VIF)
#Finlay et al. 2020 uses both

#Variance inflation factor (VIF)

library(performance)
#The variance inflation factor is a measure to analyze the magnitude of multicollinearity of model terms
#A VIF less than 5 indicates a low correlation of that predictor with other predictors. 
#A value between 5 and 10 indicates a moderate correlation, while VIF values larger than 10 are a sign 
#for high, not tolerable correlation of model predictors (James et al. 2013).
check_collinearity(M1)
check_collinearity(M2)

#Correlation of fixed effects (variables)
ggpairs(MaxN_glm)
#Can be used for continuous variables, not useful for categorical variables

# Pseudo R^2 --------------------------------------------------------------

#McFadden manual calculation
with(summary(M1), 1 - deviance/null.deviance)
with(summary(M2), 1 - deviance/null.deviance)

#According to Zuur et al. 2009
with(summary(M1), 100*((null.deviance-deviance)/null.deviance))
with(summary(M2), 100*((null.deviance-deviance)/null.deviance))

# library(DescTools)
#Pseudo R calculations: 
#"McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "AldrichNelson", "VeallZimmermann", 
#"Efron", "McKelveyZavoina", "Tjur", "all"

# PseudoR2(M1, which = "McFadden")
# PseudoR2(M1, which = "McFaddenAdj")
# PseudoR2(M1, which = "Nagelkerke")
# PseudoR2(M1, which = "CoxSnell")
# 
# PseudoR2(M2, which = "McFadden")
# PseudoR2(M2, which = "McFaddenAdj")
# PseudoR2(M2, which = "Nagelkerke")
# PseudoR2(M2, which = "CoxSnell")


# Test for overdispersion -------------------------------------------------

library(DHARMa)

#simulated residuals M1
sim_M1 <- simulateResiduals(M1, refit=T)
testOverdispersion(sim_M1)
plotSimulatedResiduals(sim_M1)

#simulated residuals M2
sim_M2 <- simulateResiduals(M2, refit=T) #, n=99
testOverdispersion(sim_M2)
plotSimulatedResiduals(sim_M2)


# Test for zero-inflation -------------------------------------------------

testZeroInflation(sim_M1) # no zero-inflation
testZeroInflation(sim_M2) # no zero-inflation

# Goodness of fit of the GLM ----------------------------------------------

#Website: https://www.theanalysisfactor.com/r-glm-model-fit/

library(ResourceSelection)

#Hosmer and Lemeshow goodness of fit (GOF) test
hoslem.test(Factors$MaxN_rep, fitted(M1))
#Our model appears to fit well because we have no significant difference between the model and 
#the observed data (i.e. the p-value is above 0.05).

hoslem.test(Factors$MaxN_rep, fitted(M2))
#Our model appears to fit well because we have no significant difference between the model and 
#the observed data (i.e. the p-value is above 0.05).

# Test of parameters ------------------------------------------------------

#Website: https://benwhalley.github.io/just-enough-r/generalized-linear-models.html

#Analysis of Deviance Table (Type III tests)
car::Anova(M2, type=3)



# GLMM for comparison -----------------------------------------------------

library(lme4)
M3 <- glmer(MaxN_rep ~ Location + (1|Replicate), family = poisson, data = MaxN_glm)
summary(M3) #Higher AIC than negative binomial model M2

sim_M3 <- simulateResiduals(M3, refit=T, n=250) # takes a while, about 10 minutes or so
plotSimulatedResiduals(sim_M3)

testOverdispersion(sim_M3) # requires refit=T
testZeroInflation(sim_M3) # no zero-inflation


M4 <- glmer.nb(MaxN_rep ~ Location + (1|Replicate), data = MaxN_glm)
summary(M4) #Higher AIC than negative binomial model M2

sim_M4 <- simulateResiduals(M4, refit=T, n=99)
plotSimulatedResiduals(sim_M4)

testOverdispersion(sim_M4) # requires refit=T
testZeroInflation(sim_M4) # no zero-inflation

# Predictions based on the GLM --------------------------------------------

library(ggeffects)
#Prediction dataframe at response scale with 95% confidence intervals
pred.df <- as.data.frame(ggpredict(M1, terms = c("Location"))) %>% 
  rename(Location = "x")

library(RColorBrewer)
#ggplot
pred.plot <- ggplot(data = pred.df, aes(x = Location, y = predicted, fill = Location, group = Location)) +
  geom_col(aes(fill = Location), alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.5, size = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") + 
  geom_jitter(data = MaxN_glm, aes(x = Location, y = MaxN_rep, color = Location, fill = Location, 
                                   group = Location), 
              alpha = 0.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.5))

pred.plot


# Predict on negative binomial --------------------------------------------
library(ggeffects)
library(RColorBrewer)


#Prediction dataframe at response scale with 95% confidence intervals
pred.df <- as.data.frame(ggpredict(M2, terms = c("Location"))) %>% 
  rename(Location = "x")


#ggplot
pred.plot <- ggplot(data = pred.df, aes(x = Location, y = predicted, fill = Location, group = Location)) +
  geom_col(aes(fill = Location), alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.5, size = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") + 
  geom_jitter(data = MaxN_glm, aes(x = Location, y = MaxN_rep, color = Location, fill = Location, 
                                   group = Location), 
              alpha = 0.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.5))

pred.plot


# Ad hoc test GLM ---------------------------------------------------------

#Tukeys post hoc test to 
library(lsmeans)
tukeytest <- lsmeans(M1, pairwise~Location*Protection_status*Coastal_Oceanic, adjust="Holm")
#adjust with tukey or holm. Holm's method is more conservative than Tukey. 
tukeytest


# Zero-inflated data ------------------------------------------------------

library(VGAM)

M3 <- vglm(MaxN_rep ~ Location, family = posnegbinomial,
            control = vglm.control(maxit = 100),
            data = MaxN_glm)

M4 <- vglm(MaxN_rep ~ Location, family = pospoisson(),
           control = vglm.control(maxit = 100),
           data = MaxN_glm)

summary(M3)
summary(M4)


########################################################

library(lme4)

M1 <- glmer(MaxN_rep ~ Location + (1|Replicate), family = poisson, data = MaxN_glm)
M2 <- glmer.nb(MaxN_rep ~ Location + (1|Replicate), data = MaxN_glm)
summary(M1)
summary(M2)


library(ggeffects)
#Prediction dataframe at response scale with 95% confidence intervals
pred.df <- as.data.frame(ggpredict(M1, terms = c("Location"))) %>% 
  rename(Location = "x")

library(RColorBrewer)
#ggplot
pred.plot <- ggplot(data = pred.df, aes(x = Location, y = predicted, fill = Location, group = Location)) +
  geom_col(aes(fill = Location), alpha = 0.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), color = "black", width = 0.5, size = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") + 
  geom_jitter(data = MaxN_glm, aes(x = Location, y = MaxN_rep, color = Location, fill = Location, 
                                   group = Location), 
              alpha = 0.5, position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.5))

pred.plot


