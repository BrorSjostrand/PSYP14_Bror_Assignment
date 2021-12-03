# ZK home assignment part 3

#packages
library(psych) # for describe\t
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath

# custom function
# This is a function to extract standardized beta coefficients from linear mixed models.
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

d3_raw = read.csv("https://tinyurl.com/b385chpu")
d4_raw = read.csv("https://tinyurl.com/4f8thztv")

#checking the data

str(d3_raw)
glimpse(d3_raw)
summary(d3_raw)
describe(d3_raw)

ggplot(d3_raw, aes(pain))+
  geom_bar()

ggplot(d3_raw, aes(sex))+
  geom_bar()

ggplot(d3_raw, aes(age))+
  geom_bar()

ggplot(d3_raw, aes(STAI_trait))+
  geom_bar()

ggplot(d3_raw, aes(pain_cat))+
  geom_bar()

ggplot(d3_raw, aes(mindfulness))+
  geom_bar()

ggplot(d3_raw, aes(cortisol_serum))+
  geom_histogram()

ggplot(d3_raw, aes(cortisol_saliva))+
  geom_histogram()

ggplot(d3_raw, aes(weight))+
  geom_histogram()

ggplot(d3_raw, aes(IQ))+
  geom_histogram()

ggplot(d3_raw, aes(household_income))+
  geom_histogram()

ggplot(d3_raw, aes(hospital))+
  geom_bar()

# a couple of minor issues discovered.
# in one case sex is stated as "woman" instead of "female"
# also one participant is described as having a negative household income.
# the last issue can be ignored since it won't affect the model.
# The first issue will be resolved.

d3=d3_raw %>% 
  mutate(sex=replace(sex, sex=="woman", "female"))

ggplot(d3, aes(sex))+
  geom_bar()

# First, build a linear mixed model on data file 3, 
# accounting for the clustering of the data at different hospital sites. 
# We have no reason to assume that the effects of the different 
# predictors would be different in the different hospitals, 
# so fit a random intercept model including the random intercept of hospital-ID, 
#and the fixed effect predictors you used in assignment part 1.

# m2_new =lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = d3)

m3 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +
            cortisol_serum + (1 | hospital), data = d3)

# Once the model is built, note the model coefficients 
# and the confidence intervals of the coefficients for all fixed effect predictors, 
# and compare them to the ones obtained in assignment part 1. 

m3

stdCoef.merMod(m3)

confint(m3)

summary(m3)

r2beta(m3, method = "nsj", data = d3)

m2c

summary(m2)

confint(m2)

#calculating mean value of standardized beta

(-0.19 + 0.08 + (-0.08)+0.26+(-0.14)+0.34)/6

(-0.15+0.06+(-0.03)+0.4+(-0.17)+0.33)/6


# Also, compute the variance explained by the fixed effect predictors using marginal R2, 
# and the variance explained by the fixed and random effect terms combined using conditional R2.

r.squaredGLMM(m3)



# checking data file 4


str(d4_raw)
glimpse(d4_raw)
summary(d4_raw)

ggplot(d4_raw, aes(pain))+
  geom_bar()

ggplot(d4_raw, aes(sex))+
  geom_bar()

ggplot(d4_raw, aes(age))+
  geom_bar()

ggplot(d4_raw, aes(STAI_trait))+
  geom_bar()

ggplot(d4_raw, aes(pain_cat))+
  geom_bar()

ggplot(d4_raw, aes(mindfulness))+
  geom_bar()

ggplot(d4_raw, aes(cortisol_serum))+
  geom_histogram()

ggplot(d4_raw, aes(cortisol_saliva))+
  geom_histogram()

ggplot(d4_raw, aes(weight))+
  geom_histogram()

ggplot(d4_raw, aes(IQ))+
  geom_histogram()

ggplot(d4_raw, aes(household_income))+
  geom_histogram()

ggplot(d4_raw, aes(hospital))+
  geom_bar()




# Now use the regression equation obtained on data file 3 to predict pain in data file 4. 


prediction_m3 = predict(m3, newdata = d4_raw, allow.new.levels = TRUE)


# Now compute the variance explained by the model on data file 4. 
# You can do this by using the formula we learned in class: 1-(RSS/TSS). 
# Compare this R2 to the marginal and conditional R2 values 
# computed for the model on data file 3.


m3_RSS = sum((d4_raw$pain - predict(m3, newdata = d4_raw, allow.new.levels = TRUE))^2)
m2_RSS = sum((d4_raw$pain - predict(m2, newdata = d4_raw))^2)

mod_mean4 <- lm(pain ~ 1, data = d4_raw)
TSS4 = sum((d4_raw$pain - predict(mod_mean4))^2)

1-(m3_RSS/TSS4)

# Build a new linear mixed effects model on dataset 3 predicting pain. 
# However, instead of including all predictors, 
# you should only include the most influential predictor from the previous model. 
# Allow for both random intercept and random slope. 
# Now visualize the fitted regression lines for each hospital separately.

summary(m3)
m_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = d3)



summary(m_slope)

d3p = d3 %>%
  mutate(pred_slope = predict(m_slope))


d3p %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 2) + geom_line(color = "red",
            aes(y = pred_slope, x = cortisol_serum,)) + facet_wrap(~hospital, ncol = 2)







# Model diagnostics is optional

# checking for influential outliers

infl_obs = influence(m3, obs = T)$alt.fixed # this can take a minute or so
infl_group = influence(m3, group = "hospital")$alt.fixed

data_plot_infl_o = as_tibble(infl_obs) %>%
  gather(colnames(infl_obs), value = coefficient, key = predictor)

data_plot_infl_g = as_tibble(infl_group) %>%
  gather(colnames(infl_group), value = coefficient, key = predictor)

data_plot_infl_o %>%
  ggplot() + aes(x = 1, y = coefficient, group = predictor) +
  geom_violin() + geom_jitter(width = 0.2) + facet_wrap(~predictor,
                                                        scales = "free")

data_plot_infl_g %>%
  ggplot() + aes(x = 1, y = coefficient, group = predictor) +
  geom_violin() + geom_jitter(width = 0.2) + facet_wrap(~predictor,
                                                        scales = "free")



data_plot_infl_o %>%
  ggplot() + aes(x = 1, y = coefficient, group = predictor) +
  geom_boxplot() + facet_wrap(~predictor, scales = "free")


data_plot_infl_g %>%
  ggplot() + aes(x = 1, y = coefficient, group = predictor) +
  geom_boxplot() + facet_wrap(~predictor, scales = "free")


# checking normality

qqmath(m3, id = 0.05)


d3 %>%
  ggplot() + aes(sample = resid) + stat_qq() + stat_qq_line() +
  facet_wrap(~hospital, scales = "free")

qqmath(ranef(m3))




# checking linearity

plot(m3, arg = "pearson")

