# ZK Home assignment part 1 clean

library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse) # for tidy code

#data sample 1
d1_raw = read.csv("https://tinyurl.com/ha-dataset1")

# checking the variables included in model 2 for coding errors.

str(d1_raw)
glimpse(d1_raw)
summary(d1_raw)
describe(d1_raw)

ggplot(d1_raw, aes(pain))+
  geom_histogram()

ggplot(d1_raw, aes(pain))+
  geom_bar()

ggplot(d1_raw, aes(sex))+
  geom_bar()

ggplot(d1_raw, aes(age))+
  geom_bar()

ggplot(d1_raw, aes(STAI_trait))+
  geom_bar()


# the pain scale goes from 0-10 but there is one value that is 55.
# a very plausible explanation is that the proper value is 5, 
# but that another digit was added by mistake.
# Therefore I am deciding to change the value 55 to 5 in the data.

d1=d1_raw %>% 
  mutate(pain=replace(pain, pain==55, 5))

 
# Since the STAI is a scale from 20-80, the value of 4.2 should be impossible.
# A likely explanation is that the proper value is 42, 
# but that a decimal point was misplaced during data recording.
# Therefore I am deciding to change the value 4.2 to 42 in the data.

d1=d1 %>% 
  mutate(STAI_trait=replace(STAI_trait, STAI_trait==4.20, 42.00))


# Conduct a hierarchical regression, 
# building a model containing age and sex as predictors of pain (model 1)

m1 <- lm(pain ~ age + sex, data = d1)


# building a new model with the predictors: 
# age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures (model 2).

m2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +
           cortisol_serum + cortisol_saliva, data = d1)


# checking the model itself for influential outliers (for example using Cook's distance). 

4/160

m1 %>% 	
  plot(which = 5)	

m1 %>% 	
  plot(which = 4)	

m2 %>% 	
  plot(which = 5)	

m2 %>% 	
  plot(which = 4)	

# no problem for m1 or m2.

# Furthermore, check the final model to see if the assumptions of linear regression hold true, 
# that is, normality (of the residuals), 

m1 %>% 	
  plot(which = 2)	

m2 %>% 	
  plot(which = 2)	

residuals_m2 = enframe(residuals(m2))	
residuals_m2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

describe(residuals(m2))

describe(residuals(m1))

# linearity (of the relationship), 

m1 %>% 	
  residualPlots()	


windows()
m2 %>% 	
  residualPlots()	

# Tukey test for m1 is significant.

# Since 47 seems to be the most influential outlier in both models (using Cook's distance), 
# and causes a violation of linearity for m1, I have decided to exclude them.

d1b = d1 %>% 	
  slice(-c(47))	


# making new models for the new data.

m1b <- lm(pain ~ age + sex, data = d1b)


m2b <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +
           cortisol_serum + cortisol_saliva, data = d1b)

# CHecking assumptions again

m1b %>% 	
  plot(which = 5)	

m1b %>% 	
  plot(which = 4)	

m2b %>% 	
  plot(which = 5)	

m2b %>% 	
  plot(which = 4)	

# no problem for m1 or m2.

# Furthermore, check the final model to see if the assumptions of linear regression hold true, 
# that is, normality (of the residuals), 

m1b %>% 	
  plot(which = 2)	

m2b %>% 	
  plot(which = 2)	

describe(residuals(m2b))

describe(residuals(m1b))

# linearity (of the relationship), 

m1b %>% 	
  residualPlots()	


windows()
m2b %>% 	
  residualPlots()	

# No problems thus far.


# homogeneity of variance (also called homoscedasticity) 


m1b %>% 	
  plot(which = 3)	

m1b %>% 	
  ncvTest() # NCV test	

m1b %>% 	
  bptest() # Breush-Pagan test

m2b %>% 	
  plot(which = 3)	

m2b %>% 	
  ncvTest() # NCV test	

m2b %>% 	
  bptest() # Breush-Pagan test


# No problems detected


# check that there is no excess multicollinearity ("uncorrelated predictors" in Navarro's words).

m1b %>% 	
  vif()	

m2b %>% 	
  vif()	


# Here we find trouble. VIF for m2b is almost 5 for both cortisol measures.
# Since the threshold is 3, this must be dealt with.
# Removing the saliva cortisol measure seems to solve problems with multicollinearity

m2c <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +
              cortisol_serum, data = d1b)


m2c %>% 	
  plot(which = 5)	

m2c %>% 	
  plot(which = 4)	

m2c %>% 	
  plot(which = 2)	

describe(residuals(m2c))

m2c %>% 	
  plot(which = 3)	

m2c %>% 	
  ncvTest() # NCV test	

m2c %>% 	
  bptest() # Breush-Pagan test


windows()
m2c %>% 	
  residualPlots()	

summary(m1)
summary(m2c)

AIC(m1)
AIC(m2c)

anova(m1b, m2c)


# function to obtain regression coefficients	
# source: https://www.statmethods.net/advstats/bootstrapping.html	
bs_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(coef(fit)) 	
}	

bs_to_boot(m1, d1)

bs_to_boot(m2c, d1)


coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

coef_table(m1b)
coef_table(m2c)
