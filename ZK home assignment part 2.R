#packages
library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse) # for tidy code

#data sample 1
d1_raw = read.csv("https://tinyurl.com/ha-dataset1")

d1=d1_raw %>% 
  mutate(pain=replace(pain, pain==55, 5))

d1=d1 %>% 
  mutate(STAI_trait=replace(STAI_trait, STAI_trait==4.20, 42.00))

d1b = d1 %>% 	
  slice(-c(47))

# run a backward regression to confirm her claim. She used the 
#following variables as predictors in the initial model (before stepwise exclusion): 
#age, sex, STAI, pain catastrophizing, mindfulness, serum cortisol, weight, IQ, household income. 
#Run a backward regression using these predictors as an initial model.


m_initial = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + 
               cortisol_serum + weight + IQ + household_income, data=d1b)

# (Before you run the actual backward regression, 
# you will have to re-run the data and model diagnostics,
# as there are new variables in the model).

m_initial %>% 	
  plot(which = 5)	

m_initial %>% 	
  plot(which = 4)	

m_initial %>% 	
  plot(which = 2)	

describe(residuals(m_initial))

m_initial %>% 	
  plot(which = 3)	

m_initial %>% 	
  ncvTest() # NCV test	

m_initial %>% 	
  bptest() # Breush-Pagan test


windows()
m_initial %>% 	
  residualPlots()	

# No problems found

#Run a new regression model now only using the predictors that were retained in the end 
#of the backward regression, and save this model in a new R object. We will refer to this 
#model as the "backward model". Run the full regression model you arrived at in the end of 
#assignment part 1 again, and save this model in another R object. We will refer to this model 
#as the "theory-based model". Compare the backward model and the theory-based model 
#based on AIC (and using the anova() function if appropriate).

m_back = step(m_initial, direction = "backward")

m2c <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +
            cortisol_serum, data = d1b)
m_theory = m2c

AIC(m_back)
AIC(m_theory)
AIC(m_initial)

summary(m_back)
summary(m_initial)

coef_table(m_back)

bs_to_boot(m_back, d1)

#data sample 2
d2 = read.csv("https://tinyurl.com/87v6emky")


str(d2)
glimpse(d2)
summary(d2)

ggplot(d2, aes(pain))+
  geom_histogram()

ggplot(d2, aes(pain))+
  geom_bar()

ggplot(d2, aes(sex))+
  geom_bar()

ggplot(d2, aes(age))+
  geom_bar()

ggplot(d2, aes(STAI_trait))+
  geom_bar()

ggplot(d2, aes(pain_cat))+
  geom_bar()

ggplot(d2, aes(mindfulness))+
  geom_bar()

ggplot(d2, aes(cortisol_serum))+
  geom_histogram()


#Comparing the models

m_theory_RSS = sum((d2$pain - predict(m_theory, newdata = d2))^2)
m_back_RSS = sum((d2$pain - predict(m_back, newdata = d2))^2)

mod_mean <- lm(pain ~ 1, data = d2)
TSS = sum((d2$pain - predict(mod_mean))^2)

1 - (m_theory_RSS/TSS)
1 - (m_back_RSS/TSS)

anova(m_back, m_theory)

anova(m_initial, m_back)
anova(m_back, m_initial)

