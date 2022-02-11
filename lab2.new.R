# Assignment 2 - Mixed linear models

library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM	
library(skimr) # to skim for summary statistics 



stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

data_A <- read_csv("https://bit.ly/3FS0391")
data_B <- read_csv("https://bit.ly/3HayfOR")


# Data A ------------------------------------------------------------------

data_A = data_A %>% #recode sex to dummy variables
  mutate(sex = factor(recode(sex,
                             "male" = "1",
                             "female" = "0",
                             "woman" = "0")),
         hospital = factor(hospital, levels = c("hospital_1", 
                                                "hospital_2", 
                                                "hospital_3", 
                                                "hospital_4", 
                                                "hospital_5", 
                                                "hospital_6", 
                                                "hospital_7", 
                                                "hospital_8", 
                                                "hospital_9", 
                                                "hospital_10")))

skim(data_A) # checking out summary statistics 

# simple regression model with only fixed effect terms
mod_fixed = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_A)

mod_fixed %>% 
  summary() 

confint(mod_fixed, level = 0.95) #confidence interval of the fixed effects predictors

r2beta(mod_fixed, method = "nsj", data = data_A) #marginal R2 with confidence interval

# marginal and conditional R squared values
r.squaredGLMM(mod_fixed)

sum(residuals(mod_fixed)^2) #prediction efficiency error of fixed model - RSS

cAIC(mod_fixed)$caic # Conditional AIC

                  
# a random intercept model with random effect term hospital-ID

mod_rnd_int <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + 
                      (1 | hospital), data = data_A) 
summary(mod_rnd_int)

confint(mod_rnd_int, level = 0.95)
r.squaredGLMM(mod_rnd_int)  #conditional R2
sum(residuals(mod_rnd_int)^2)  #prediction efficiency of random intercept - RSS
cAIC(mod_rnd_int)$caic  #conditional AIC

r2beta(mod_rnd_int, method = "nsj", data = data_A)
plot(mod_rnd_int)

pred_rnd_int = predict(mod_rnd_int)

data_A%>%
  ggplot() + aes(y = pain, x = age) + 
  geom_point(aes(color = hospital),size = 4) +
  geom_smooth(method = "lm", se = F)

data_A%>%
  ggplot() + aes(y = pain, x = cortisol_serum) + 
  geom_point(aes(color = hospital),size = 4) +
  geom_smooth(method = "lm", se = F) 

int_plot = data_A %>%
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)
int_plot

int_plot + xlim(-1, 50) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)


slope_plot = data_A%>%
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE) + xlim(-1, 50) + geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
slope_plot
# Data B ------------------------------------------------------------------

data_B = data_B %>% #recode sex to dummy variables
  mutate(sex = factor(recode(sex,
                             "male" = "1",
                             "female" = "0",
                             "woman" = "0")),
         hospital = factor(hospital, levels = c("hospital_11", 
                                                "hospital_12", 
                                                "hospital_13", 
                                                "hospital_14", 
                                                "hospital_15", 
                                                "hospital_16", 
                                                "hospital_17", 
                                                "hospital_18", 
                                                "hospital_19", 
                                                "hospital_20")))

pred = predict(mod_rnd_int, newdata = data_B, allow.new.levels = T)
pred

RSS = sum(abs(data_B$pain - pred)^2)
RSS
    # 307.3396

mod_b_mean = lm(pain ~ 1, data = data_B)  # maybe change to data_B_df
mod_b_mean 

TSS = sum(abs(data_B$pain - predict(mod_b_mean)) ^ 2)
TSS
 # 4.95

R2 =  1 - (RSS/TSS)
R2
 # 0.3797384 - The variance explained by the predictive model is 38 %



# Model C -----------------------------------------------------------------

stdCoef.merMod(mod_rnd_int)

mod_rnd_slope = lmer(pain ~ cortisol_serum + 
                       (cortisol_serum | hospital), 
                     data = data_A)

pred_slope = predict(mod_rnd_slope)
pred_slope

pred_int = predict(mod_rnd_slope)

graph_rnd_slope = data_A %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + 
  geom_line(color = "red",
            aes(y = pred_slope, x = cortisol_serum)) + 
            facet_wrap(~hospital, ncol = 2)
graph_rnd_slope
graph_rnd_slope + labs(title = "Random slope model")

anova(mod_rnd_int, mod_rnd_slope)

cAIC(mod_rnd_slope)$caic

confint(mod_rnd_int)

# Calculation RSS ---------------------------------------------------------


# Final table 

sm = summary(mod_rnd_int)		
sm_p_values = as.character(round(sm$coefficients[,"Pr(>|t|)"], 3))		
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))		
sm_p_values1[sm_p_values == "0"] = "<.001"		

coef_CI = suppressWarnings(confint(mod_rnd_int))		

sm_table = cbind(as.data.frame(round(cbind(as.data.frame(sm$coefficients[,"Estimate"])), 
                                           coef_CI[c("(Intercept)", "cortisol_serum")], 
                                           c(0, stdCoef.merMod(mod_rnd_int)[2,1]), 2)), 
                                              sm_p_values)		
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
sm_table["(Intercept)","Std.Beta"] = "0"		
sm_table		




# R2 -marginal and conditional 

cAIC_int = round(cAIC(mod_rnd_int)$caic, 2)	
cAIC_slope = round(cAIC(mod_rnd_slope)$caic, 2)		
chisq = round(anova(mod_rnd_int, mod_rnd_slope)$Chisq[2], 2)		
chisq_p = round(anova(mod_rnd_int, mod_rnd_slope)$Pr[2], 3)		
chisq_df = anova(mod_rnd_int, mod_rnd_slope)[2,"Chi Df"]		
R2 = round(as.data.frame(r2beta(mod_rnd_slope, method = "nsj", data = data_A))[1,"Rsq"], 4)		
R2ub = round(as.data.frame(r2beta(mod_rnd_slope, method = "nsj", data = data_A))[1,"upper.CL"], 2)		
R2lb = round(as.data.frame(r2beta(mod_rnd_slope, method = "nsj", data = data_A))[1,"lower.CL"], 2)		

R2
R2ub
R2lb

chisq
 # 50.72

chisq_df
#

chisq_p
 # 0
cAIC_int
 #621.43
cAIC_slope
 #664.54
chisq
chisq_df
anova(mod_rnd_int, mod_rnd_slope)

coef(mod_rnd_int)

write.table(data_B, file = "data.csv",
            sep = "\t", row.names = F)
