library(tidyverse) # for dplyr and ggplot2
library (lmtest) # for lrtest
library(pscl)# for pR2
library(skimr) # for summary stats
library(rpart) # for prediction of age
library(dominanceanalysis) # for dominance analysis 
library(modelsummary) # for the tables 


data_t = read_csv("https://bit.ly/31Gl1d2")

data_t = data_t%>%
  mutate(Sex = factor(recode(Sex,
                             "male" = "1",
                             "female" = "0")),
         Name = as.character(Name),
         Survived = as_factor(Survived),
         Pclass = as_factor(Pclass),
         Embarked = as_factor(Embarked), 
         Age = as.numeric(Age),
         SibSp = as.numeric(SibSp),
         Ticket = as.factor(Ticket),
         Fare = as.numeric(Fare),
         Cabin = as_factor(Cabin),
         Parch = as.numeric(Parch),
         PassengerId = as.factor(PassengerId)) 

# Data preperation --------------------------------------------------------

 # Creating a new variable for Age to use for the descision tree
data_t$Age_RE <- data_t$Age

summary(data_t$Age_RE)
  # 177 NA's

# Predicting the missing values of age by using the method descision tree
Age_Replaced = rpart(Age_RE ~ 
                       PassengerId + 
                       Survived + 
                       Pclass + 
                       Sex + 
                       SibSp +
                       Parch + 
                       Ticket + 
                       Fare + 
                       Cabin +
                       Embarked, data=data_t[!is.na(data_t$Age_RE), ],  
                     method="anova")

#Fill AGE missing values with prediction made by decision tree prediction
data_t$Age_RE[is.na(data_t$Age_RE)] = predict(Age_Replaced, data_t[is.na(data_t$Age_RE),])


#confirm the missing values have been filled
summary(data_t$Age_RE)

# Replacing orginal Age with Age_RE
data_t$Age = data_t$Age_RE

#Removing AGE_RE
data_t <- subset(data_t, select = -c(Age_RE))

# Creating a logistic regression model 

finalmodel = glm(Survived ~ Sex + Age + SibSp + Parch + Pclass, family = binomial(), data = data_t)
summary(finalmodel)

# McFaddens R2-squared to check the model perfomance 
pR2(finalmodel)

# -2LL, deviance
pR2(finalmodel)["llh"] * -2

# Percentage of survived passangers
data_t %>%
  group_by(Survived) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
    # Passengers that didn't survive: ~ 62 % (549 cases)
    # Passengers that did survive: 38 % (342 cases)

# predicted value by categorization

data_t = data_t %>%
  mutate(pred_finalmodel = predict(finalmodel)) %>%
  mutate(pred_finalmodel = case_when(pred_finalmodel <= 0 ~ 0,
                                     pred_finalmodel> 0 ~ 1))

# coding correct guesses
data_t = data_t %>%
  mutate(correct_prediction = case_when(pred_finalmodel == Survived ~ "correct",
                                        pred_finalmodel != Survived ~ "incorrect"))

# correct categorization rate overall
data_t %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
  #79.9  % 712 out of 891 cases 

# correctly categorized as having survived
data_t %>%
  filter(Survived == "1") %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count)) 
    # 69 % 

# correctly categorized as not having survived
data_t %>%
  filter(Survived == "0") %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
    # 86.7 %  - The model was better at detecting the abscence of event of interest


 # Fine-tuning sensitivity of the final model
 # THe model was better at categorizing having not survived rather than having survived rate 
 # Therefore we are going to decrease the treshold by -0.4 
data_t = data_t %>% 
  mutate(pred_finalmodel_tuned = predict(finalmodel)) %>% 
  mutate(pred_finalmodel_tuned = case_when(pred_finalmodel_tuned <= -0.4 ~ 0,
                                           pred_finalmodel_tuned > -0.4 ~ 1))

# coding correct guesses

data_t = data_t %>% 
  mutate(correct_prediction_tuned = 
           case_when(pred_finalmodel_tuned == Survived ~ "correct",
                     pred_finalmodel_tuned != Survived ~ "incorrect"))

# correct categorization rate overall
data_t %>%
  group_by(correct_prediction_tuned) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

  # correct: 78.5 %

# correctly categorized as having survived
data_t %>%
  filter(Survived == "1") %>%
  group_by(correct_prediction_tuned) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

      #correct: 76 % 

# correctly categorized as having not having survived
data_t %>%
  filter(Survived == "0") %>%
  group_by(correct_prediction_tuned) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))
      # correct: 80 % 

# Creating null model
finalmodel_null = glm(Survived ~ 1, family = binomial(), data = data_t)
summary(finalmodel_null)

# comparing logistic regression model with the null model
AIC(finalmodel_null, finalmodel)

# AIC-test: comparing the likelihood ratio with of the final model with/and the null model 
lrtest(finalmodel_null, finalmodel)


## CIs using profiled log-likelihood

confint(finalmodel)

# Calculating the odds ratios of each predictor

exp(coef(finalmodel))

# Odds ratios and 95 CI

exp(cbind(OR = coef(finalmodel), confint(finalmodel)))


#Calculating the regression equations

Kate_withL = 3.941388 + -2.756920 * 0 +  -0.035722 * 20 + 
  -0.342353 * 1 + -0.063571 * 1 + -2.424834 * 0
Kate_withL
# Log(odds): 2.821024 of Kate surviving with the presence of Leonardo

Odds = exp(Kate_withL)
Odds 
# Odds: 16.79404 - 16.7 times more likely that Kate survives with Leonardo

p = Odds / (1 + Odds)
p 
# The predicted probability using our model of Kate's survival with Leonardo is 94 %

Kate_withoutL = 3.941388 + -2.756920 * 0 +  -0.035722 * 20 + 
  -0.342353 * 0 + -0.063571 * 1 + -2.424834 * 0

Kate_withoutL
# Log(odds): 3.163377

Odds_1 = exp(Kate_withoutL)
Odds_1 
# Odds: 23.65033  23 times more likely to survive without Leonardo

p_1 = Odds_1 / (1 + Odds_1)
p_1 
# 0.9594326: The predicted probability using our model of Kate's survival without L is 95.9 %

Sue_withL = 3.941388 + -2.756920 * 0 +  -0.035722 * 4 + 
  -0.342353 * 0 + -0.063571 * 2 + -2.424834 * 0
Sue_withL
# Log(Odds): 3.671358

Odds_2 = exp(Sue_withL)
Odds_2
# Odds: 39.30525 - 39 times more likely Sue would have survived in the prescence of Leonardo

p_2 = Odds_2 / (1 + Odds_2)
p_2
# 0.9751893: The predicted probability of her surviving with the presence of Leonardo is 97.5 % 

Sue_withoutL = 3.941388 + -2.756920 * 0 +  -0.035722 * 4 + 
  -0.342353 * 0 + -0.063571 * 1 + -2.424834 * 0

Sue_withoutL
# Log(odds): 3.734929

Odds_3 = exp(Sue_withoutL)
Odds_3
# Odds: 41.88505 - 42 more times likely of Sue to survive without Leonardo

p_3 = Odds_3 / (1 + Odds_3)
p_3
# 0.9766819: The predicted probability of her surviving without Leonardo is 97.6 % 


# # Dominance analysis  ---------------------------------------------------

# Dominance analysis 
dominance_finalmodel<-dominanceAnalysis(finalmodel)
contributionByLevel(dominance_finalmodel, fit.functions="r2.m")
averageContribution(dominance_finalmodel,fit.functions = "r2.m")
# The most influential predictor is sex as seen by the average contribution table
# This result is not surprising. Right after follows Pclass, which is also not surprising
#However the results show that age is apparently not the most influential predictor.
# Which again was visible when comparing age, gender and pclass, where 
# the majority of women in first class survived despite the passengers being older age average

  
# Plotting the dominance analysis

plot(dominance_finalmodel, which.graph ="conditional",fit.function = "r2.m")
plot(dominance_finalmodel, which.graph ="general",fit.function = "r2.m", decreasing = T) + coord_flip()


# Tables and plots --------------------------------------------------------


modelsummary(list("Null model" = finalmodel_null, 
                  "Logistic regression model" = finalmodel), stars =  T, 
                    statistic = c( "CI" = "conf.int", conf_level = 0.95,
                     "p" = "p.value"), title = "Table of statistics" )

eval(parse("https://raw.githubusercontent.com/RemPsyc/niceplots/master/niceTableFunction.R", 
           encoding = 'UTF-8'))
stats.table <- as.data.frame(summary(finalmodel)$coefficients)

apa.reg.table(finalmodel_null,finalmodel, filename = "Table1_APA.docx", table.number = 2) 
              
              
install.packages("gtsummary")
remotes::install_github("ddsjoberg/gtsummary")
library(gtsummary)

t1 = tbl_regression(finalmodel, exponentiate = TRUE, intercept = T)
t1
