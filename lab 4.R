library(psychTools)
library(lavaan)
library(semPlot)
library(ICS)
library(mvtnorm)


my_data = holzinger.swineford

Model_A = '
  #measurement model
VPA =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
VA =~ t06_paracomp + t07_sentcomp +  t09_wordmean 
PS =~ t10_addition + t12_countdot + t13_sccaps 

 #regressions
 
 #residual correlations
'


# Task 1 ------------------------------------------------------------------



fitA = sem(Model_A, data = my_data) # The df is 32, which means the model is identified and can be used

plotA = semPaths(fitA) 

summary(fitA)

# Checking the multivariate normality assumption 


mvnorm.kur.test(my_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges",
                           "t06_paracomp", "t07_sentcomp", "t09_wordmean",
                           "t10_addition", "t12_countdot","t13_sccaps")])

mvnorm.skew.test(my_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges",
                            "t06_paracomp", "t07_sentcomp", "t09_wordmean",
                            "t10_addition", "t12_countdot","t13_sccaps")])
# The assumption has been violated.

fitA_MLM = sem(Model_A, data = my_data, estimator = "MLM")
summary(fitA_MLM, fit.measures = T)
# MLM is to be preferred compared to bootstrap, since the overall results of the fit indeces
# gave better results 

fitA_boot <- sem(Model_A, data = my_data, se = "bootstrap", test = "bootstrap")
summary(fitA_boot)
summary(fitA_boot, fit.measures =  T)


# Task 2 and Task 3 ------------------------------------------------------------------

Model_B = '
#measurement model
VPA =~ t01_visperc + t02_cubes + t03_frmbord + t04_lozenges
VA =~ t06_paracomp + t07_sentcomp +  t09_wordmean 
PS =~ t10_addition + t12_countdot + t13_sccaps 

#regressions

#residual correlations
t10_addition ~~ t12_countdot
'

fitB = sem(Model_B, data = my_data)

PlotB = semPaths(fitB) 
PlotB

fitB_MLM = sem(Model_B, data = my_data, estimator = "MLM") 
# Comparison model fit (AIC and Chi-squared-change-test)
anova(fitA_MLM, fitB_MLM)

summary(fitB_MLM, standardized = T, rsquare = T)

Path_B = semPaths(fitB_MLM, whatLabels = "std")

# t02_cubes is the least influenced by VPA.


#Calculating the value of t_13 when t01 increses by one unit
c * (a+b)
a= 0.23
b = 0.38
c = 0.31

value = c +(a*b)
value
 # 0.40 - the total effect of 1 unit increase in t01.