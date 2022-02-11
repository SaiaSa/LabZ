# Lab assignment 3 - Dimension reduction technique

library(imputeTS) # for replacing na - mean values 
library(psych) # for the mixedCor, cortest.bartlett, KMO, fa functions
library(car) # for vif
library(GPArotation) # for the psych fa function to have the required rotation functionalities
library(MVN) # for mvn function
library(ICS) # for multivariate skew and kurtosis test
library(tidyverse) # for tidy code


fviz_loadnings_with_cor <- function(mod, axes = 1, loadings_above = 0.4){	
  require(factoextra)	
  require(dplyr)	
  require(ggplot2)	
  
  
  
  if(!is.na(as.character(mod$call$call)[1])){	
    if(as.character(mod$call$call)[1] == "PCA"){	
      contrib_and_cov = as.data.frame(rbind(mod[["var"]][["contrib"]], mod[["var"]][["cor"]]))	
      
      vars = rownames(mod[["var"]][["contrib"]])	
      attribute_type = rep(c("contribution","correlation"), each = length(vars))	
      contrib_and_cov = cbind(contrib_and_cov, attribute_type)	
      contrib_and_cov	
      
      plot_data = cbind(as.data.frame(cbind(contrib_and_cov[contrib_and_cov[,"attribute_type"] == "contribution",axes], contrib_and_cov[contrib_and_cov[,"attribute_type"] == "correlation",axes])), vars)	
      names(plot_data) = c("contribution", "correlation", "vars")	
      
      plot_data = plot_data %>% 	
        mutate(correlation = round(correlation, 2))	
      
      plot = plot_data %>% 	
        ggplot() +	
        aes(x = reorder(vars, contribution), y = contribution, gradient = correlation, label = correlation)+	
        geom_col(aes(fill = correlation)) +	
        geom_hline(yintercept = mean(plot_data$contribution), col = "red", lty = "dashed") + scale_fill_gradient2() +	
        xlab("variable") +	
        coord_flip() +	
        geom_label(color = "black", fontface = "bold", position = position_dodge(0.5))	
      
      
    }	
  } else if(!is.na(as.character(mod$Call)[1])){	
    
    if(as.character(mod$Call)[1] == "fa"){	
      loadings_table = mod$loadings %>% 	
        matrix(ncol = ncol(mod$loadings)) %>% 	
        as_tibble() %>% 	
        mutate(variable = mod$loadings %>% rownames()) %>% 	
        gather(factor, loading, -variable) %>% 	
        mutate(sign = if_else(loading >= 0, "positive", "negative"))	
      
      if(!is.null(loadings_above)){	
        loadings_table[abs(loadings_table[,"loading"]) < loadings_above,"loading"] = NA	
        loadings_table = loadings_table[!is.na(loadings_table[,"loading"]),]	
      }	
      
      if(!is.null(axes)){	
        
        loadings_table = loadings_table %>% 	
          filter(factor == paste0("V",axes))	
      }	
      
      
      plot = loadings_table %>% 	
        ggplot() +	
        aes(y = loading %>% abs(), x = reorder(variable, abs(loading)), fill = loading, label =       round(loading, 2)) +	
        geom_col(position = "dodge") +	
        scale_fill_gradient2() +	
        coord_flip() +	
        geom_label(color = "black", fill = "white", fontface = "bold", position = position_dodge(0.5)) +	
        facet_wrap(~factor) +	
        labs(y = "Loading strength", x = "Variable")	
    }	
  }	
  
  
  
  
  
  
  return(plot)	
  
}	
      
arqdf = read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Exercise_06%20-%20CFA%20and%20EFA/animalrights.csv")


# Transform the data ------------------------------------------------------


# Reverse scoring items 

columnsToReverse <- c("ar16", "ar19", "ar21", "ar24", "ar28")


arqdf[,columnsToReverse] <- 6-arqdf[,columnsToReverse]


# Replace the NA in each variable with the median of each variable because they are skewed

arqdf_replaced = arqdf %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))


# Checking for outliers

bplot_arqdf = boxplot(arqdf, beside = T)
bplot_arqdf

boxplot.stats(arqdf, coef = 1.5)

boxplot(arqdf)


# Builing a linear model to check potential features as well as accounting for multicollinearity

mod_allitems = lm(liberal ~ ar1 + ar2 + ar3 + ar4 + ar5 + ar6 + ar7 + ar8 + ar9 + 
    ar10 + ar11 + ar12 + ar13 + ar14 + ar15 + ar16 + ar17 + ar18 +
    ar19 + ar20 + ar21 + ar22 + ar23 + ar24 + ar25 + 
    ar26 + ar27 + ar28 + sex + party, data = arqdf_replaced)

plot(mod_allitems)


# creating a dataframe with only the items of relevance for the EFA

arqdf_items_only = arqdf_replaced %>% 
  dplyr::select(ar1:ar28)


# Factorability -----------------------------------------------------------
# Storing the correlation matrix in variable cor, that only contains the numerical
# the numerical variables

arqdfcorrel = cor(arqdf_items_only)  

# Bartlett test

bfi_factorability <- cortest.bartlett(arqdfcorrel)
bfi_factorability
  # The ratio score is higher than 5, which means this test can not be considered
  # to be reliable, therefore a KMO-test will be conducted. 

# KMO test
KMO(arqdfcorrel)

  # The results of the KMO shows that the variables have reasonable factorability
  # The KMO is < 0.6 in all cases 

# Factor extraction -------------------------------------------------------

# To check if the multivariate assumption has been violated or not

result = mvn(arqdf_replaced[, 1:28], mvnTest = "hz")
result$multivariateNormality

    #The Henze-Zirkler test shows that the p-value is > 0.05

mvnorm.kur.test(arqdf_replaced[, 1:28])

    # The multivariate kurtosis test shows that the p-value is < 0.05

mvnorm.skew.test(arqdf_replaced[, 1:28])

    # The multivariate skewedness test shows that the p-value is < 0.05

 # The assumption of multivariate normality has been violated, 
   # which is not surprising considering the exploratory data analysis
   # showed us that all of the variables are skewed. 

# Creating a model with factor 5 to start off the exploration of the fractor extraction
EFA_mod1 <- fa(arqdfcorrel, nfactors = 5, fm = "pa")
# Sorted communality
EFA_mod1_common <- as.data.frame(sort(EFA_mod1$communality, decreasing = TRUE))
EFA_mod1_common


# Calculating the average communality
mean(EFA_mod1$communality)
  # ~0.44, hence the average communicality is lower than 0.6


# Choosing the ideal number of factors by testing the four tests
 #Creating eigenvalues 
ev = eigen(cor(arqdfcorrel)) # get eigenvalues
ev$values

#Scree test

scree(arqdfcorrel, pc = F)
 # Two factors 

# Parallell test(analysis)

fa.parallel(arqdfcorrel, fa="fa")

 # 2 factors 

# VSS-test and MAP-test
 vss(arqdfcorrel)

  # The VSS suggest 1-2 factors to keep and MAP 2 factors

 

# Factor rotation ---------------------------------------------------------
 
 # Creating the factor rotation for mod3 with only 2 factor/comp.
  EFA_mod_promax3 <- fa(arqdfcorrel, nfactors = 2, fm = "pa", rotate = "promax")
    #Oblique rotation
 EFA_mod_varimax3 <- fa(arqdfcorrel, nfactors = 2, fm = "pa", rotate = "varimax")
    #Orthogonal rotation
 
 #The chosen rotation technique is promax (oblique)
 
 fa.diagram(EFA_mod_promax3)
 fa.diagram(EFA_mod_varimax3)
 
 # Post-extraction communality with two factors 
 
 EFA_mod_promax3 = fa(arqdfcorrel, nfactors = 2, fm = "pa")
 EFA_mod_promax3_common<- as.data.frame(sort(EFA_mod_promax3$communality, decreasing = TRUE))
 EFA_mod_promax3
 mean(EFA_mod_promax3$communality)
 # 0.3410226
 
# Loadings ----------------------------------------------------------------
  
 # Loadings from the models with 2 factors
 fviz_loadnings_with_cor(EFA_mod_promax3, axes = 1, loadings_above = 0.4)
  #Consumption of meat and one item about hunting? 
 fviz_loadnings_with_cor(EFA_mod_promax3, axes = 2, loadings_above = 0.4)
   #Use of animals for medical purposes / other commercial purposes
 

 fviz_loadnings_with_cor(EFA_mod_varimax3, axes = 1, loadings_above = 0.4)
  # Morals? For example "Not morally right to wear fur"
 
 fviz_loadnings_with_cor(EFA_mod_varimax3, axes = 2, loadings_above = 0.4)
  #Animal rights in medical research and as beings
 
 # Saving factor scores 
 
 factorscores = factor.scores(arqdf_replaced[, 1:28], EFA_mod_promax3)$scores
 arqdf_replaced_with_factorscores = cbind(arqdf_replaced, factorscores)

 summary(arqdf_replaced_with_factorscores)
 
 # Giving new names to the factor structures
 
 # Regression model 
 
 arqdf_replaced_with_factorscores = arqdf_replaced_with_factorscores %>% 
   rename(values = PA1,
          research = PA2)

 lm_model = lm(liberal ~ values + research, arqdf_replaced_with_factorscores)
 summary(lm_model)
 
 

 