##testing interactions between variables 

#steps: 
#1. adapt function to test for interactions (can handle three vars now instead of 2)
  #. probably will only need to do the graph, then do a regression clustered by ID with all three vars
#2. run the function on all demographic variables 
#3. look at results and figure out what to do from there 
#4. if time, look into CTP data and dimensionality reduction with that and the other scores and stuff idk 
#5. also try a random forest to isolate the most important variables (probably more important than a PCA thing)

##### function ####
library(tidyverse)
library(sandwich)
library(lmtest)
load("cleaned.Rdata")
load("duplicates.Rdata")

interaction <- function(data, response, explanatory, interaction, levels=NULL){
  
  #turning explanatory variable into factor
  if (!is.null(levels)){
    data[[explanatory]] <- factor(data[[explanatory]], levels = levels)
  }
  
  #creating formula for logistic regression 
  formula <- as.formula(paste(response, "~", explanatory, "*", interaction))
  
  #running logistic regression 
  logreg <- glm(formula, data = data, family = "binomial")
  
  #clustered standard errors 
  clustered <- vcovCL(logreg, cluster = ~ data$StudyClientId, type = "HC0")
  
  #getting coefficients with clustered standard errors 
  logreg_clustered <- coeftest(logreg, vcov = clustered)
  
  #output from logistic regression with clustered standard erros
  print(paste0("Logistic regression for", explanatory, ",", response, ", and", interaction, "with clustered standard errors"))
  print(logreg_clustered)
  
  ##taking a subset of the data 
  sub <- data[,c(explanatory, response, interaction)]
  sub <- na.omit(sub)
  
  #creating and printing bar graph 
  a <- ggplot(data = sub)+
    stat_summary(aes(x=sub[[explanatory]], y=sub[[response]], fill=sub[[interaction]]), fun = "mean", geom="bar", position = "dodge")+
    theme_minimal()+
    labs(title = paste("Proportion of Successful Outcomes by", explanatory, "and", interaction), x = explanatory, y = "Proportion of Successful Outcomes",
         fill = interaction)
  print(a)
}

#risk level and race 
interaction(RiskClientScores, "Success", "RiskLevel", "Race")
#no interaction btwn the two variables, risk level is significant when comparing high to low (z=-2.4174, p = 0.0156)
#after controlling for race, participants with a risk level of high are 49.78% less likely to have successful outcomes than participants with a low risk level 

#risk level and hispanic 
interaction(RiskClientScores, "Success", "RiskLevel", "Hispanic")
#no interaction, but risk level is significant when comparing moderate to low (z = -2.3373, p = 0.019423) and high to low (z = -2.8275, p = 0.004691)
#after controlling for hispanic ethnicity, participants with a risk level of high are 51.58% less likely to have successful outcomes than participants with a low risk level
#after controlling for hispanic ethnicity, participants with a risk level of moderate are 35.79% less likely to have successful outcomes than participants with a low risk level

#risk level and martial status 
interaction(RiskClientScores, "Success", "RiskLevel", "MaritalStatus")
#there is significance in the interaction term, but only bc there are only 6 participants with the "other" marital status 
#sig. when looking at risk level high * other and risk level moderate * other, but looking at the graph reveals this is not actually worth looking into further 

#risk level and 