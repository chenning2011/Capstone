#### descriptive statistics ##########

library(descr)
library(tidyverse)
load("cleaned.Rdata")
load("duplicates.Rdata")

##### takeaways ############
#sig. relationship btwn success (binary) and assessed risk level 
#sig. relationship between success and program 
#sig. relationship between length of stay and success
#sig. relationship between form and success 

############function###########
#creating a function to run chisquared test and create bar graph 
bivariate <- function(data, explanatory, response, levels=NULL){
  
  #turning explanatory variable into factor
  if (!is.null(levels)){
    data[[explanatory]] <- factor(data[[explanatory]], levels = levels)
  }
  
  #running chisquared test
  myChi <- chisq.test(data[[response]], data[[explanatory]])
  
  #running post-hoc for chisquared test 
  source("https://raw.githubusercontent.com/PassionDrivenStatistics/R/master/ChiSquarePostHoc.R")
  Observed_table<-myChi$observed
  posthoc <- chisq.post.hoc(Observed_table, popsInRows=FALSE, control="bonferroni")
  
  #output from chisquared test and post-hoc test
  print(paste("Chisquared test for", explanatory, "and", response))
  print(myChi)
  print(paste("Post-hoc test for", explanatory, "and", response))
  print(posthoc)
  
  #taking a subset of the data 
  sub <- data[,c(explanatory, response)]
  sub <- na.omit(sub)
  
  #creating and printing bar graph 
  a <- ggplot(data = sub)+
    stat_summary(aes(x=sub[[explanatory]], y=sub[[response]], fill=sub[[explanatory]]), fun = "mean", geom="bar")+
    theme_minimal()+
    theme(legend.position = "none")+
    labs(title = paste("Proportion of Successful Outcomes by",explanatory), x = explanatory, y = "Proportion of Successful Outcomes")
  print(a)
}

##### analysis ############
#risk level 
bivariate(RiskClientScores, "RiskLevel", "Success",
          levels = c("Low", "Moderate", "High", "Very High"))
#significant relationship btween risk level and success (X2 = 20.307, p = 0.0001466)
#significant difference between low and high (adj.p = 0.0001), and moderate and high (p=0.0207)

#race
bivariate(RiskClientScores, "Race", "Success", 
          levels = c("Caucasian or White", "African American or Black", "Asian",
                     "Native", "Multi-Racial", "Other"))
#not sig. (X2 = 4.6412, p = 0.4612)
freq(RiskClientScores$Race)
#this could be because there are only 2 asian respondents - might want to group together more

#ethnicity 
bivariate(RiskClientScores, "Ethnicity", "Success")
#no sig. relationship (X2 = 6.306, p = 0.3898)
freq(RiskClientScores$Ethnicity)
#similar problem to race, going to try grouping into yes vs no instead

#hispanic 
bivariate(RiskClientScores, "Hispanic", "Success")
#no sig. relationship (X2 = 3.4766, p = 0.06224)
freq(RiskClientScores$Hispanic)

#lgbtq 
bivariate(RiskClientScores, "IdentifiesAsLGBTQ", "Success")
#no sig. relationship (X2 = 1.5562, p = 0.2122), but only 18 yes'es
freq(RiskClientScores$IdentifiesAsLGBTQ)

#marital status 
bivariate(RiskClientScores, "MaritalStatus", "Success",
          levels = c("Married", "Single/Never Married", "Divorced/Annulled",
                     "Legally separated", "Widow/widower", "Other"))
#no sig. relationship (X2 = 3.5526, p = 0.6154)
freq(RiskClientScores$MaritalStatus)
#mostly single people

#religion 
bivariate(RiskClientScores, "Religion", "Success")
#no sig. relationship (X2 = 8.18, p = 0.2252)
freq(RiskClientScores$Religion)
#mostly christians

#primary language 
bivariate(RiskClientScores, "PrimaryLanguage", "Success")
#no sig. relationship (X2 = 5.3781, p = 0.2507)
freq(RiskClientScores$PrimaryLanguage)
#not many participants who speak primary languages other than english 

#veteran 
bivariate(RiskClientScores, "UsVeteran", "Success")
#no sig. relationship (X2 > 100, p = 1)
freq(RiskClientScores$UsVeteran)
#biased by there being basically no veterans

#suicide risk 
bivariate(RiskClientScores, "SuicideRiskLevel", "Success")
#no sig. relationship (X2 = 6.2786, p = 0.04331)
freq(RiskClientScores$SuicideRiskLevel)
#likely influenced by there being basically no one with suicide risk

#homicide risk  
bivariate(RiskClientScores, "HomocideRiskLevel", "Success")
#no sig. relationship (X2 = 4.5689, p = 0.1018)

#program 
bivariate(RiskClientScores, "ProgramName", "Success")
#sig. relationship(X2 = 34.633, p < 0.0001)
#sig. difference between Eddy Center and REACH (p =0.0000), and between REACH and the January Center (p = 0.0002)

#form 
bivariate(RiskClientScores, "Form", "Success")
#sig. relationship between form and success (X2 = 10.874, p = 0.004352)
#sig. relationship between RT and SRT (p = 0.0037)

###### quantitative explanatories ########

#length of stay and outcome 
logreg <- glm(Success ~ LengthOfStay, data = RiskClientScores, family = "binomial") 
summary(logreg)  
exp(logreg$coefficients)  
#sig. relationship between length of stay and success (z=3.459, p < 0.001)

#age and outcome
logreg <- glm(Success ~ AgeAtAdmission, data = RiskClientScores, family = "binomial") 
summary(logreg)  
exp(logreg$coefficients)  
#no sig. relationship between age and success (z = -0.493, p = 0.6219)
