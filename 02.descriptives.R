#### descriptive statistics ##########

library(descr)
library(tidyverse)
load("cleaned.Rdata")
load("duplicates.Rdata")

#to-dos:
  #clustering - is this going somewhere? 
  #how do the patterns matter in the context of everything else? 
#think about data management for discharge status 
#clustering on dimensions of risk/criminality could be interesting

##### takeaways ############
#sig. relationship btwn success (binary) and assessed risk level 
#sig. relationship between success and program 
#sig. relationship between length of stay and success
#sig. relationship between form and success 
#sig. relationship between suicide risk and success (x2=8.0972, p=0.01745)
  #sig. difference between moderate and no risk 

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
freq(RiskClientScores$RiskLevel)
#look at this in combination with demographic factors (does the relationship change by race? ethnicity? etc.)

#program 
bivariate(RiskClientScores, "ProgramName", "Success")
#sig. relationship(X2 = 34.633, p < 0.0001)
#sig. difference between Eddy Center and REACH (p =0.0000), and between REACH and the January Center (p = 0.0002)
#some are for higher risk/higher security people 
freq(RiskClientScores$ProgramName)

#form 
bivariate(RiskClientScores, "Form", "Success")
#sig. relationship between form and success (X2 = 10.874, p = 0.004352)
#sig. relationship between RT and SRT (p = 0.0037)

#race
bivariate(RiskClientScores, "Race", "Success", 
          levels = c("Caucasian or White", "African American or Black", "Other"))
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
bivariate(RiskClientScores, "MaritalStatus", "Success")
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

#primary language - only english and not english 
bivariate(RiskClientScores, "English", "Success")
#not sig. at all, not even worth reporting, but could be good for interaction stuff

#veteran 
bivariate(RiskClientScores, "UsVeteran", "Success")
#no sig. relationship (X2 > 100, p = 1)
freq(RiskClientScores$UsVeteran)
#biased by there being basically no veterans

#suicide risk 
bivariate(RiskClientScores, "SuicideRiskLevel", "Success")
#sig. relationship (X2 = 6.2786, p = 0.04331)
freq(RiskClientScores$SuicideRiskLevel)
#likely influenced by there being basically no one with suicide risk

#homicide risk  
bivariate(RiskClientScores, "HomocideRiskLevel", "Success")
#no sig. relationship (X2 = 4.5689, p = 0.1018)

#suicide binary 
bivariate(RiskClientScores, "Suicide", "Success")
#sig. relationship (X2 = 5.5788, p = 0.01818)

#homicide binary 
bivariate(RiskClientScores, "Homicide", "Success")
#no sig. 

###### quantitative explanatories ########

#length of stay and outcome 
logreg <- glm(Success ~ LengthOfStay, data = RiskClientScores, family = "binomial") 
summary(logreg)  
exp(logreg$coefficients)  
#sig. relationship between length of stay and success (z=3.459, p < 0.001)

#age and outcome
logreg2 <- glm(Success ~ AgeAtAdmission, data = RiskClientScores, family = "binomial") 
summary(logreg2)  
exp(logreg2$coefficients)  
#no sig. relationship between age and success (z = -0.493, p = 0.6219)

#looking at ctp vars 
log3 <- glm(Success ~ InabilityToCope + EmotionallyDisengaged+RecklessImpulsivity+
              PoorJudgement + outsourcingResponsibility + Justifying + Grandiosity + DisregardForOthers, data = RiskClientScores, 
            family = "binomial")
summary(log3)
#only grandiosity is sig. 

sum(is.na(RiskClientScores$DischargeStatus))

###########basic logistic regression with all sig. vars ###################
logregfull <- glm(Success ~ LengthOfStay + Form + RiskLevel + ProgramName, data = RiskClientScores, family = "binomial" )
summary(logregfull)  
exp(logregfull$coefficients) 
#length of stay remains sig. when controlling for form, risk, and program (z = 5.315, p < 0.001)
#risk level remains sig. when controlling for form, program, and length of stay
#risk level of low is sig when compared to high (z = 3.998, p < 0.001)
#risk level of moderate is sig when compared to high (z=3.113, p = 0.001852)
#program name remains sig. when controlling for form, length of stay, and risk 
#REACH is sig. when compared to Eddy Center (z = -6.745, p < 0.001)
#Sherman is sig. when compared to Eddy Center (z = -2.434, p = 0.014927)
#SIERRA is sig. when compared to Eddy Center (z = -3.872, p = 0.000108)
#form is not sig. 

logregctp <- glm(Success ~ LengthOfStay + Form + RiskLevel + ProgramName + EmotionallyDisengaged+RecklessImpulsivity+
                   PoorJudgement + outsourcingResponsibility + Justifying + Grandiosity + DisregardForOthers, 
                 data = RiskClientScores, family = "binomial" )
summary(logregctp)  
exp(logregctp$coefficients) 
#still only grandiosity is relevant

#statistical writing: 
# As length of stay increases by 1 day, participants' chances of successful outcomes increase by a factor of 1.0032
# when controlling for form type, program name, and risk level (z = 5.315, p < 0.001).

# Participants with a low risk level's chances of successful outcomes increase by a factor of 2.05, compared to 
# participants with a high risk level, when controlling for form type, program name, and length of stay (z = 3.998, p < 0.001).

# Participants with a moderate risk level's chances of successful outcomes increase by a factor of 1.729 compared to 
# participants with a high risk level, when controlling for form type, program name, and length of stay (z=3.113, p = 0.001852).

# Participants in the REACH program's chances of successful outcomes decrease by 68.60% when compared to 
# participants in the Eddy Center, when controlling for form type, risk level, and length of stay (z = -6.745, p < 0.001).

# Participants in the Roger Sherman House's chances of successful outcomes decrease by 40.6% when compared to 
# participants in the Eddy Center, when controlling for form type, risk level, and length of stay (z = -2.434, p = 0.014927).

# Participants in the SIERRA Center's chances of successful outcomes decrease by 50.86% when compared to 
# participants in the Eddy Center, when controlling for form type, risk level, and length of stay (z = -3.872, p = 0.000108).
