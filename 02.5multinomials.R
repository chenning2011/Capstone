#---------------------------------------------------------
# multinomial regressions
#---------------------------------------------------------

#libraries and datasets
library(nnet)
library(tidyverse)
load("cleaned.Rdata")
load("duplicates.Rdata")

#multinomial function 
multinomial <- function(data, explanatory, response, levels=NULL){
  #running multinomial regression
  mod1 <- multinom(as.formula(paste(response, "~", explanatory)), data = data)
  
  #creating z and p values 
  z <- summary(mod1)$coefficients/summary(mod1)$standard.errors 
  p <- (1- pnorm(abs(z), 0, 1)) * 2 
  
  #printing output from multinomial regressions 
  print(paste("Multinomial regression for", explanatory, "and", response))
  print("Exponated Coefficients")
  print(exp(summary(mod1)$coefficients))
  print("p-values")
  print(p)
}


#making outcome variable into a factor 
RiskClientScores$Outcome <- factor(RiskClientScores$Outcome)
RiskClientScores$Outcome <- relevel(RiskClientScores$Outcome, ref = "Successful")

#risk level and outcome 
multinomial(RiskClientScores, "RiskLevel", "Outcome")

#people with a high risk level are 2.29 times more likely to have an unsuccessful outcome than a successful outcome (p<0.001) 
#people with a high risk level are 50% less likely to have a neutral outcome than a successful outcome (p<0.001)

#program and outcome 
multinomial(RiskClientScores, "ProgramName", "Outcome")

#people in REACH are 4.5 times as likely to have neutral outcomes compared to successful outcomes, when compared to ppl at the Eddy Center (p<0.001)
#ppl in REACH are 3.2 times as likely to have an unsuccessful outcome compared to a successful outcome, when compared to ppl at the Eddy Center (p<0.001)
#ppl in Roger Sherman are 1.75 times as likely to have an unsuccessful outcome compared to a successful outcome, when compared to ppl at the Eddy Center (p=0.008)
#ppl in the January Center are 16% less likely to have an unsuccessful outcome commpared to a successful outcome, when compared to ppl in the Eddy Center (p<0.001)

#race and outcome
multinomial(RiskClientScores, "Race", "Outcome")
#no signifcance 

#form and outcome 
multinomial(RiskClientScores, "Form", "Outcome")
#nothing here either 

#hispanic 
multinomial(RiskClientScores, "Hispanic", "Outcome")
#nothing here 

#english 
multinomial(RiskClientScores, "English", "Outcome")
#nothing here 

#religion 
multinomial(RiskClientScores, "Religion", "Outcome")
#islamic ppl are less likely to have neutral outcomes (vs. successful), when compared to atheists 
#nothing else significant, nothing really important here 

#marital status
multinomial(RiskClientScores, "MaritalStatus", "Outcome")
#nothing 

#lgbtq 
multinomial(RiskClientScores, "IdentifiesAsLGBTQ", "Outcome")
#nothing 

#suicide risk level 
multinomial(RiskClientScores, "Suicide", "Outcome")
#those with suicide risk are 30% less likely to have unsuccessful outcomes than those without (compared to successful outcomes) (p=0.007)

#homicide risk level 
multinomial(RiskClientScores, "Homicide", "Outcome")
#nothing here 

#next steps with this would be to combine these with some of the quantitative variables that predict outcomes 
#not sure which variables are most interesting quantitatively 
#this probably goes back to needing to reduce dimensions with all of the CTP and ASUS variables so i should do that but i dont wanna 

#focus on machine learning to identify the factors that have most influence 
#-lasso, gradient boosting, etc. just try out different methods on the data 

#full regression with age, CTP vars, program name, age, race, and risk level 
mod1 <- multinom(Outcome ~ AgeAtAdmission + Grandiosity + DisregardForOthers + EmotionallyDisengaged+
                   InabilityToCope + RecklessImpulsivity + PoorJudgement + Justifying + outsourcingResponsibility +
                   RiskLevel + ProgramName + Suicide + Race, data = RiskClientScores)
summary(mod1)
exp(summary(mod1)$coefficients)
z <- summary(mod1)$coefficients/summary(mod1)$standard.errors 
p <- (1- pnorm(abs(z), 0, 1)) * 2 
p
