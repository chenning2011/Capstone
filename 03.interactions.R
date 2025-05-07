##testing interactions between variables 

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

###### sig. results ######
#interaction between risk level and marital status (b/c of sample size)
#interaction between risk level and religion (b/c of sample size)
#interaction between suicide and risk level 
#tried a (bad) random forest and found that the most important variables were length of stay, age, programname, religion, risk level, race
  #so need to move forward with survival analysis
#when adding in all the ctp data, all of those variables are important (after age and lengthofstay)

##### risk level interactions #####

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

#risk level and religion 
interaction(RiskClientScores, "Success", "RiskLevel", "Religion")
#risk level is no longer significant, but religion is significant (buddhist vs none)
#interaction between moderate and buddhist and moderate and jewish - likely because of small sample sizes in both religions 

#risk level and primary langauge 
interaction(RiskClientScores, "Success", "RiskLevel", "PrimaryLanguage")
#sig. in language (english vs vietnamese) and in risk level
#no interaction 

#risk level and english 
interaction(RiskClientScores, "Success", "RiskLevel", "English")
#no significance anywhere 

#risk level and lgbtq 
interaction(RiskClientScores, "Success", "RiskLevel", "IdentifiesAsLGBTQ")
#no significance anywhere 

#risk level and veteran status 
interaction(RiskClientScores, "Success", "RiskLevel", "UsVeteran")
#there are barely any veterans, so this doesn't really work 
#sig. in risk level high vs low, but not in veteran status 

#risk level and suicide risk level 
interaction(RiskClientScores, "Success", "RiskLevel", "SuicideRiskLevel")
#sig. between high and low risk level, interaction between both kinds of moderate risk 

#risk level and homicide risk level 
interaction(RiskClientScores, "Success", "RiskLevel", "HomocideRiskLevel")
#sig. between high and low risk, sig. between moderate vs np risk for homicide, interaction between moderate risk and high homicide risk 

#risk level and suicide binary
interaction(RiskClientScores, "Success", "RiskLevel", "Suicide")
#no sig. relationship here (just in risk level)
#visually seems like there is something going on, but no statistical evidence of this

#risk level and homicide binary 
interaction(RiskClientScores, "Success", "RiskLevel", "Homicide")
#sig. moderation (risk level * homicide)

###### program name interactions ######
#program and race 
interaction(RiskClientScores, "Success", "ProgramName", "Race")
#sig. in program name (REACH, Sherman, January) and race (Other)
#interaction between other race and REACH (nowhere else though)

#program and hispanic 
interaction(RiskClientScores, "Success", "ProgramName", "Hispanic")
#program name remains sig., nothing with hispanic status 
#visually seems like there's something weird with the SIERRA center, could be a sample size problem? 

#program and martial status 
interaction(RiskClientScores, "Success", "ProgramName", "MaritalStatus")
#something weird going on here 
#likely sample size problems again (not enough married people/other people in some of the programs)

#program and religion 
interaction(RiskClientScores, "Success", "ProgramName", "Religion")
#similar problem with marital status

#program and primary langauge 
interaction(RiskClientScores, "Success", "ProgramName", "PrimaryLanguage")
#similar problem with marital status and religion

#program and english 
interaction(RiskClientScores, "Success", "ProgramName", "English")
#january center only sig. program, and only one wiith interaction with english (english speakers are less successful there than normal)
#weird though because january center has 100% success rate for non-english spakears, and almost 100% for english speakers. 

#program and lgbtq 
interaction(RiskClientScores, "Success", "ProgramName", "IdentifiesAsLGBTQ")
#interaction, but LGBTQ+ ppl only exist in three of the centers, so not sure we can use this 

#program and veteran status 
interaction(RiskClientScores, "Success", "ProgramName", "UsVeteran")
#there are barely any veterans, so this doesn't really work 

#program and suicide risk level 
interaction(RiskClientScores, "Success", "ProgramName", "SuicideRiskLevel")
#lots going on here

#program and homicide risk level 
interaction(RiskClientScores, "Success", "ProgramName", "HomocideRiskLevel")
#sig. between high and low risk, sig. between moderate vs np risk for homicide, interaction between moderate risk and high homicide risk 

#program and suicide binary
interaction(RiskClientScores, "Success", "ProgramName", "Suicide")
#no one w/suicide risk in REACH

#program and homicide binary 
interaction(RiskClientScores, "Success", "ProgramName", "Homicide")
#no one w homicide risk in two programs

#program and risk level 
interaction(RiskClientScores, "Success", "ProgramName", "RiskLevel")
#no one w homicide risk in two programs

#for the poster - remove all program names 

#Graph for poster
ggplot(data = RiskClientScores)+
  stat_summary(aes(x=ProgramName, y=Success, fill=RiskLevel), fun = "mean", geom="bar", position = "dodge")+
  theme_minimal()+
  labs(title = paste("Proportion of Successful Outcomes by Program and Risk Level"), x = "Program", y = "Proportion of Successful Outcomes", fill = "Risk Level")+
  scale_fill_brewer(palette = "Set1", direction = -1)


#basic logistic regression with just success and risk level 
logreg <- glm(Success ~ RiskLevel + ProgramName + Race + AgeAtAdmission +
                EmotionallyDisengaged+RecklessImpulsivity+
                PoorJudgement + outsourcingResponsibility + Suicide + Homicide +
                Justifying + Grandiosity +DisregardForOthers, data = RiskClientScores, family = "binomial")
clustered <- vcovCL(logreg, cluster = ~ RiskClientScores$StudyClientId, type = "HC0")

#getting coefficients with clustered standard errors 
logreg_clustered <- coeftest(logreg, vcov = clustered)
logreg_clustered
#just for coefficients
exp(logreg_clustered)

#creating a dataframe so i can plot these results 
data <- data.frame(logreg_clustered[,1])
data$var <- rownames(data)
data %>% 
  filter(var !="(Intercept)") %>% 
  rename(coef = logreg_clustered...1.) %>% 
  mutate(color = ifelse(coef < 0, "negative", "positive")) -> data

data %>% 
  mutate(OR = exp(coef), 
         pvalue = logreg_clustered[2:19,4], 
         sig = ifelse(pvalue < 0.05, "sig", "not sig")) -> data

#plot of logistic regerssion coefficients w/clustered standard errors and all vars
data %>% 
  filter(!(var %in% c("Grandiosity", "outsourcingResponsibility"))) %>% 
  ggplot()+
  geom_col(aes(x=reorder(var, coef), y=coef, fill = color))+
  coord_flip()+
  theme_minimal()+
  geom_text(data = data[data$sig=="sig",], 
            aes(x=reorder(var, coef), y=coef, label = paste("OR:", round(OR,2))),
            fontface="bold",
            nudge_y = ifelse(data[data$sig=="sig",]$coef > 0, 0.2, -0.2))+
  theme(legend.position = "none")+
  labs(x="Variable", y = "Coefficient (Logit)", caption = "Grandiosity and Outsourcing Responsibility were removed for readability.\nOdds ratios are displayed for statistically significant variables", title = "Logit by Variable from Logistic Regression with Clustered SE")+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5))

#risk level high is 47.63% less likely to succeed than risk level low, regardless of other factors (z=-3.75, p=0.0001769)
#risk level moderate is 31.01% less likely to succeed than risk level low, regardless of other factors (z=-2.96, p = 0.0031)

sub<- RiskClientScores %>% 
  filter(Program %in% c(2,3,4)) %>% 
  dplyr::select(RiskLevel, AgeAtAdmission, Success, 38:45, Suicide, Race, Homicide, StudyClientId) 

#basic logistic regression with just success and risk level, subsetting for just programs 2,3,4 (subset taken from 4.5ML code, so maybe reorganize all of these files later)
logreg <- glm(Success ~ RiskLevel + Race + AgeAtAdmission +
                EmotionallyDisengaged+RecklessImpulsivity+
                PoorJudgement + outsourcingResponsibility + Suicide + Homicide +
                Justifying + Grandiosity +DisregardForOthers, data = sub, family = "binomial")
clustered <- vcovCL(logreg, cluster = ~ sub$StudyClientId, type = "HC0")

#getting coefficients with clustered standard errors 
logreg_clustered <- coeftest(logreg, vcov = clustered)
logreg_clustered
#just for coefficients
exp(logreg_clustered)

#creating a dataframe so i can plot these results 
data <- data.frame(logreg_clustered[,1])
data$var <- rownames(data)
data %>% 
  filter(var !="(Intercept)") %>% 
  rename(coef = logreg_clustered...1.) %>% 
  mutate(color = ifelse(coef < 0, "negative", "positive")) -> data

data %>% 
  mutate(OR = exp(coef), 
         pvalue = logreg_clustered[2:15,4], 
         sig = ifelse(pvalue < 0.05, "sig", "not sig")) -> data

#plot of logistic regerssion coefficients w/clustered standard errors and all vars
data %>% 
  filter(!(var %in% c("Grandiosity", "outsourcingResponsibility"))) %>% 
  ggplot()+
  geom_col(aes(x=reorder(var, coef), y=coef, fill = color))+
  coord_flip()+
  theme_minimal()+
  geom_text(data = data[data$sig=="sig",], 
            aes(x=reorder(var, coef), y=coef, label = paste("OR:", round(OR,2))),
            fontface="bold",
            nudge_y = ifelse(data[data$sig=="sig",]$coef > 0, 0.07, -0.07))+
  theme(legend.position = "none")+
  labs(x="Variable", y = "Coefficient (Logit)", caption = "Odds ratios are displayed for statistically significant variables", title = "Logit by Variable from Logistic Regression with Clustered SE \nfor Low-Supervision Programs")+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75))

#risk level high is 47.63% less likely to succeed than risk level low, regardless of other factors (z=-3.75, p=0.0001769)
#risk level moderate is 31.01% less likely to succeed than risk level low, regardless of other factors (z=-2.96, p = 0.0031)

sub<- RiskClientScores %>% 
  filter(Program %in% c(1,5)) %>% 
  dplyr::select(RiskLevel, AgeAtAdmission, Success, 38:45, Suicide, Race, Homicide, StudyClientId) 

#basic logistic regression with just success and risk level, subsetting for just programs 2,3,4 (subset taken from 4.5ML code, so maybe reorganize all of these files later)
logreg <- glm(Success ~ RiskLevel + Race + AgeAtAdmission +
                EmotionallyDisengaged+RecklessImpulsivity+
                PoorJudgement + outsourcingResponsibility + Suicide +
                Justifying + Grandiosity +DisregardForOthers, data = sub, family = "binomial")
clustered <- vcovCL(logreg, cluster = ~ sub$StudyClientId, type = "HC0")

#getting coefficients with clustered standard errors 
logreg_clustered <- coeftest(logreg, vcov = clustered)
logreg_clustered
#just for coefficients
exp(logreg_clustered)

data <- data.frame(logreg_clustered[,1])
data$var <- rownames(data)
data %>% 
  filter(var !="(Intercept)") %>% 
  rename(coef = logreg_clustered...1.) %>% 
  mutate(color = ifelse(coef < 0, "negative", "positive")) -> data

data %>% 
  mutate(OR = exp(coef), 
         pvalue = logreg_clustered[2:14,4], 
         sig = ifelse(pvalue < 0.05, "sig", "not sig")) -> data

#plot of logistic regerssion coefficients w/clustered standard errors and all vars
ggplot(data)+
  geom_col(aes(x=reorder(var, coef), y=coef, fill = color))+
  coord_flip()+
  theme_minimal()+
  geom_text(data = data[data$sig=="sig",], 
            aes(x=reorder(var, coef), y=coef, label = paste("OR:", round(OR,2))),
            fontface="bold",
            nudge_y = ifelse(data[data$sig=="sig",]$coef > 0, 0.05, -0.09))+
  theme(legend.position = "none")+
  labs(x="Variable", y = "Coefficient (Logit)", caption = "Odds ratios are displayed for statistically significant variables", title = "Logit by Variable from Logistic Regression with Clustered SE \nfor High-Supervision Programs")+
  scale_fill_brewer(palette = "Set1")
