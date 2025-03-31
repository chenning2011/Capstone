#survival analysis and random forest

library(tidyverse)
load("cleaned.Rdata")
load("duplicates.Rdata")

#survival analysis - preliminary 

#maybe look into housing at release and length of stay 
#ppl might be being held until they find somewhere to live? 
library(survival)
library(survminer)
surv <- survfit(Surv(LengthOfStay, Success)~RiskLevel+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv)$plot + geom_vline(xintercept=120)

#testing cox ph assumptions
ggsurvplot(surv, fun = "cloglog")

#adding more variables
surv.program <-survfit(Surv(LengthOfStay, Success)~ProgramName+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv.program)$plot + geom_vline(xintercept=120)+theme(legend.position = "right")+guides(color = guide_legend(nrow=5))

#adding all variables
surv.cox <- coxph(Surv(LengthOfStay, Success)~RiskLevel+ProgramName+Form+Suicide+cluster(StudyClientId), data = RiskClientScores)
summary(surv.cox)

surv.program <-survfit(Surv(LengthOfStay, Success)~ProgramName+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv.program, fun = "cloglog")
#stratify by program or type of program, this isn't very good 

surv.form <- survfit(Surv(LengthOfStay, Success)~Form, data = RiskClientScores)
#testing assumptions
ggsurvplot(surv.form, fun = "cloglog")
cox.zph(surv.cox)

#stratifying based on type of program 
s <- RiskClientScores[RiskClientScores$ProgramName == "Roger Sherman House"|
                           RiskClientScores$ProgramName == "SIERRA Center - Work Release"|
                           RiskClientScores$ProgramName == "Eddy Center",]
subset2 <- RiskClientScores[RiskClientScores$ProgramName == "The January Center"|
                            RiskClientScores$ProgramName == "REACH (ReEntry Assisted Community Housing)",]

#less intense programs only 
surv.subset <- coxph(Surv(LengthOfStay, Success)~RiskLevel, data = s)
summary(surv.subset)
#risk level high is significantly associated with lower hazards when compared to ppl with low risk levels
#therefore, ppl with high risk level are less likely to successfully complete the program 

#visualization
surv.subsetfit <- survfit(Surv(LengthOfStay, Success)~RiskLevel, data = s)
ggsurvplot(surv.subsetfit)$plot + geom_vline(xintercept = 120)

#testing assumptions 
ggsurvplot(surv.subsetfit, fun = "cloglog")
cox.zph(surv.subset)

#more intense programs only 
surv.subset2 <- coxph(Surv(LengthOfStay, Success)~RiskLevel, data = subset2)
summary(surv.subset2)
#no difference between risk levels in the more intense programs

#visualizations
surv.subset2fit <- survfit(Surv(LengthOfStay, Success)~RiskLevel, data = subset2)
ggsurvplot(surv.subset2fit)$plot + geom_vline(xintercept=120)

#testing assumtions
ggsurvplot(surv.subset2fit, fun = "cloglog")
cox.zph(surv.subset2)

########## random forest ########
#this is better w/CTP and LSI type vars, things with less missing data 
library(randomForest)
library(caret)

library(qacBase)
df_plot(RiskClientScores)

#taking subset just with variables of interest for this 
subset <- RiskClientScores %>% 
  select(RiskLevel, ProgramName, AgeAtAdmission, Success, 
         38:45, Suicide) %>% 
  na.omit()

subset$Success <- factor(subset$Success, levels = c(0, 1), labels = c("Failure", "Success"))
subset <- subset %>%
  mutate(across(where(is.character), as.factor))

control <- trainControl(method="cv", 
                        number=10,
                        summary=twoClassSummary,
                        classProbs=TRUE)
set.seed(1234)
fit.forest <- train(Success ~ ., 
                    data=subset, 
                    method="rf",
                    metric="ROC",
                    ntree=1000,
                    trControl=control,
                    tuneLength=3)
fit.forest
imp <- varImp(fit.forest)
plot(imp)
#finding interesting things when you do smaller scale analysis, things seem important, but then adding everything else in diminishes their impact - need to do more exploratory methods


