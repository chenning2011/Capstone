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

ggsurvplot(surv, fun = "cloglog")

surv.program <-survfit(Surv(LengthOfStay, Success)~ProgramName+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv.program)$plot + geom_vline(xintercept=120)

surv.cox <- coxph(Surv(LengthOfStay, Success)~RiskLevel+ProgramName+Form+Suicide+cluster(StudyClientId), data = RiskClientScores)
summary(surv.cox)

surv.program <-survfit(Surv(LengthOfStay, Success)~ProgramName+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv.program, fun = "cloglog")
#stratify by program or type of program

surv.form <- survfit(Surv(LengthOfStay, Success)~Form, data = RiskClientScores)
ggsurvplot(surv.form, fun = "cloglog")

cox.zph(surv.cox)

#need to stratify, these are all bad 

########## random forest ########
#this is better w/CTP and LSI type vars, things with less missing data 
library(randomForest)
library(caret)

#taking subset just with variables of interest for this 
subset <- RiskClientScores %>% 
  select(Race, IdentifiesAsLGBTQ, MaritalStatus, Religion, English, UsVeteran, Hispanic, 
         RiskLevel, Form, ProgramName, AgeAtAdmission, LengthOfStay, LivingArrangementAtAdmission,
         Suicide, Success, 38:45) %>% 
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

#next steps: reducing dimensions for CTP variables, if possible 
#try some clustering with those variables? 
#look into asus scale and run it in the random forest with the CTP vars, ones with less missing data 
#run correlation matrix to figure out which ones overlap
#run correlation matrix on CTP vars
#do a new random forest 
#look at length of stay by some of these newer variables, re-run survival 