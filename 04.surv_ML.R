#survival analysis and machine learning models 

library(tidyverse)
library(randomForest)
library(caret)
library(qacBase)
library(survival)
library(survminer)
load("cleaned.Rdata")
load("duplicates.Rdata")

#---------------------------------------------------------
# survival analysis
#---------------------------------------------------------

#maybe look into housing at release and length of stay 
#ppl might be being held until they find somewhere to live? 
surv <- survfit(Surv(LengthOfStay, Success)~RiskLevel+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv)$plot + geom_vline(xintercept=120)

#testing cox ph assumptions
ggsurvplot(surv, fun = "cloglog")

#adding more variables
surv.program <-survfit(Surv(LengthOfStay, Success)~ProgramName+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv.program)$plot + geom_vline(xintercept=120)+theme(legend.position = "right")+guides(color = guide_legend(nrow=5))+labs(caption = "All programs, except program 2, have a contracted length of 120 days.")+theme(plot.caption = element_text(size=8))

#graphing code for poster for wesfest
surv.programanon <-survfit(Surv(LengthOfStay, Success)~Program+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv.programanon)$plot + 
  geom_vline(xintercept=120)+theme(legend.position = "right")+
  guides(color = guide_legend(nrow=5))+
  labs(caption = "All programs, except program 5, have a contracted length of 120 days.")+
  theme(plot.caption = element_text(size=8))+
  ggtitle("Time Until Successful Outcome, Stratified by Program")+
  scale_color_brewer(palette="Set1")

survdiff(Surv(LengthOfStay, Success)~Program + cluster(StudyClientId), data = RiskClientScores, rho = 0)

#regular graph code 
surv.program <-survfit(Surv(LengthOfStay, Success)~ProgramName+cluster(StudyClientId), data = RiskClientScores)
ggsurvplot(surv.program)$plot + 
  geom_vline(xintercept=120)+theme(legend.position = "right")+
  guides(color = guide_legend(nrow=5))+
  labs(caption = "All programs, except The January Center, have a contracted length of 120 days.")+
  theme(plot.caption = element_text(size=8))+
  ggtitle("Time Until Successful Outcome, Stratified by Program")+
  scale_color_brewer(palette="Set1")

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
                           RiskClientScores$ProgramName == "REACH (ReEntry Assisted Community Housing)",]
subset2 <- RiskClientScores[RiskClientScores$ProgramName == "The January Center"|
                            RiskClientScores$ProgramName == "Eddy Center",]

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

#---------------------------------------------------------
# random forest
#---------------------------------------------------------
#this is better w/CTP and LSI type vars, things with less missing data 
df_plot(RiskClientScores)

#taking subset just with variables of interest for this 
sub<- RiskClientScores %>% 
  select(RiskLevel, Program, AgeAtAdmission, Success, 
         38:45, Suicide, Race) %>% 
  na.omit()
#kept race as it is the demographic variable with the least amount of missing data 
#other demographic variables had too much missing data to be included in this 

#setting up success and making all variables into factors
sub$Success <- factor(sub$Success, levels = c(0, 1), labels = c("Failure", "Success"))
sub <- sub %>%
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(Program = factor(Program))

#cross-validation function for all machine learning models
control <- trainControl(method="cv", 
                        number=10,
                        summary=twoClassSummary,
                        classProbs=TRUE)

#running random forest 
set.seed(1234)
fit.forest <- train(Success ~ ., 
                    data=sub, 
                    method="rf",
                    metric="ROC",
                    ntree=500,
                    trControl=control,
                    tuneLength=5)
fit.forest

#making a nicer graph for variable importance
imp <- varImp(fit.forest)
df <- imp$importance
df$name <- rownames(df)

#creating categories for the variables 
df <- df %>% 
  mutate(type = case_when(
    name %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    name %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(name, "Program") ~ "Program", 
    TRUE ~ "Criminal Thinking Profile"
  ))

#graphing the variable importance
df %>% 
  filter(Overall > 0) %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, Overall), y=Overall, fill=type))+
  coord_flip()+
  labs(x="", title = "Relative Importance of Variables in Random Forest Model", 
       y = "Importance", fill = "Variable Type", 
       caption = "")+
  theme_minimal()+
  scale_fill_brewer(palette ="Set2", direction = -1)

#finding interesting things when you do smaller scale analysis, 
#things seem important, but then adding everything else in diminishes 
#their impact - need to do more exploratory methods

#---------------------------------------------------------
# lasso
#---------------------------------------------------------
#setting up parameters
lambda <- 10^seq(-3, 3, length=100)

#building model
set.seed(1234)
model.lasso <- train(
  Success ~., data = sub, 
  method = "glmnet",
  metric = "ROC",
  trControl = control,
  tuneGrid = data.frame(alpha = 1, lambda = lambda), 
  family = "binomial"
)

model.lasso$finalModel

#looking at the variables that lasso identified
x <- coef(model.lasso$finalModel, model.lasso$bestTune$lambda)
x <- as.matrix(x)
x[x!=0,1]
#exponentiated values 
exp(x[x!=0,1])
#included the variables risk level, program name, grandiosity, suicide risk, 
#race (but only white when compared to black) - these are considered sig. in this model
#it removed all other CTP vars, other race, and age

#getting variable importance
imp <- varImp(model.lasso)
df <- imp$importance
df$name <- rownames(df)

#creating categories for the variables 
df <- df %>% 
  mutate(type = case_when(
    name %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    name %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(name, "Program") ~ "Program",
    name == "SuicideRisk" ~ "Suicide Risk", 
    TRUE ~ "Criminal Thinking Profile"
  ))

#graphing the variable importance
df %>% 
  filter(Overall > 0) %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, Overall), y=Overall, fill=type))+
  coord_flip()+
  labs(x="", title = "Relative Importance of Variables in Lasso Regression Model", 
       y = "Importance", fill = "Variable Type", 
       caption = "")+
  theme_minimal()+
  scale_fill_brewer(palette ="Set1", direction = -1)

#---------------------------------------------------------
# stepwise
#---------------------------------------------------------

#running multidirectional stepwise model 
model.stepAIC <- train(Success ~ ., data=sub,
                       method="glmStepAIC",
                       direction = "both",
                       trControl = control, 
                       family = "binomial", 
                       metric="ROC")
summary(model.stepAIC$finalModel)
#sig. variables here are risk, programs, and justifying 

#number of vars 
length(model.stepAIC$finalModel$coefficients)-1
#9 total variables in the model 

imp <- varImp(model.stepAIC$finalModel)

#making a nicer graph for variable importance
imp$name <- rownames(imp)

#creating categories for the variables 
imp <- imp %>% 
  mutate(type = case_when(
    name %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    name %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(name, "Program") ~ "Program", 
    TRUE ~ "Criminal Thinking Profile"
  ))

#graphing the variable importance
ggplot(imp)+
  geom_col(aes(x=reorder(name, Overall), y=Overall, fill=type))+
  coord_flip()+
  labs(x="", title = "Relative Importance of Variables in Stepwise AIC Model", 
       y = "Importance", fill = "Variable Type", 
       caption = "")+
  theme_minimal()+
  scale_fill_brewer(palette ="Set2", direction = -1)

#---------------------------------------------------------
# gradient boosting
#---------------------------------------------------------

#gradient boosting model 
set.seed(1234)
model.gbm <- train(Success~., 
                   data = sub, 
                   method = "gbm", 
                   tuneLength=3,
                   trControl = control,
                   metric="ROC")
model.gbm

#relative importance of all the variables
imp <- summary(model.gbm$finalModel)

#creating categories for the variables 
imp <- imp %>% 
  mutate(type = case_when(
    var %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    var %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(var, "Program") ~ "Program", 
    var=="SuicideRisk" ~ "Suicide Risk", 
    TRUE ~ "Criminal Thinking Profile"
  ))

#graphing the variable importance
imp %>% 
  filter(rel.inf > 0) %>% 
  ggplot()+
  geom_col(aes(x=reorder(var, rel.inf), y=rel.inf, fill=type))+
  coord_flip()+
  labs(x="", title = "Relative Importance of Variables in Gradient Boosted Model", 
       y = "Importance", fill = "Variable Type", 
       caption = "")+
  theme_minimal()+
  scale_fill_brewer(palette ="Set2", direction = -1)

#---------------------------------------------------------
# takeaways from ML models
#---------------------------------------------------------

#Random forest identified the following variables as important: 
#-age, race (other and white), all programs, all CTP variables, and risk levels 

#lasso identified the following variables as important: 
#-all programs, risk levels, suicide risk, grandiosity 

#bidirectional stepwise identified the following variables as important: 
#-justifying, grandiosity, poor judgement, all programs, and risk levels 

#gradient boosting identified the following variables as important: 
#all, but especially REACH, age, grandiosity, January Center, Risk high and inability to cope

#---------------------------------------------------------
# logistic regression w/train method w/all vars in sub
#---------------------------------------------------------

#logistic regression 
set.seed(1234)
model.glm <- train(Success~., 
                   data = sub, 
                   method = "glm", 
                   tuneLength=3,
                   trControl = control,
                   metric="ROC")
model.glm

#looking closer at this model 
model.glm$finalModel

#looking for significance 
summary(model.glm$finalModel)
#sig. variables are risk level, all programs, justifying

#taking the exponent b/c these are logged
exp(model.glm$finalModel$coefficients)

#getting relative importance of the variables 
imp <- varImp(model.glm)

#turning it into a dataframe 
df <- imp$importance
df$name <- rownames(df)

#creating categories for the variables 
df <- df %>% 
  mutate(type = case_when(
    name %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    name %in% c("AgeAtAdmission", "`RaceCaucasian or White`", 
                "RaceOther") ~ "Demographic",
    str_detect(name, "Program") ~ "Program", 
    name=="SuicideRisk" ~ "Suicide Risk", 
    TRUE ~ "Criminal Thinking Profile"
  ))

#graphing the variable importance
df %>% 
  filter(Overall > 0) %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, Overall), y=Overall, fill=type))+
  coord_flip()+
  labs(x="", title = "Relative Importance of Variables in Logistic Regression Model", 
       y = "Importance", fill = "Variable Type", 
       caption = "")+
  theme_minimal()+
  scale_fill_brewer(palette ="Set1", direction = -1)

#most important variables are program, risk level, 
#then jusitfying, grandiosity, suicide risk, race white

#overall, getting a sense that the CTP variables actually aren't that good 
#as predictors, and generally aren't significant 
#---------------------------------------------------------
# plotting the CV to find the best model 
#---------------------------------------------------------
## compare models
results <- resamples(list(forest = fit.forest,
                          lasso = model.lasso, 
                          stepwise = model.stepAIC,
                          gbm = model.gbm, 
                          glm = model.glm))

#numeric comparison between all the models 
summary(results)

#visual comparison between all the models
bwplot(results,
       scales =list(x=list(relation = "free")))
dotplot(results,
        scales =list(x=list(relation = "free")))

#need to decide which is best based on all the numbers 
#lasso is only slightly better than logistic regression in terms of ROC 
#glm a bit better on sens/spec than lasso 

#overall, in every model, participants in REACH are deemed an important predictor 
#being in REACH makes you less likely to succeed in the program, no matter what 
#this is especially true for participants who are deemed high risk 
#these participants need more support 

#from here, can just drill down into REACH and other work release programs
#could even combine REACH w/other programs if they're similar 
#other programs are roger sherman and sierra center - can run same models on subset w/just these three programs
# need to figure out why they're struggling so much 
#make a visualization that shows the magnitude of each coefficient instead of just 
#variable importance (i.e. which ones are positive and which ones are negative)

