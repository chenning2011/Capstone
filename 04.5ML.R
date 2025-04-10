#---------------------------------------------------------
# splitting machine learning models up by just work release programs
#---------------------------------------------------------

library(tidyverse)
library(randomForest)
library(caret)
library(qacBase)
library(survival)
library(survminer)
load("cleaned.Rdata")
load("duplicates.Rdata")

#taking subset just with variables of interest for this 
sub<- RiskClientScores %>% 
  filter(Program %in% c(2,3,4)) %>% 
  dplyr::select(RiskLevel, AgeAtAdmission, Success, 38:45, Suicide, Race) %>% 
  na.omit()
#kept race as it is the demographic variable with the least amount of missing data 
#other demographic variables had too much missing data to be included in this 

#setting up success and making all variables into factors
sub$Success <- factor(sub$Success, levels = c(0, 1), labels = c("Unsuccessful", "Successful"))
sub <- sub %>%
  mutate(across(where(is.character), as.factor))

#testing/training sets
set.seed(1234)
index <- createDataPartition(sub$Success, p=.8,
                             list=FALSE)
train <- sub[index,]
test <- sub[-index, ]

#cross-validation function for all machine learning models
control <- trainControl(method="cv", 
                        number=10,
                        summary=twoClassSummary,
                        classProbs=TRUE)

#running random forest 
set.seed(1234)
fit.forest <- train(Success ~ ., 
                    data=train, 
                    method="rf",
                    metric="Accuracy",
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
    str_detect(name, "ProgramName") ~ "Program Name", 
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
  Success ~., data = train, 
  method = "glmnet",
  metric = "Accuracy",
  trControl = control,
  tuneGrid = data.frame(alpha = 1, lambda = lambda), 
  family = "binomial"
)

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
    str_detect(name, "ProgramName") ~ "Program Name",
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
  scale_fill_brewer(palette ="Set2", direction = -1)

#---------------------------------------------------------
# stepwise
#---------------------------------------------------------

#running multidirectional stepwise model 
model.stepAIC <- train(Success ~ ., data=train,
                       method="glmStepAIC",
                       direction = "both",
                       trControl = control, 
                       family = "binomial", 
                       metric="Accuracy")
summary(model.stepAIC$finalModel)
#sig. variables here are risk, programs, and justifying 

#number of vars 
length(model.stepAIC$finalModel$coefficients)-1
#6 total variables in the model 

imp <- varImp(model.stepAIC$finalModel)

#making a nicer graph for variable importance
imp$name <- rownames(imp)

#creating categories for the variables 
imp <- imp %>% 
  mutate(type = case_when(
    name %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    name %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(name, "ProgramName") ~ "Program Name", 
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
                   data = train, 
                   method = "gbm", 
                   tuneLength=3,
                   trControl = control,
                   metric="Accuracy")
model.gbm



#relative importance of all the variables
imp <- summary(model.gbm$finalModel)

#creating categories for the variables 
imp <- imp %>% 
  mutate(type = case_when(
    var %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    var %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(var, "ProgramName") ~ "Program Name", 
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
  scale_fill_brewer(palette = "Dark2", direction = -1)

#---------------------------------------------------------
# logistic regression w/train method w/all vars in sub
#---------------------------------------------------------

#logistic regression 
set.seed(1234)
model.glm <- caret::train(Success~., 
                   data = train, 
                   method = "glm", 
                   tuneLength=3,
                   trControl = control,
                   metric="Accuracy")
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
    name %in% c("AgeAtAdmission", "`RaceCaucasian or White`", "RaceOther") ~ "Demographic",
    str_detect(name, "ProgramName") ~ "Program Name", 
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
  scale_fill_brewer(palette ="Set2", direction = -1)

#most important variables are justifying, risk level, then more ctp vars

#overall, getting a sense that the CTP variables actually aren't that good 
#as predictors, and generally aren't significant 
#---------------------------------------------------------
# plotting the CV to find the best model 
#---------------------------------------------------------
## compare models
results <- resamples(list(random_forest = fit.forest,
                          lasso = model.lasso, 
                          bi_stepwise = model.stepAIC,
                          gradient_boosted = model.gbm, 
                          logistic = model.glm))

#numeric comparison between all the models 
summary(results)

#visual comparison between all the models
bwplot(results,
       scales =list(x=list(relation = "free")))
dotplot(results,
        scales =list(x=list(relation = "free")))

#testing best model on test dataset now 
test$pred <- as.factor(predict(model.gbm, test))
confusionMatrix(test$pred, test$Success, positive = "Successful")

test$pred <- as.factor(predict(model.glm, test))
confusionMatrix(test$pred, test$Success, positive = "Successful")

test$pred <- as.factor(predict(model.stepAIC, test))
confusionMatrix(test$pred, test$Success, positive = "Successful")

test$pred <- as.factor(predict(model.lasso, test))
confusionMatrix(test$pred, test$Success, positive = "Successful")

test$pred <- as.factor(predict(fit.forest, test))
confusionMatrix(test$pred, test$Success, positive = "Successful")

#trying to make my own graph
viz <- results$values

#flipping it 
viz <- pivot_longer(viz, cols = 2:16)

#splitting into two columns 
viz <- separate_wider_delim(data = viz, cols = name, delim = "~", names = c("type", "sens"))

#trying boxplot 
ggplot(viz, aes(x=reorder(type, value)))+
  geom_boxplot(aes(y=value))+
  stat_summary(aes(y=value), fun= "mean", geom = "point", color = "navy")+
  facet_grid(~sens)+
  coord_flip()+
  theme_bw()

#---------------------------------------------------------
# looking just at factors that influence success for REACH specifically
#---------------------------------------------------------

#taking subset just with variables of interest for this 
sub<- RiskClientScores %>% 
  filter(Program==3) %>% 
  select(RiskLevel, AgeAtAdmission, Success, 38:45, Suicide, Race) %>% 
  na.omit()
#kept race as it is the demographic variable with the least amount of missing data 
#other demographic variables had too much missing data to be included in this 

#setting up success and making all variables into factors
sub$Success <- factor(sub$Success, levels = c(0, 1), labels = c("Failure", "Success"))
sub <- sub %>%
  mutate(across(where(is.character), as.factor))

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
    str_detect(name, "ProgramName") ~ "Program Name", 
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
    str_detect(name, "ProgramName") ~ "Program Name",
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
  scale_fill_brewer(palette ="Set2", direction = -1)

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
#6 total variables in the model 

imp <- varImp(model.stepAIC$finalModel)

#making a nicer graph for variable importance
imp$name <- rownames(imp)

#creating categories for the variables 
imp <- imp %>% 
  mutate(type = case_when(
    name %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    name %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(name, "ProgramName") ~ "Program Name", 
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

model.gbm$finalModel$param

#relative importance of all the variables
imp <- summary(model.gbm$finalModel)

#creating categories for the variables 
imp <- imp %>% 
  mutate(type = case_when(
    var %in% c("RiskLevelModerate", "RiskLevelHigh") ~ "Risk Level",
    var %in% c("AgeAtAdmission", "RaceOther", "RaceCaucasian or White") ~ "Demographic",
    str_detect(var, "ProgramName") ~ "Program Name", 
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
  scale_fill_brewer(palette ="Set1", direction = -1)

library(RColorBrewer)

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
    name %in% c("AgeAtAdmission", "`RaceCaucasian or White`", "RaceOther") ~ "Demographic",
    str_detect(name, "ProgramName") ~ "Program Name", 
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
  scale_fill_brewer(palette ="Set2", direction = -1)

#most important variables are justifying, risk level, then more ctp vars

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
#not enough actual observations for this to have made any sense to do, but who cares lol
