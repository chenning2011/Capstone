#---------------------------------------------------------
# importing data files
#---------------------------------------------------------

#libraries
library(tidyverse)
library(readxl)

#remove anything that says interim assessment
#add in ctp 
#work on bivariate descriptives
#talk to val about survival with multiple obs per person (mixed interrupted survival analysis)


#supplement to the re-entry form scores
#applies only to people who have been incarcerated for less than 4 years
ScoresSRTData <- read_excel("ScoresSRTData.xlsx")

#re-entry form scores (scoring them before they re-enter the community)
#condition to fill out this form: within 6 months of discharge/re-entry date and unrestricted by security concerns
#this form is given to people who are in prison
#each one of these forms is made to identify the likelihood of recidivism 
#applies to people who have been in jail for more than 4 years
ScoresRTData <- read_csv("ScoresRTData.csv")

#community supervision tool scores
#condition to fill out this form: charged with criminal offense and referred to probation 
#these people are allowed to remain within the community, but are supervised
#the form's goal is to help decide the level/needs for supervision
ScoresCSTData <- read_excel("ScoresCSTData.xlsx")

#where people are staying, how long they've been there, and past trauma they're willing to disclose
EpisodeTraumaData <- read_excel("EpisodeTraumaData.xlsx")

#suicide and homocide risk levels at admission for patients 
#taken from a form - adapted from columbia suicide severity rating scale
RiskDataRevised <- read_excel("RiskDataRevised.xlsx")

#this is the most important one 
#this contains information on the patients discharge dates, where they went to, where they were before
#also their income levels and residences, if applicable 
EpisodeData <- read_excel("EpisodeData.xlsx")

#this one is interesting, but might not be as useful
#essentially looking at a series of 65 questions, each of which is scored from 0-4 
#each of the scores is tallied and relates to a specific category, which is given an overall score 
#these scores are risk ratings, and have "normal ranges" that people can go outside of 
#would need to categorize these scores 
CtpData <- read_excel("CtpData.xlsx")

#demographic information
ClientData <- read_excel("ClientData.xlsx")

#data on incidents that they could link clients to -> might not be as helpful
#need more information on when these incidents are occuring (is it while they're under probation?)
#for this one, download the new wide version from teams before doing work with 
IncidentData <- read_excel("IncidentData.xlsx")

#other data sets include: 
#information on drugs taken and for how long 
#information on meetings with the people in the program, how many and how long they were 
  #this could be good as a measure of how effective these touchpoints are
#information on employment status 
  #could also be important as a demographic variable
#information on what they were arrested for -> missing data but also interesting 
#LSI -> another measure of risk of recidivism 

#---------------------------------------------------------
# merging client data with the scores data
#---------------------------------------------------------

#add form identifiers to each set of scores 
ScoresCSTData <- ScoresCSTData %>% 
  mutate(Form = "CST")

ScoresRTData <- ScoresRTData %>% 
  mutate(Form = "RT")

ScoresSRTData <- ScoresSRTData %>% 
  mutate(Form = "SRT")

#right join datasets to client data, remove interim assessment, remove unneccessary vars, fix override 
ClientScoresCST <- ClientData %>% 
  right_join(ScoresCSTData, by ="StudyClientId", relationship = "many-to-many") %>% 
  filter(AssessmentType != "Interim Assessment") %>% 
  mutate(RiskLevel = ifelse(ProfessionalOverride == 1, RiskLevelFinalOverride, RiskLevel)) %>% 
  select(-(Q1_1:Q7_Score), -ProfessionalOverride, -RiskLevelFinalOverride, -BiologicalGender)

#joining clientdata w/clientscoresRT
ClientScoresRT <- ClientData %>% 
  right_join(ScoresRTData, by ="StudyClientId", relationship = "many-to-many") %>% 
  filter(AssessmentType != "Interim Assessment") %>% 
  mutate(RiskLevel = ifelse(ProfessionalOverride == 1, RiskLevelFinalOverride, RiskLevel),
         TotalScore = Q1_Score+Q3_Score+Q3_Score1) %>% 
  select(-(Q1_1:Q3_Score1), -(ProfessionalOverride:RiskLevelFinalOverride), -BiologicalGender, -AgeAtAssessment)

#joining clientdata w/clientscoresSRT
ClientScoresSRT <- ClientData %>% 
  right_join(ScoresSRTData, by ="StudyClientId", relationship = "many-to-many") %>% 
  filter(AssessmentType != "Interim Assessment") %>% 
  mutate(RiskLevel = ifelse(ProfessionalOverride == 1, RiskLevelFinalOverride, RiskLevel),
         TotalScore = Q1_Score+Q2_Score+Q3_Score+Q4_Score) %>% 
  select(-(Q1_1:Q4_Score), -(ProfessionalOverride:RiskLevelFinalOverride), -BiologicalGender, -AgeAtAssessment)

#changing order of variables for CST so I can append the datasets together
ClientScoresCST <- ClientScoresCST[,c(1:14,16:17,15)]

#appending all three datasets
ClientScoresAll <- bind_rows(ClientScoresCST, ClientScoresRT, ClientScoresSRT)

#counting clientIds to find how many appear more than once
duplicates <- ClientScoresAll %>% 
  count(StudyClientId) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  select(StudyClientId)

#creating a list with duplicates to filter dataset
#extracting only duplicate IDs
duplicates_list <- list(duplicates$StudyClientId)
duplicate <- duplicates_list[[1]]
duplicate_data <- ClientScoresAll %>% 
  filter(StudyClientId %in% duplicate)

length(unique(duplicate_data$StudyClientId))
#there are 380 clients that have answered the forms more than once

#combining the dataset with all client scores & info with episode data
EpisodeClientScores <- left_join(ClientScoresAll, EpisodeData, by=c("StudyClientId", "StudyEpisodeId"))

#changing assessmentyear and type variables so that they don't match the exact names 
RiskDataRevised <- RiskDataRevised %>% 
  rename(AssessmentTypeSuicide = AssessmentType, AssessmentYearSuicide = AssessmentYear)

#combining full dataset with homicide/suicide risk data 
RiskClientScores <- EpisodeClientScores %>% 
  left_join(RiskDataRevised, by = c("StudyClientId", "StudyEpisodeId"),
            relationship = "many-to-many") %>% 
  select(-(fldSuicideWishedDead:fldHomicidePreparationAddInfo), -(HomeTownCity:HomeTownZip), -Interim, -AssessmentTypeSuicide) %>% 
  filter(WhenTaken != "Interim")

#---------------------------------------------------------
# data management
#---------------------------------------------------------

#creating binary for successful vs not successful outcomes 
RiskClientScores <- RiskClientScores %>% 
  mutate(Success = ifelse(DischargeStatus=="Successful", 1, 0))

#saving datafile for further analysis 
save(RiskClientScores, file = "cleaned.Rdata")
