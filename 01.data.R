#---------------------------------------------------------
# importing data files
#---------------------------------------------------------

#libraries
library(tidyverse)
library(readxl)

#supplement to the re-entry form scores
#applies only to people who have been incarcerated for less than 4 years
ScoresSRTData <- read_excel("Data Files/ScoresSRTData.xlsx")

#re-entry form scores (scoring them before they re-enter the community)
#condition to fill out this form: within 6 months of discharge/re-entry date and unrestricted by security concerns
#this form is given to people who are in prison
#each one of these forms is made to identify the likelihood of recidivism 
#applies to people who have been in jail for more than 4 years
ScoresRTData <- read_csv("Data Files/ScoresRTData.csv")

#community supervision tool scores
#condition to fill out this form: charged with criminal offense and referred to probation 
#these people are allowed to remain within the community, but are supervised
#the form's goal is to help decide the level/needs for supervision
ScoresCSTData <- read_excel("Data Files/ScoresCSTData.xlsx")

#where people are staying, how long they've been there, and past trauma they're willing to disclose
EpisodeTraumaData <- read_excel("Data Files/EpisodeTraumaData.xlsx")

#suicide and homocide risk levels at admission for patients 
#taken from a form - adapted from columbia suicide severity rating scale
RiskDataRevised <- read_excel("Data Files/RiskDataRevised.xlsx")

#this is the most important one 
#this contains information on the patients discharge dates, where they went to, where they were before
#also their income levels and residences, if applicable 
EpisodeData <- read_excel("Data Files/EpisodeData.xlsx", 
                          col_types = c("numeric", "numeric", "text", 
                                         "text", "numeric", "numeric", "numeric", 
                                         "numeric", "text", "text", "text", 
                                         "text", "numeric", "text", "text", 
                                         "numeric", "text", "text", "text", 
                                         "text", "skip"))

#this one is interesting, but might not be as useful
#essentially looking at a series of 65 questions, each of which is scored from 0-4 
#each of the scores is tallied and relates to a specific category, which is given an overall score 
#these scores are risk ratings, and have "normal ranges" that people can go outside of 
#would need to categorize these scores 
CtpData <- read_excel("Data Files/CtpData.xlsx")

#demographic information
ClientData <- read_excel("Data Files/ClientData.xlsx")

#data on incidents that they could link clients to -> might not be as helpful
#need more information on when these incidents are occuring (is it while they're under probation?)
#for this one, download the new wide version from teams before doing work with 
IncidentData <- read_excel("Data Files/IncidentData.xlsx")

#ASUS substance use data 
ASUS_dimension_scores <- read_excel("Data Files/ASUS_dimension_scores.xlsx")

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

#combining the dataset with all client scores & info with episode data
EpisodeClientScores <- left_join(ClientScoresAll, EpisodeData, by=c("StudyClientId", "StudyEpisodeId"))

#changing assessmentyear and type variables so that they don't match the exact names
#removing interim assessments and keeping only distinct pairs to prepare for joining to larger dataset
RiskDataRevised <- RiskDataRevised %>% 
  rename(AssessmentTypeSuicide = AssessmentType, AssessmentYearSuicide = AssessmentYear) %>% 
  filter(WhenTaken != "Interim") %>% 
  distinct(StudyClientId, StudyEpisodeId, .keep_all = T)

#combining full dataset with homicide/suicide risk data 
RiskClientScores <- EpisodeClientScores %>% 
  left_join(RiskDataRevised, by = c("StudyClientId", "StudyEpisodeId"),
            relationship = "many-to-many") %>% 
  select(-(fldSuicideWishedDead:fldHomicidePreparationAddInfo), -(HomeTownCity:HomeTownState), -Interim, -AssessmentTypeSuicide) %>% 
  filter(WhenTaken != "Interim")

#---------------------------------------------------------
# adding ctp data
#---------------------------------------------------------
#removing year and interim assessment from ctp 
#keeping only distinct data so that if someone was given the CTP 
#assessment more than once, only the first listed results are kept to prepare for join
CtpData <- CtpData %>% 
  filter(AssessmentType!="Interim Assessment") %>% 
  select(-AssessmentYear, -AssessmentType) %>% 
  distinct(StudyClientId, StudyEpisodeId, .keep_all = T)

RiskClientScores <- RiskClientScores %>% 
  left_join(CtpData, by = c("StudyClientId", "StudyEpisodeId"), 
            relationship = "many-to-many")

#adding in ASUS data 
ASUS_dimension_scores <- ASUS_dimension_scores %>% 
  rename(StudyClientId = StudyClientID,
         StudyEpisodeId = StudyEpisodeID)

RiskClientScores <- RiskClientScores %>% 
  left_join(ASUS_dimension_scores, by = c("StudyClientId", "StudyEpisodeId"), 
            relationship = "many-to-many") 

#counting clientIds to find how many appear more than once
duplicates <- RiskClientScores %>% 
  count(StudyClientId) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  select(StudyClientId)

#creating a list with duplicates to filter dataset
#extracting only duplicate IDs
duplicates_list <- list(duplicates$StudyClientId)
duplicate <- duplicates_list[[1]]
duplicate_data <- RiskClientScores %>% 
  filter(StudyClientId %in% duplicate) %>% 
  select(StudyClientId, StudyEpisodeId, everything())

#saving duplicate data 
save(duplicate_data, file ="duplicates.Rdata")

#removing some unexplainable duplicates that appear to be errors 
RiskClientScores <- RiskClientScores %>% 
  filter(!(StudyClientId==54 & StudyEpisodeId == 3029 & AssessmentYear == 2022)) %>% 
  filter(!(StudyClientId==1759 & StudyEpisodeId == 3445 & AssessmentYear == 2023))

#---------------------------------------------------------
# data management
#---------------------------------------------------------
#creating binary for successful vs not successful outcomes 
RiskClientScores <- RiskClientScores %>% 
  mutate(Success = ifelse(DischargeStatus %in% c("Successful", "Transferred", 
                                                 "Completed Program/Treatment","Auto-Discharged"), 
                          1, 0))

#creating 3-level outcome variable for multinomial regressions
RiskClientScores <- RiskClientScores %>%
  mutate(Outcome = case_when(DischargeStatus %in% c("Successful", "Transferred",
                                                    "Completed Program/Treatment",
                                                    "Auto-Discharged") ~ "Successful", 
                             DischargeStatus %in% c("Arrested New", "Escaped",
                                                    "Discharged to Higher Level of Care", 
                                                    "Remanded") ~ "Unsuccessful", 
                             DischargeStatus %in% c("Deceased", "Medical") ~ "Neutral")
  )


#data management for race variable
RiskClientScores <- RiskClientScores %>% 
  mutate(Race = ifelse(Race == "Some other race"|Race == "Asian"| Race == "Native"| 
                         Race == "Other Pacific Islander"|Race == "Multi-Racial"|
                         Race == "American Indian or Alaskan Native"| Race == "Native Hawaiian/Other Pacific Islander",
                       "Other", Race),
         Race = ifelse(Race == "Undisclosed"|Race == "Not on file", NA, Race))

#data management for ethnicity variable 
RiskClientScores <- RiskClientScores %>% 
  mutate(Ethnicity = ifelse(Ethnicity == "Unknown", NA, Ethnicity))

#creating hispanic/latino variable 
RiskClientScores <- RiskClientScores %>% 
  mutate(Hispanic = ifelse(Ethnicity == "No, Not of Hispanic, Latino, or Spanish Origin.", 0, 1),
         Hispanic = factor(Hispanic))

#data management for marital status variable 
RiskClientScores <- RiskClientScores %>% 
  mutate(MaritalStatus = case_when(MaritalStatus=="Not Specified" ~ NA, 
                                   MaritalStatus %in% c("Divorced/Annulled", "Widow/widower", 
                                                        "Legally separated", "Other", 
                                                        "Single/Never Married") ~ "Not Currently Married",
                                   MaritalStatus=="Married" ~ "Married"))

#data management for religion variable
RiskClientScores <- RiskClientScores %>% 
  mutate(Religion = ifelse(Religion == "Unknown", NA, Religion),
         Religion = ifelse(Religion == "Atheist"|Religion=="Agnostic"|Religion =="None", 
                           "None", Religion),
         Religion = ifelse(Religion %in% c("Catholic", "Christian", "Orthodox Christian",
                                           "Pentecostal", "Baptist", "Mormon", "Protestant"), 
                           "Christian", Religion))

#setting reference level for religion variable 
RiskClientScores$Religion <- factor(RiskClientScores$Religion, 
                                    levels = c("None", "Christian", "Buddhist", "Islamic", "Jewish", "Other"))

#data management for primary language 
RiskClientScores <- RiskClientScores %>% 
  mutate(PrimaryLanguage = ifelse(PrimaryLanguage=="Not Specified"|PrimaryLanguage=="Unknown", NA, PrimaryLanguage))

#setting reference level for primary language 
RiskClientScores$PrimaryLanguage <- factor(RiskClientScores$PrimaryLanguage, 
                                           levels = c("English", "Arabic", "Chinese/Mandarin", "Spanish", "Vietnamese"))

#data management for english as primary language 
RiskClientScores <- RiskClientScores %>% 
  mutate(English = ifelse(PrimaryLanguage=="English", 1, 0))

#setting english as a factor 
RiskClientScores$English <- factor(RiskClientScores$English)

#data management for suicide risk 
RiskClientScores <- RiskClientScores %>% 
  mutate(SuicideRiskLevel = ifelse(SuicideRiskLevel=="High risk - contact supervisor immediately, increase monitoring, seek clinical support",
                                   "High risk", SuicideRiskLevel),
         SuicideRiskLevel = ifelse(SuicideRiskLevel=="Moderate risk - review client's personal safety plan and implement as needed",
                                   "Moderate risk", SuicideRiskLevel))

#setting suicide risk as factor and changing reference level 
RiskClientScores$SuicideRiskLevel <- factor(RiskClientScores$SuicideRiskLevel, 
                                            levels = c("No endorsed risk", "Moderate risk", "High risk"))

#data management for homicide risk 
RiskClientScores <- RiskClientScores %>% 
  mutate(HomocideRiskLevel = ifelse(HomocideRiskLevel=="High risk - contact supervisor immediately, increase monitoring, seek clinical support",
                                    "High risk", HomocideRiskLevel),
         HomocideRiskLevel = ifelse(HomocideRiskLevel=="Moderate risk - review client's personal safety plan and implement as needed",
                                    "Moderate risk", HomocideRiskLevel))

#setting homicide risk as factor and changing reference level 
RiskClientScores$HomocideRiskLevel <- factor(RiskClientScores$HomocideRiskLevel, 
                                            levels = c("No endorsed risk", "Moderate risk", "High risk"))

#collapsing categories for suicide risk and homicide risk 
RiskClientScores <- RiskClientScores %>% 
  mutate(Homicide = ifelse(HomocideRiskLevel=="No endorsed risk", "No risk", "Risk"),
         Suicide = ifelse(SuicideRiskLevel=="No endorsed risk", "No risk", "Risk"))

#collapsing categories for risk level 
RiskClientScores <- RiskClientScores %>% 
  mutate(RiskLevel = ifelse(RiskLevel == "High"| RiskLevel =="Very High", "High", RiskLevel))

#setting risk level reference level 
RiskClientScores$RiskLevel <- factor(RiskClientScores$RiskLevel, levels = c("Low", "Moderate", "High"))

#saving datafile for further analysis 
save(RiskClientScores, file = "cleaned.Rdata")

#finding number of participants
length(unique(RiskClientScores$StudyClientId))
#1129 unique participants after data management 

#---------------------------------------------------------
# correlations in asus and ctp data (trying to reduce dimensions)
#---------------------------------------------------------

library(qacBase)

#getting correlation matrix for quantitative variables in the dataset 
cor_plot(ASUS_dimension_scores)
#many of these numbers are highly correlated (almost have a correlation of 1)
#need to do something with these numbers 

#ctp data 
cor_plot(CtpData)
#high correlation between recklessimpulsivity and poor judgement, 
#poor judgement and justifying/disregard for others, and justifying and disregard for others
#all values positively correlated

