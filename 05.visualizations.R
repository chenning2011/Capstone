#---------------------------------------------------------
# alluvial graph of living arrangements
#---------------------------------------------------------

#libraries and dataset
library(tidyverse)
library(reshape2)
library(ggalluvial)
load("cleaned.Rdata")
load("duplicates.Rdata")

#making new dataset for graphing purposes
alluvial <- RiskClientScores

#collapsing categories
alluvial <- alluvial %>% 
  mutate(LivingArrangementAtAdmission = case_when(
    LivingArrangementAtAdmission == "CTDOC- Contracted Residential Work Release Program" ~ "Work Release",
    LivingArrangementAtAdmission %in% c("Inpatient Hospitalization long term for Psychiatric or SA", 
                                        "Inpatient Hospitalization long term for medical")  ~ "Inpatient",
    LivingArrangementAtAdmission %in% c("Other Residence - Family or Friends",
                                        "Independent Residence") ~ "Residence",
    LivingArrangementAtAdmission %in% c("Homeless Shelter w/ Case  Management", "Literally homeless",
                                        "Homeless Shelter w/out case management") ~ "Homeless",
    LivingArrangementAtAdmission=="Incarcerated / Correctional Facility" ~ "Incarcerated",
    LivingArrangementAtAdmission=="Residential Treatment" ~ "Treatment",
    LivingArrangementAtAdmission %in% c("Scattered Site", "Sober Housing / Halfway House") ~ "Halfway House",
    LivingArrangementAtAdmission %in% c("Supportive Housing", "Transitional Housing") ~ "Supportive/Transitional Housing",
    TRUE ~ LivingArrangementAtAdmission
  ), 
  LivingArrangementAtDischarge = case_when(
    LivingArrangementAtDischarge == "CTDOC- Contracted Residential Work Release Program" ~ "Work Release",
    LivingArrangementAtDischarge %in% c("Inpatient Hospitalization long term for Psychiatric or SA", 
                                        "Inpatient Hospitalization long term for medical")  ~ "Inpatient",
    LivingArrangementAtDischarge %in% c("Other Residence - Family or Friends",
                                        "Independent Residence") ~ "Residence",
    LivingArrangementAtDischarge %in% c("Homeless Shelter w/ Case  Management", "Literally homeless",
                                        "Homeless Shelter w/out case management") ~ "Homeless",
    LivingArrangementAtDischarge=="Incarcerated / Correctional Facility" ~ "Incarcerated",
    LivingArrangementAtDischarge=="Residential Treatment" ~ "Treatment",
    LivingArrangementAtDischarge %in% c("Scattered Site", "Sober Housing / Halfway House") ~ "Halfway House",
    LivingArrangementAtDischarge %in% c("Supportive Housing", "Transitional Housing") ~ "Supportive/Transitional Housing",
    TRUE ~ LivingArrangementAtDischarge
  ))

#changing variable names
alluvial <- alluvial %>% 
  rename(Admission = LivingArrangementAtAdmission, Discharge = LivingArrangementAtDischarge)

#counting the people who go from each category between admission and discharge
alluvial <- alluvial %>% 
  count(ProgramName, Admission, Discharge) %>% 
  na.omit() 

#creating id column for reshaping data with
alluvial <- alluvial %>% 
  mutate(id = 1:nrow(alluvial))

#changing shape of data and renaming columns
alluvial_long <- melt(alluvial, id = c("id", "n", "ProgramName"))
names(alluvial_long) <- c("id", "Number", "ProgramName","Time", "Arrangement")

#creating proportions 
#step 1. total values of people at each program 
total <- alluvial_long %>% 
  group_by(ProgramName, Time) %>% 
  summarize(sum = sum(Number))
#step 2. add totals into dataset
alluvial_long <- alluvial_long %>% 
  left_join(total, by = c("ProgramName", "Time"))
#step 3. create proportions
alluvial_long <- alluvial_long %>% 
  mutate(prop = Number/sum)

#graph code, with program name facet added in
ggplot(alluvial_long, aes(x=Time, stratum=Arrangement, alluvium=id, y=prop,fill=Arrangement))+ 
  geom_flow()+ 
  geom_stratum()+ 
  theme_minimal()+
  facet_wrap(~ProgramName)+
  theme(legend.position="bottom")+ 
  labs(x="Time", y="Percent", title = "Living Arrangements At Admission and Discharge")+
  scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = scales::percent)


#take a look at risk/success within programs as well     
