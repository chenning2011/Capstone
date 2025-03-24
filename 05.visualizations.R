#---------------------------------------------------------
# alluvial graph of living arrangements
#---------------------------------------------------------

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
  count(Admission, Discharge) %>% 
  na.omit() 

#creating id column for reshaping data with
alluvial <- alluvial %>% 
  mutate(id = 1:nrow(alluvial))

#changing shape of data and renaming columns
library(reshape2)
library(ggalluvial)
alluvial_long <- melt(alluvial, id = c("id", "n"))
names(alluvial_long) <- c("id", "Number", "Time", "Arrangement")

#graph code
ggplot(alluvial_long, aes(x=Time, stratum=Arrangement, alluvium=id, y=Number,fill=Arrangement))+ 
  geom_flow()+ 
  geom_stratum()+ 
  theme_minimal()+
  theme(legend.position="bottom")+ 
  ggtitle("Living Arrangements At Admission and Discharge")+
  scale_fill_brewer(palette = "Spectral")
