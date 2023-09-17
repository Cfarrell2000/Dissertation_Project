######## Cathy Farrell             #########
######## R Code                    #########
######## Student number: 40386708  #########
######## R version 4.3.0           #########


############ Data Import and Manipulations ###########

#Set working directory as the "Researcher Project" Folder created 
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project")

#Load R packages 
library("tidyverse")
library("ggplot2")
library(dplyr)
library(ggpubr)
library(EnvStats)
library(gridExtra)
library("grid")
library(plyr)
library(readr)
library(readxl)

#Import the data "NI.study.117.cases with linked clinical information in R console: 
NI.study <- read.csv("C:/Users/cathy/OneDrive/Desktop/Research Project/NI study-117 cases with clinical data.QuPath-300523.csv")
#View imported file check R imported it correctly 
View(NI.study)
#Look at the structure of the data 
str(NI.study)

#Replace N/A with NA 
NI.study<- replace(NI.study, NI.study=="N/A", NA)
#Replace NK with NA
NI.study <- replace(NI.study, NI.study=="NK", NA)
#Replace unknown with NA
NI.study<- replace(NI.study, NI.study=="unknown", NA)
#Replace Case with lethal Pca 
NI.study <- replace(NI.study, NI.study=="Case", "lethal PCa")
#Replace Control with nonlethal pca 
NI.study <- replace(NI.study, NI.study=="Control", "non-lethal PCa")
#Replace See comments with Dead 
NI.study <- replace(NI.study, NI.study=="See comments", "Alive")
#View  the data 
View(NI.study)
#Add GS_status column to define 3+4 vs 4+3 (If primary pattern 3 = 3+4 and if GS primary pattern is 4 = 4+3)  
NI.study$GS_status<- ifelse(NI.study$Gleason.score < 7, "<7",
                            ifelse(NI.study$Gleason.primary.pattern == 3 & NI.study$Gleason.secondary.pattern == 4, "3+4",
                                   ifelse(NI.study$Gleason.primary.pattern == 4 & NI.study$Gleason.secondary.pattern == 3, "4+3",
                                          ">7")))

#Give the columns new names to make our data frame a bit tidier 
colnames(NI.study)<- c("X", "NIB.Number", "Cohort", "Age.at.diagnosis", "Year.of.Diagnosis", "Race", "Ethnicity", "Tumour Clinical Stage", "Gleason primary pattern", "Gleason secondary pattern", 
                       "Gleason score", "PSA..ng.ml..closest.to.but.prior.to.diagnosis", "Trust...ALTP..ANTP..CAHP..RVHP.", "Days.from.diagnosis.to.commencement.of.primary.treatment..Column.L.", 
                       "Days.from.diagnosis.to.commencement.of.ADT", "Days.from.diagnosis.to.finish.of.ADT" , "Body mass index" , "Complete.blood.count...at.time.of.diagnosis.or.if.not.recorded.at.diagnosis..at.the.closest.point.prior.to.diagnosis.." , "Lipid.profile...at.time.of.diagnosis.or.if.not.recorded.at.diagnosis..at.the.closest.point.prior.to.diagnosis..",
                       "Was.pt.fasting.or.not.at.blood.draw..Y.N.NK", "Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N", 
                       "PSA.values..ng.ml..from.follow.up..top.value.is.that.recorded.at.closest.point.after.diagnosis.and.subsequent.follow.up.values.below..most.recent.at.the.bottom..", "Metastasis..Y.N" , "Days.from.primary.diagnosis.to.metastasis" , "Site.s..of.metastasis", 
                       "Vital.status..alive.dead.", "Cause.of.death", "Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.", 
                       "Comments", "BCR..Y.N", "PSA.value..ng.ml..at.BCR", "Days.from.primary.diagnosis.to.BCR", "NICR..Comment.regarding.the.Date.used.for.BCR.value", 
                       "primary_treatment", "history_of_hypercholesterolemia", "history_of_diabetes", "history_cardiovascular_disease", "hypertension", "family_history_pca", "smoking_status","alcohol_consumption", "Study.ID", "Multiplex.IMF", "Multiplex.image.ID", 
                       "Fused.status", "Fused.Batch", "Notes", "Gland.percentage.of.tumour.core", "Stroma.percentage.of.tumour.core", "Tissue.compartment", 
                       "CD3+(TG)", "CD3+PD-1+(TG)", "CD3+CD4+(TG)", "CD3+CD4+FOXP3+(TG)", "CD3+CD4+FOXP3+PD-1+(TG)", "CD3+CD4+PD-1+(TG)", "CD3+CD4+PD-1-(TG)", "CD3+CD4+PD-1-FOXP3-(TG)", "CD3+CD8+(TG)","CD3+CD8+FOXP3+(TG)", "CD3+CD8+FOXP3+PD-1+(TG)", "CD3+CD8+PD-1+(TG)", "CD3+CD8+PD-1-(TG)", "CD3+CD8+PD-1-FOXP3-(TG)", "CD68+(TG)", 
                       "Tissue.compartment.1", "CD3+(TS)", "CD3+PD-1+(TS)", "CD3+CD4+(TS)", "CD3+CD4+FOXP3+(TS)", "CD3+CD4+FOXP3+PD-1+(TS)", "CD3+CD4+PD-1+(TS)", "CD3+CD4+PD-1-(TS)", "CD3+CD4+PD-1-FOXP3-(TS)", "CD3+CD8+(TS)","CD3+CD8+FOXP3+(TS)", "CD3+CD8+FOXP3+PD-1+(TS)", "CD3+CD8+PD-1+(TS)", "CD3+CD8+PD-1-(TS)", "CD3+CD8+PD-1-FOXP3-(TS)", "CD68+(TS)", "GS_status" )

#Import NICC clinical file which contain all clinical info and four new columns "erg_cutpoint", "PTEN", "PTENloss_zscore", "PTEN_status"
NICC_clinical_SJW_090623 <- read.delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PTEN_Tumour.txt")
#View imported file check R imported it correctly 
View(NICC_clinical_SJW_090623)
#Merge the file "NICC_clinical_SJW_090623" and add four new columns "erg_cutpoint", "PTEN", "PTENloss_zscore", "PTEN_status" with the NI.study   
NI.study <- merge(NI.study, NICC_clinical_SJW_090623[c("NIB.Number", "erg_cutpoint", "PTEN", "PTENloss_zscore", "PTENloss_zscore_median", "Sample.ID")], all.x = TRUE)
#Import data "NI.study-117 cases with clinical data.QuPath-named phenotypes.2 new.ERG.PTEN.110623.xlsx" file which contains extra column "days from primary diagnosis to PCa death or metatsasis or last follow up"
NI_study_117<- read_excel("NI study-117 cases with clinical data.QuPath-named phenotypes.2 new.ERG.PTEN.110623.xlsx")
#Merge new column "days from primary diagnosis to PCa death or metatsasis or last follow up" onto NI.study by NIB.Number
NI.study <- merge(NI.study, NI_study_117[c("NIB.Number", "Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up")], all.x = T)
#View the data to check the two file merged correctly
View(NI.study)
#Allow direct access to columns 
attach(NI.study)
#Add new column PTEN status (> median = PTEN-null and < median= PTEN-intact)
NI.study <- NI.study %>% 
  add_column(PTEN_status= case_when(
    startsWith(PTENloss_zscore_median, ">") ~ "PTEN-null", 
    startsWith(PTENloss_zscore_median, "<") ~ "PTEN-intact",  
  ))                 
#View data 
View(NI.study)
#Set factor for Tumour Clinical Stage and define there levels 
NI.study$`Tumour Clinical Stage` <- factor(NI.study$`Tumour Clinical Stage`, labels=c("ST1", "ST2", "ST3"))
#Check Structure Tumour Clinical Stage variable
str(NI.study$`Tumour Clinical Stage`) 
class(NI.study$`Tumour Clinical Stage`) #check class 
levels(NI.study$`Tumour Clinical Stage`) #check levels
#Set factor for GS_status and define there levels 
NI.study$GS_status <- factor(NI.study$GS_status, labels = c("3+4", "4+3"))
class(NI.study$GS_status) #check class
levels(NI.study$GS_status) #check levels
#Change Days from primary diagnosis to metastasis to numeric 
NI.study$Days.from.primary.diagnosis.to.metastasis <- as.numeric(NI.study$Days.from.primary.diagnosis.to.metastasis)
str(NI.study$Days.from.primary.diagnosis.to.metastasis) #Check Structure
#Change Days from primary diagnosis to metastasis to numeric 
NI.study$Days.from.primary.diagnosis.to.BCR <- as.numeric(NI.study$Days.from.primary.diagnosis.to.BCR)
str(NI.study$Days.from.primary.diagnosis.to.BCR) #Check Structure
#Attach to allow access to columns directly 
attach(NI.study)

#Define patients by immune subtype i.e., > or < median for each immune cell phenotype: 

#Define immune-subtypes: CD3+(TG) i.e., > or < median 
NI.study <- NI.study %>% mutate(CD3_median_TG= median(`CD3+(TG)`)) %>% 
  mutate(CD3positivemedianTG = case_when(
    `CD3+(TG)` > CD3_median_TG ~ "abovemedian", 
    `CD3+(TG)` < CD3_median_TG ~  "equalbelowmedian",
    `CD3+(TG)` == CD3_median_TG ~  "equalbelowmedian"))
#View Data
View(NI.study)

#Define immune-subtypes: CD3+(TS) i.e., > or < median 
NI.study <- NI.study %>% mutate(CD3_median_TS= median(`CD3+(TS)`)) %>% 
  mutate(CD3positivemedianTS = case_when(
    `CD3+(TS)` > CD3_median_TS ~ "abovemedian", 
    `CD3+(TS)` < CD3_median_TS ~  "equalbelowmedian",
    `CD3+(TS)` == CD3_median_TS ~  "equalbelowmedian"))
#View Data
View(NI.study)

#Define immune-subtypes: CD3+CD4+(TG) i.e., > or < median 
NI.study <- NI.study %>% mutate(CD3CD4_median_TG= median(`CD3+CD4+(TG)`)) %>% 
  mutate(CD3CD4positivemedianTG = case_when(
    `CD3+CD4+(TG)` > CD3CD4_median_TG ~ "abovemedian", 
    `CD3+CD4+(TG)` < CD3CD4_median_TG ~  "equalbelowmedian",
    `CD3+CD4+(TG)` == CD3CD4_median_TG ~  "equalbelowmedian"))
#View Data
View(NI.study)

#Define immune-subtypes: CD3+CD4+(TS) i.e., > or < median 
NI.study <- NI.study %>% mutate(CD3CD4_median_TS= median(`CD3+CD4+(TS)`)) %>% 
  mutate(CD3CD4positivemedianTS = case_when(
    `CD3+CD4+(TS)` > CD3CD4_median_TS ~ "abovemedian", 
    `CD3+CD4+(TS)` < CD3CD4_median_TS ~  "equalbelowmedian",
    `CD3+CD4+(TS)` == CD3CD4_median_TS ~  "equalbelowmedian"))
#View Data
View(NI.study)

#Define immune-subtypes: CD68+(TG) i.e., > or < median 
NI.study <- NI.study %>% mutate(CD68_median_TG= median(`CD68+(TG)`)) %>% 
  mutate(CD68positivemedianTG = case_when(
    `CD68+(TG)` > CD68_median_TG ~ "abovemedian", 
    `CD68+(TG)` < CD68_median_TG ~  "equalbelowmedian",
    `CD68+(TG)` == CD68_median_TG ~  "equalbelowmedian"))
#View Data
View(NI.study)

#Define immune-subtypes: CD68+(TS) i.e., > or < median 
NI.study <- NI.study %>% mutate(CD68_median_TS= median(`CD68+(TS)`)) %>% 
  mutate(CD68positivemedianTS = case_when(
    `CD68+(TS)` > CD68_median_TS ~ "abovemedian", 
    `CD68+(TS)` < CD68_median_TS ~  "equalbelowmedian",
    `CD68+(TS)` == CD68_median_TS ~  "equalbelowmedian"))
#View Data
View(NI.study)

#need to factor immune subtype i.e., > or < median for each immune cell phenotype:

#need to set CD3postivemedianTG to a factor
NI.study$CD3positivemedianTG <- factor(NI.study$CD3positivemedianTG, labels = c("equalbelowmedian", "abovemedian"))
#need to set CD3postivemedianTS to a factor: 
NI.study$CD3positivemedianTS <- factor(NI.study$CD3positivemedianTS, labels = c("equalbelowmedian", "abovemedian"))
#need to set CD3CD4postivemedianTG to a factor
NI.study$CD3CD4positivemedianTG <- factor(NI.study$CD3CD4positivemedianTG, labels = c("equalbelowmedian", "abovemedian"))
#need to set CD3CD4postivemedianTS to a factor: 
NI.study$CD3CD4positivemedianTS <- factor(NI.study$CD3CD4positivemedianTS, labels = c("equalbelowmedian", "abovemedian"))
#need to set CD68postivemedianTG to a factor
NI.study$CD68positivemedianTG <- factor(NI.study$CD68positivemedianTG, labels = c("equalbelowmedian", "abovemedian"))
#need to set CD68postivemedianTS to a factor: 
NI.study$CD68positivemedianTS <- factor(NI.study$CD68positivemedianTS, labels = c("equalbelowmedian", "abovemedian"))

#Add Column for OS analysis: EventOS defines as event occurring death and alive not occuring: 
NI.study <- NI.study %>% 
  add_column(EventOS= case_when(
    startsWith(Vital.status..alive.dead., "D")~ 1,  #Dead = event occurred 
    startsWith(Vital.status..alive.dead., "A")~ 0,  #Alive = event has not occurred
  ))  
#View how many event have occurred/not occured 
table(NI.study$EventOS) 

#Add column for PCa-specific survival analysis: Event defined as the event occurring (i.e., having lethal PCa = event 1 and nonlethal PCa; event = 0) 
NI.study <- NI.study %>% 
  add_column(EventLPCA= case_when(
    startsWith(Cohort, "l")~ 1,  #l = lethal PCa (event has occurred)
    startsWith(Cohort, "n")~ 0,  #n= nonlethal PCa (event has not occurred)
  ))    
#View how many events have occurred/not occurred
table(NI.study$EventLPCA)

#Subset out the non-lethal PCa cases (i.e., Sub-Cohort)
Nonlethal <- subset(NI.study, NI.study$Cohort== "non-lethal PCa")
Nonlethal

#####################################################################################################################################################################################
############ Summarise the data ###########

#Use this approach for categorical variables:

#Categorical vairbale: PTEN status 
table(NI.study$Cohort, NI.study$PTEN_status)

#Categorical variable: Tumour Clinical Stage
table(NI.study$Cohort, NI.study$`Tumour Clinical Stage`)

#Categorical variable: GS_status 
table(NI.study$Cohort, NI.study$GS_status)

#Categorical variable: TRUST
table(NI.study$Cohort, NI.study$Trust...ALTP..ANTP..CAHP..RVHP.)

#Categorical variable: Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N
table(NI.study$Cohort, NI.study$Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)

#Categorical variable: Metastasis
table(NI.study$Cohort, NI.study$Metastasis..Y.N)

#Categorical variable: Sites of Metastasis 
table(NI.study$Cohort, NI.study$Site.s..of.metastasis)

#Categorical variable: vital status 
table(NI.study$Cohort, NI.study$Vital.status..alive.dead.)

#Categorical variable: Cause of death 
table(NI.study$Cohort, NI.study$Cause.of.death)

#Categorical variable: BCR 
table(NI.study$Cohort, NI.study$BCR..Y.N)

#Categorical variable: primary treatment
table(NI.study$Cohort, NI.study$primary_treatment)

#Categorical variable: history of hypercholesterolemia
table(NI.study$Cohort, NI.study$history_of_hypercholesterolemia)

#Categorical variable: history of diabetes 
table(NI.study$Cohort, NI.study$history_of_diabetes)

#Categorical variable: history of CVD 
table(NI.study$Cohort, NI.study$history_cardiovascular_disease)

#Categorical variable: hypertension 
table(NI.study$Cohort, NI.study$hypertension)

#Categorical variable:family history of PCa
table(NI.study$Cohort, NI.study$family_history_pca)

#Categorical variable: smoking status 
table(NI.study$Cohort, NI.study$smoking_status)

#Categorical variable: alcohol consumption 
table(NI.study$Cohort, NI.study$alcohol_consumption)

#Use this approach for continuous variables: 

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and summarise PSA..ng.ml..closest.to.but.prior.to.diagnosis by mean, max, min , median, standard derivation 
NI.study %>% group_by(Cohort) %>% summarize(Mean=mean(PSA..ng.ml..closest.to.but.prior.to.diagnosis), Max=max(PSA..ng.ml..closest.to.but.prior.to.diagnosis), Min=min(PSA..ng.ml..closest.to.but.prior.to.diagnosis), Median=median(PSA..ng.ml..closest.to.but.prior.to.diagnosis), Std=IQR(PSA..ng.ml..closest.to.but.prior.to.diagnosis))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries Age at diagnosis by mean, max, min , median, standard derivation 
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Age.at.diagnosis), Max=max(Age.at.diagnosis), Min=min(Age.at.diagnosis), Median=median(Age.at.diagnosis), Std=IQR(Age.at.diagnosis))

#Group patients by cohort staus (i.e., lethal/non-lethal PCa) and find/summarise Days.from.diagnosis.to.commencement.of.primary.treatment..Column.L. by mean, max, min , median, standard derivation 
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Days.from.diagnosis.to.commencement.of.primary.treatment..Column.L.), Max=max(Days.from.diagnosis.to.commencement.of.primary.treatment..Column.L.), Min=min(Days.from.diagnosis.to.commencement.of.primary.treatment..Column.L.), Median=median(Days.from.diagnosis.to.commencement.of.primary.treatment..Column.L.), Std=IQR(Days.from.diagnosis.to.commencement.of.primary.treatment..Column.L.))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and summarise Days.from.diagnosis.to.commencement.of.ADT by mean, max, min , median, standard derivation 
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Days.from.diagnosis.to.commencement.of.ADT, na.rm=T), Max=max(Days.from.diagnosis.to.commencement.of.ADT, na.rm=T), Min=min(Days.from.diagnosis.to.commencement.of.ADT, na.rm=T), Median=median(Days.from.diagnosis.to.commencement.of.ADT, na.rm=T), Std=IQR(Days.from.diagnosis.to.commencement.of.ADT, na.rm=T))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries days from diagnosis to finish of ADT by mean, max, min , median, standard derivation 
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Days.from.diagnosis.to.finish.of.ADT, na.rm=T), Max=max(Days.from.diagnosis.to.finish.of.ADT, na.rm=T), Min=min(Days.from.diagnosis.to.finish.of.ADT, na.rm=T), Median=median(Days.from.diagnosis.to.finish.of.ADT, na.rm=T), Std=IQR(Days.from.diagnosis.to.finish.of.ADT, na.rm=T))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries days from primary diagnosis to metastasis by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Days.from.primary.diagnosis.to.metastasis), Max=max(Days.from.primary.diagnosis.to.metastasis), Min=min(Days.from.primary.diagnosis.to.metastasis), Median=median(Days.from.primary.diagnosis.to.metastasis), Std=IQR(Days.from.primary.diagnosis.to.metastasis))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries days from diagnosis to death/or last follow up by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.), Max=max(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.), Min=min(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.), Median=median(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.), Std=IQR(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries PSA value at BCRby mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(PSA.value..ng.ml..at.BCR, na.rm=T), Max=max(PSA.value..ng.ml..at.BCR, na.rm=T), Min=min(PSA.value..ng.ml..at.BCR, na.rm=T), Median=median(PSA.value..ng.ml..at.BCR, na.rm=T), Std=IQR(PSA.value..ng.ml..at.BCR, na.rm=T))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries days  days from primary diagnosis to BR by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Days.from.primary.diagnosis.to.BCR, na.rm=T), Max=max(Days.from.primary.diagnosis.to.BCR, na.rm=T), Min=min(Days.from.primary.diagnosis.to.BCR, na.rm=T), Median=median(Days.from.primary.diagnosis.to.BCR, na.rm=T), Std=IQR(Days.from.primary.diagnosis.to.BCR, na.rm=T))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries gland percentage of tumour core by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Gland.percentage.of.tumour.core), Max=max(Gland.percentage.of.tumour.core), Min=min(Gland.percentage.of.tumour.core), Median=median(Gland.percentage.of.tumour.core), Std=IQR(Gland.percentage.of.tumour.core))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries stroma percentage of tumour core by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(Stroma.percentage.of.tumour.core), Max=max(Stroma.percentage.of.tumour.core), Min=min(Stroma.percentage.of.tumour.core), Median=median(Stroma.percentage.of.tumour.core), Std=IQR(Stroma.percentage.of.tumour.core))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+(TG)`), Max=max(`CD3+(TG)`), Min=min(`CD3+(TG)`), Median=median(`CD3+(TG)`), Std=IQR(`CD3+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+(TS)`), Max=max(`CD3+(TS)`), Min=min(`CD3+(TS)`), Median=median(`CD3+(TS)`), Std=IQR(`CD3+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+PD-1+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+PD-1+(TG)`), Max=max(`CD3+PD-1+(TG)`), Min=min(`CD3+PD-1+(TG)`), Median=median(`CD3+PD-1+(TG)`), Std=IQR(`CD3+PD-1+(TG)`))
##Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+PD-1+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+PD-1+(TS)`), Max=max(`CD3+PD-1+(TS)`), Min=min(`CD3+PD-1+(TS)`), Median=median(`CD3+PD-1+(TS)`), Std=IQR(`CD3+PD-1+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+(TG)`), Max=max(`CD3+CD4+(TG)`), Min=min(`CD3+CD4+(TG)`), Median=median(`CD3+CD4+(TG)`), Std=IQR(`CD3+CD4+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+(TS)`), Max=max(`CD3+CD4+(TS)`), Min=min(`CD3+CD4+(TS)`), Median=median(`CD3+CD4+(TS)`), Std=IQR(`CD3+CD4+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+FOXP3+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+FOXP3+(TG)`), Max=max(`CD3+CD4+FOXP3+(TG)`), Min=min(`CD3+CD4+FOXP3+(TG)`), Median=median(`CD3+CD4+FOXP3+(TG)`), Std=IQR(`CD3+CD4+FOXP3+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+FOXP3+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+FOXP3+(TS)`), Max=max(`CD3+CD4+FOXP3+(TS)`), Min=min(`CD3+CD4+FOXP3+(TS)`), Median=median(`CD3+CD4+FOXP3+(TS)`), Std=IQR(`CD3+CD4+FOXP3+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+FOXP3+PD-1+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+FOXP3+PD-1+(TG)`), Max=max(`CD3+CD4+FOXP3+PD-1+(TG)`), Min=min(`CD3+CD4+FOXP3+PD-1+(TG)`), Median=median(`CD3+CD4+FOXP3+PD-1+(TG)`), Std=IQR(`CD3+CD4+FOXP3+PD-1+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+FOXP3+PD-1+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+FOXP3+PD-1+(TS)`), Max=max(`CD3+CD4+FOXP3+PD-1+(TS)`), Min=min(`CD3+CD4+FOXP3+PD-1+(TS)`), Median=median(`CD3+CD4+FOXP3+PD-1+(TS)`), Std=IQR(`CD3+CD4+FOXP3+PD-1+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+PD-1+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+PD-1+(TG)`), Max=max(`CD3+CD4+PD-1+(TG)`), Min=min(`CD3+CD4+PD-1+(TG)`), Median=median(`CD3+CD4+PD-1+(TG)`), Std=IQR(`CD3+CD4+PD-1+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+PD-1+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+PD-1+(TS)`), Max=max(`CD3+CD4+PD-1+(TS)`), Min=min(`CD3+CD4+PD-1+(TS)`), Median=median(`CD3+CD4+PD-1+(TS)`), Std=IQR(`CD3+CD4+PD-1+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+PD-1- density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+PD-1-(TG)`), Max=max(`CD3+CD4+PD-1-(TG)`), Min=min(`CD3+CD4+PD-1-(TG)`), Median=median(`CD3+CD4+PD-1-(TG)`), Std=IQR(`CD3+CD4+PD-1-(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+PD-1- density (Tumour Stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+PD-1-(TS)`), Max=max(`CD3+CD4+PD-1-(TS)`), Min=min(`CD3+CD4+PD-1-(TS)`), Median=median(`CD3+CD4+PD-1-(TS)`), Std=IQR(`CD3+CD4+PD-1-(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+PD-1-FOXP3- density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+PD-1-FOXP3-(TG)`), Max=max(`CD3+CD4+PD-1-FOXP3-(TG)`), Min=min(`CD3+CD4+PD-1-FOXP3-(TG)`), Median=median(`CD3+CD4+PD-1-FOXP3-(TG)`), Std=IQR(`CD3+CD4+PD-1-FOXP3-(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD4+PD-1-FOXP3- density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD4+PD-1-FOXP3-(TS)`), Max=max(`CD3+CD4+PD-1-FOXP3-(TS)`), Min=min(`CD3+CD4+PD-1-FOXP3-(TS)`), Median=median(`CD3+CD4+PD-1-FOXP3-(TS)`), Std=IQR(`CD3+CD4+PD-1-FOXP3-(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+(TG)`), Max=max(`CD3+CD8+(TG)`), Min=min(`CD3+CD8+(TG)`), Median=median(`CD3+CD8+(TG)`), Std=IQR(`CD3+CD8+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+(TS)`), Max=max(`CD3+CD8+(TS)`), Min=min(`CD3+CD8+(TS)`), Median=median(`CD3+CD8+(TS)`), Std=IQR(`CD3+CD8+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+FOXP3+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+FOXP3+(TG)`), Max=max(`CD3+CD8+FOXP3+(TG)`), Min=min(`CD3+CD8+FOXP3+(TG)`), Median=median(`CD3+CD8+FOXP3+(TG)`), Std=IQR(`CD3+CD8+FOXP3+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+FOXP3+ density (Tumour storma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+FOXP3+(TS)`), Max=max(`CD3+CD8+FOXP3+(TS)`), Min=min(`CD3+CD8+FOXP3+(TS)`), Median=median(`CD3+CD8+FOXP3+(TS)`), Std=IQR(`CD3+CD8+FOXP3+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+FOXP3+PD-1 density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+FOXP3+PD-1+(TG)`), Max=max(`CD3+CD8+FOXP3+PD-1+(TG)`), Min=min(`CD3+CD8+FOXP3+PD-1+(TG)`), Median=median(`CD3+CD8+FOXP3+PD-1+(TG)`), Std=IQR(`CD3+CD8+FOXP3+PD-1+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+FOXP3+PD-1 density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+FOXP3+PD-1+(TS)`), Max=max(`CD3+CD8+FOXP3+PD-1+(TS)`), Min=min(`CD3+CD8+FOXP3+PD-1+(TS)`), Median=median(`CD3+CD8+FOXP3+PD-1+(TS)`), Std=IQR(`CD3+CD8+FOXP3+PD-1+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+PD-1+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+PD-1+(TG)`), Max=max(`CD3+CD8+PD-1+(TG)`), Min=min(`CD3+CD8+PD-1+(TG)`), Median=median(`CD3+CD8+PD-1+(TG)`), Std=IQR(`CD3+CD8+PD-1+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+PD-1+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+PD-1+(TS)`), Max=max(`CD3+CD8+PD-1+(TS)`), Min=min(`CD3+CD8+PD-1+(TS)`), Median=median(`CD3+CD8+PD-1+(TS)`), Std=IQR(`CD3+CD8+PD-1+(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+PD-1- density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+PD-1-(TG)`), Max=max(`CD3+CD8+PD-1-(TG)`), Min=min(`CD3+CD8+PD-1-(TG)`), Median=median(`CD3+CD8+PD-1-(TG)`), Std=IQR(`CD3+CD8+PD-1-(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+PD-1- density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+PD-1-(TS)`), Max=max(`CD3+CD8+PD-1-(TS)`), Min=min(`CD3+CD8+PD-1-(TS)`), Median=median(`CD3+CD8+PD-1-(TS)`), Std=IQR(`CD3+CD8+PD-1-(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+PD-1-FOXP3- density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+PD-1-FOXP3-(TG)`), Max=max(`CD3+CD8+PD-1-FOXP3-(TG)`), Min=min(`CD3+CD8+PD-1-FOXP3-(TG)`), Median=median(`CD3+CD8+PD-1-FOXP3-(TG)`), Std=IQR(`CD3+CD8+PD-1-FOXP3-(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD3+CD8+PD-1-FOXP3- density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD3+CD8+PD-1-FOXP3-(TS)`), Max=max(`CD3+CD8+PD-1-FOXP3-(TS)`), Min=min(`CD3+CD8+PD-1-FOXP3-(TS)`), Median=median(`CD3+CD8+PD-1-FOXP3-(TS)`), Std=IQR(`CD3+CD8+PD-1-FOXP3-(TS)`))

#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD68+ density (Tumour gland) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD68+(TG)`), Max=max(`CD68+(TG)`), Min=min(`CD68+(TG)`), Median=median(`CD68+(TG)`), Std=IQR(`CD68+(TG)`))
#Group patients by cohort status (i.e., lethal/non-lethal PCa) and find/summaries CD68+ density (Tumour stroma) by mean, max, min , median, standard derivation
NI.study %>% group_by(Cohort) %>% summarise(Mean=mean(`CD68+(TS)`), Max=max(`CD68+(TS)`), Min=min(`CD68+(TS)`), Median=median(`CD68+(TS)`), Std=IQR(`CD68+(TS)`))

#Examine number patients by immune defined phenotype groups 
table(NI.study$CD3positivemedianTG, NI.study$Cohort) #CD3+ TG plus Cohort
table(NI.study$CD3positivemedianTS, NI.study$Cohort) #CD3+ TS plus Cohort
table(NI.study$CD3CD4positivemedianTG, NI.study$Cohort) #CD3+CD4+ TG plus Cohort
table(NI.study$CD3CD4positivemedianTS, NI.study$Cohort) #CD3+CD4+ TS plus Cohort
table(NI.study$CD68positivemedianTG, NI.study$Cohort) #CD68+ TG plus Cohort
table(NI.study$CD68positivemedianTS, NI.study$Cohort) #CD68+ TS plus Cohort

####################################################################################################################################################################################
#### Part One: How are immune cell densities related to tumour characteristics and outcomes? ####

#Part (a) •	Are immune cell densities associated with tumour characteristics (i.e., Grade and Stage) within the Sub-Cohort
#Are Immune cell densities related to tumour characteristics: 

#Load R packages 
library(tidyverse)
library(ggpubr)
library(rstatix)

#Tumour Clinical Stage#

#CD3+ (TG) density and Tumour Clinical Stage: 
#Summary statistics: compute the median and IQR of CD3+ (TG) immune cell density by Tumour Clinical stage 
CD3TGTCSsum<- Nonlethal %>% group_by(`Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Turn into table add to graph
texttableCD3TGTCS <-ggtexttable(CD3TGTCSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
  theme=ttheme(base_size=5,  "classic"))
#Visualize the data: Create a box plot of the CD3+ (TS) density by Tumour Clinical Stage: 
CD3plotTCSTG <- ggplot(data=subset(Nonlethal, !is.na(`Tumour Clinical Stage`)), aes(x=`Tumour Clinical Stage`, y=`CD3+(TG)` ,fill=`Tumour Clinical Stage`)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis compare CD3+ (TG) density by Tumour Clinical Stage in 
CD3plotTCSTG<- CD3plotTCSTG + xlab("Tumour Clinical Stage") + ylab("Density of CD3+ immune cell phenotype") #Add title
CD3plotTCSTG<- CD3plotTCSTG + theme(legend.position = "top")

#CD3+ (TS) density and Tumour Clinical Stage: 
#Summary statistics: compute the median and IQR of CD3+ (TS) density by Tumour Clinical stage 
CD3TSTCSsum<- Nonlethal %>% group_by(`Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+ (TS) density by Tumour Clinical Stage: 
CD3plotTCSTS <- ggplot(data=subset(Nonlethal, !is.na(`Tumour Clinical Stage`)), aes(x=`Tumour Clinical Stage`, y=`CD3+(TS)` ,fill=`Tumour Clinical Stage`)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis compare CD3+ (TS) density by Tumour Clinical Stage in 
CD3plotTCSTS<- CD3plotTCSTS + xlab("Tumour Clinical Stage") + ylab("Density of CD3+ immune cell phenotype") #Add title
CD3plotTCSTS <- CD3plotTCSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSTCS <-ggtexttable(CD3TSTCSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme(base_size=5,"classic"))


#CD3+CD4+ (TG) density and Tumour Clinical Stage: 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TG) density by Tumour Clinical stage 
CD3CD4TGTCSSum<- Nonlethal %>% group_by(`Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+CD4+ (TG) density by Tumour Clinical Stage: 
CD3CD4plotTCSTG <- ggplot(data=subset(Nonlethal, !is.na(`Tumour Clinical Stage`)), aes(x=`Tumour Clinical Stage`, y=`CD3+CD4+(TG)` ,fill=`Tumour Clinical Stage`)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis compare CD3+CD4+ (TG) density by Tumour Clinical Stage in 
CD3CD4plotTCSTG<- CD3CD4plotTCSTG + xlab("Tumour Clinical Stage") + ylab("Density of CD3+CD4+ immune cell phenotype") #Add title
CD3CD4plotTCSTG <- CD3CD4plotTCSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGTCS <-ggtexttable(CD3CD4TGTCSSum, rows=NULL, cols=c("Tumour Clinical Stage", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme(base_size=5,"classic"))

#CD3+CD4+ (TS) density and Tumour Clinical Stage: 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TS) density by Tumour Clinical stage 
CD3CD4TCSTSsum<- Nonlethal %>% group_by(`Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+CD4+ (TS) density by Tumour Clinical Stage: 
CD3CD4plotTCSTS <- ggplot(data=subset(Nonlethal, !is.na(`Tumour Clinical Stage`)), aes(x=`Tumour Clinical Stage`, y=`CD3+CD4+(TS)` ,fill=`Tumour Clinical Stage`)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis compare CD3+CD4+ (TS) density by Tumour Clinical Stage in 
CD3CD4plotTCSTS<- CD3CD4plotTCSTS + xlab("Tumour Clinical Stage") + ylab("Density of CD3+CD4+ immune cell phenotype") #Add title
CD3CD4plotTCSTS<- CD3CD4plotTCSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSTCS <-ggtexttable(CD3CD4TCSTSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size=5,"classic"))


#CD68+ (TG) density and Tumour Clinical Stage: 
#Summary statistics: compute the median and IQR of CD68+ (TG) density by Tumour Clinical stage 
CD68TGTCSsum<- Nonlethal %>% group_by(`Tumour Clinical Stage`) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD68+ (TG) density by Tumour Clinical Stage: 
CD68plotTCSTG <- ggplot(data=subset(Nonlethal, !is.na(`Tumour Clinical Stage`)), aes(x=`Tumour Clinical Stage`, y=`CD68+(TG)` ,fill=`Tumour Clinical Stage`)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis compare CD68+ (TG) density by Tumour Clinical Stage in 
CD68plotTCSTG <- CD68plotTCSTG + xlab("Tumour Clinical Stage") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotTCSTG <- CD68plotTCSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGTCS <-ggtexttable(CD68TGTCSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size=5,"classic"))


#CD68+ (TS) density and Tumour Clinical Stage: 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TS) density by Tumour Clinical stage 
CD68TSTCSSum<- Nonlethal %>% group_by(`Tumour Clinical Stage`) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD68+ (TS) density by Tumour Clinical Stage: 
CD68plotTCSTS <- ggplot(data=subset(Nonlethal, !is.na(`Tumour Clinical Stage`)), aes(x=`Tumour Clinical Stage`, y=`CD68+(TS)` ,fill=`Tumour Clinical Stage`)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis compare CD68+ (TS) density by Tumour Clinical Stage in 
CD68plotTCSTS <- CD68plotTCSTS + xlab("Tumour Clinical Stage") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotTCSTS <- CD68plotTCSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSTCS <-ggtexttable(CD68TSTCSSum, rows=NULL, cols=c("Tumour Clinical Stage", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                 theme=ttheme(base_size=5,"classic"))

##Gleason Score/Grade##

#CD3+ (TG) density and Gleason Score/Grade: 
#Summary statistics: compute the median and IQR of CD3+ (TG) density by GS_Status
CD3TGGSsum<- Nonlethal %>% group_by(GS_status) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+ (TG) density by GS_Status 
CD3plotGS <- ggplot(Nonlethal, aes(x=GS_status, y=`CD3+(TG)`, fill=GS_status)) +geom_boxplot() + stat_compare_means() #run Wilcox compare CD3+ (TG) density by GS_status  
CD3plotGS<- CD3plotGS + xlab("Gleason Score") + ylab("Density of CD3+ immune cell phenotype") + labs(fill="Gleason Score") #Add title
CD3plotGS <- CD3plotGS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGGS <-ggtexttable(CD3TGGSsum, rows=NULL, cols=c("Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                 theme=ttheme("classic"))

#CD3+ (TS) density and Gleason Score/Grade: 
#Summary statistics: compute the median and IQR of CD3+ (TS) density by GS_Status
CD3TSGSSum<- Nonlethal %>% group_by(GS_status) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+ (TS) density by GS_Status 
CD3plotGSTS <- ggplot(Nonlethal, aes(x=GS_status, y=`CD3+(TS)`, fill=GS_status)) +geom_boxplot() + stat_compare_means() #run Wilcox compare CD3+ (TS) density by GS_status  
CD3plotGSTS <-  CD3plotGSTS+ xlab("Gleason Score") + ylab("Density of CD3+ immune cell phenotype") #Add title
CD3plotGSTS <-  CD3plotGSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSGS <-ggtexttable(CD3TSGSSum, rows=NULL, cols=c("Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                               theme=ttheme("classic"))

#CD3+CD4+ (TG) density and Gleason Score/Grade: 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TG) density by GS_Status
CD3CD4TGGSSum <- Nonlethal %>% group_by(GS_status) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+CD4+ (TG) density by GS_Status 
CD3CD4plotGSTG <- ggplot(Nonlethal, aes(x=GS_status, y=`CD3+CD4+(TG)`, fill=GS_status)) +geom_boxplot() + stat_compare_means() #run Wilcox compare CD3+CD4+ (TG) density by GS_status 
CD3CD4plotGSTG <-  CD3CD4plotGSTG  +xlab("Gleason Score") + ylab("Density of CD3+CD4+ immune cell phenotype") #Add title
CD3CD4plotGSTG <- CD3CD4plotGSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGGS <-ggtexttable(CD3CD4TGGSSum, rows=NULL, cols=c("Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                               theme=ttheme("classic"))

#CD3+CD4+ (TS) density and Gleason Score/Grade: 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TS) density by GS_Status
CD3CD4TSGSsum<- Nonlethal %>% group_by(GS_status) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+ (TS) density by GS_Status 
CD3CD4plotGSTS <- ggplot(Nonlethal, aes(x=GS_status, y=`CD3+CD4+(TS)`, fill=GS_status)) +geom_boxplot() + stat_compare_means() #run Wilcox compare CD3+Cd4+ (TS) density by GS_status 
CD3CD4plotGSTS <- CD3CD4plotGSTS+ xlab("Gleason Score") + ylab("Density of CD3+CD4+ immune cell phenotype")  #Add title
CD3CD4plotGSTS <- CD3CD4plotGSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSGS <-ggtexttable(CD3CD4TSGSsum, rows=NULL, cols=c("Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme("classic"))

#CD68+ (TG) density and GS  
#Summary statistics: compute the median and IQR of CD68+ (TG) density by GS_Status
CD68TGGSsum<- Nonlethal %>% group_by(GS_status) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+CD4+ (TG) density by GS_Status 
CD68plotGSTG <- ggplot(Nonlethal, aes(x=GS_status, y=`CD68+(TG)`, fill=GS_status)) +geom_boxplot() + stat_compare_means() #run Wilcox compare CD68+ (TG) density by GS_status 
CD68plotGSTG <- CD68plotGSTG + xlab("Gleason Score") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotGSTG <- CD68plotGSTG+ theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGGS <-ggtexttable(CD68TGGSsum, rows=NULL, cols=c("Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme("classic"))

#CD68+ (TS) density and GS 
#Summary statistics: compute the median and IQR of CD68+ (TS) density by GS_Status
CD68TSGSsum<- Nonlethal %>% group_by(GS_status) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD68+ (TS) density by GS_Status 
CD68plotGSTS <- ggplot(Nonlethal, aes(x=GS_status, y=`CD68+(TS)`, fill=GS_status)) +geom_boxplot() + stat_compare_means() #run Wilcox compare CD68+ (TS) density by GS_status 
CD68plotGSTS <- CD68plotGSTS + xlab("Gleason Score") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotGSTS <- CD68plotGSTS +  theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSGS <-ggtexttable(CD68TSGSsum, rows=NULL, cols=c("Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))

#Perform/Estimating risk ratio and risk differences using regression in Subcohort (look if immune cell density influence the risk of TCS and GS)

#Load package
library(risks)
library(rifttable)
library(dplyr)
library(broom)

#Risk ratio (RR): Immune cell density and Gleason Score/Grade (During this analysis we will uses the continuous variable and categorical variable)

#initial we need to set GS_status binary outcome at 1  (i.e., binary outcome 4+3 ) 
Nonlethal <- Nonlethal %>%  mutate(GS_binaryoutcome = case_when(
  `Gleason primary pattern` == 3 ~ 0, 
  `Gleason primary pattern` == 4 ~ 1
))

#Perform RR analysis: CD3+(TG) [> or < median] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3TGCatGS_status <- riskratio(formula = GS_binaryoutcome ~ CD3positivemedianTG, data = Nonlethal)  
summary(fitRRCD3TGCatGS_status) #Summarize the results
tidy(fitRRCD3TGCatGS_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3TGdensityGS_status <- riskratio(formula = GS_binaryoutcome ~ `CD3+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TGdensityGS_status) #summarize the results
tidy(fitRRCD3TGdensityGS_status, exponentiate = T) #access the models coefficeints 

#Perform RR analysis: CD3+(TS) [> or < median] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3TSCatGS_status <- riskratio(formula = GS_binaryoutcome ~ CD3positivemedianTS, data = Nonlethal)  
summary(fitRRCD3TSCatGS_status) #Summarize the results
tidy(fitRRCD3TSCatGS_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3TIQRensityGS_status <- riskratio(formula = GS_binaryoutcome ~ `CD3+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TIQRensityGS_status) #summarize the results
tidy(fitRRCD3TIQRensityGS_status, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3CD4TGCatGS_status <- riskratio(formula = GS_binaryoutcome ~ CD3CD4positivemedianTG, data = Nonlethal)  
summary(fitRRCD3CD4TGCatGS_status) #Summarise the results
tidy(fitRRCD3CD4TGCatGS_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3CD4TGdensityGS_status <- riskratio(formula = GS_binaryoutcome ~ `CD3+CD4+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TGdensityGS_status) #sumrise the results
tidy(fitRRCD3CD4TGdensityGS_status, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3CD4TSCatGS_status <- riskratio(formula = GS_binaryoutcome ~ CD3CD4positivemedianTS, data = Nonlethal)  
summary(fitRRCD3CD4TSCatGS_status) #Summarise the results
tidy(fitRRCD3CD4TSCatGS_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and GS_Status (i.e., binary outcome 4+3)
fitRRCD3CD4TIQRensityGS_status <- riskratio(formula = GS_binaryoutcome ~ `CD3+CD4+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TIQRensityGS_status) #summarize the results 
tidy(fitRRCD3CD4TIQRensityGS_status, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and GS_Status (i.e., binary outcome 4+3)
fitRRCD68TGCatGS_status <- riskratio(formula = GS_binaryoutcome ~ CD68positivemedianTG, data = Nonlethal)  
summary(fitRRCD68TGCatGS_status) #Summarise the results
tidy(fitRRCD68TGCatGS_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and GS_Status (i.e., binary outcome 4+3)
fitRRCD68TGdensityGS_status <- riskratio(formula = GS_binaryoutcome ~ `CD68+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TGdensityGS_status) #sumrise the results
tidy(fitRRCD68TGdensityGS_status, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and GS_Status (i.e., binary outcome 4+3)
fitRRCD68TSCatGS_status <- riskratio(formula = GS_binaryoutcome ~ CD68positivemedianTS, data = Nonlethal)  
summary(fitRRCD68TSCatGS_status) #Summarise the results
tidy(fitRRCD68TSCatGS_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and GS_Status (i.e., binary outcome 4+3)
fitRRCD68TIQRensityGS_status <- riskratio(formula = GS_binaryoutcome ~ `CD68+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TIQRensityGS_status) #summarize the results 
tidy(fitRRCD68TIQRensityGS_status, exponentiate = T) #access the models coefficient

#RR: Immune cell density and Tumour Clinical Stage 

#Subset out ST1 and ST2 patients 
ST1ST2patients <-subset(Nonlethal, `Tumour Clinical Stage`=="ST1" | `Tumour Clinical Stage`=="ST2")
#Subset out the ST1 and ST3 patients
ST1ST3patients <- subset(Nonlethal, `Tumour Clinical Stage`=="ST1" | `Tumour Clinical Stage`=="ST3")

#Factor Tumour Clinical stage and add labels 
ST1ST2patients$`Tumour Clinical Stage` <- factor(ST1ST2patients$`Tumour Clinical Stage`, labels = c("ST1", "ST2"))
ST1ST3patients$`Tumour Clinical Stage` <- factor(ST1ST3patients$`Tumour Clinical Stage`, labels= c("ST1", "ST3"))

#Perform RR analysis: CD3+(TG) [> or < median] and TCS (i.e., binary outcome ST2)
fitRRCD3TGCatTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3positivemedianTG, data= ST1ST2patients)
summary(fitRRCD3TGCatTCS_ST2) #Summarize the results
tidy(fitRRCD3TGCatTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+(TG) [> or < median] and TCS (i.e., binary outcome ST3)
fitRRCD3TGCatTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3positivemedianTG, data= ST1ST3patients)
summary(fitRRCD3TGCatTCS_ST3) #Summarize the results
tidy(fitRRCD3TGCatTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+(TG) [> or < median] [continuous] and TCS (i.e., binary outcome ST2) 
fitRRCD3TGdensityTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+(TG)`, data= ST1ST2patients, approach= "logistic")
summary(fitRRCD3TGdensityTCS_ST2) #Summarize the results
tidy(fitRRCD3TGdensityTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+(TG) [> or < median] [continuous] and TCS (i.e., binary outcome ST3) 
fitRRCD3TGdensityTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+(TG)`, data= ST1ST3patients, approach= "logistic")
summary(fitRRCD3TGdensityTCS_ST3) #Summarize the results
tidy(fitRRCD3TGdensityTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+(TS) [> or < median] and TCS (i.e., binary outcome ST2)
fitRRCD3TSCatTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3positivemedianTS, data= ST1ST2patients)
summary(fitRRCD3TSCatTCS_ST2) #Summarize the results
tidy(fitRRCD3TSCatTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+(TS) [> or < median] and TCS (i.e., binary outcome ST3)
fitRRCD3TSCatTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3positivemedianTS, data= ST1ST3patients)
summary(fitRRCD3TSCatTCS_ST3) #Summarize the results
tidy(fitRRCD3TSCatTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+(TS) [> or < median] [continuous] and TCS (i.e., binary outcome ST2) 
fitRRCD3TIQRensityTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+(TS)`, data= ST1ST2patients, approach= "logistic")
summary(fitRRCD3TIQRensityTCS_ST2) #Summarize the results
tidy(fitRRCD3TIQRensityTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+(TS) [> or < median] [continuous] and TCSM (i.e., binary outcome ST3) 
fitRRCD3TIQRensityTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+(TS)`, data= ST1ST3patients, approach= "logistic")
summary(fitRRCD3TIQRensityTCS_ST3) #Summarize the results
tidy(fitRRCD3TIQRensityTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#RR: Estimating RR for CD3+CD4+ on predicting the risk of TCS: 

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and TCS (i.e., binary outcome ST2)
fitRRCD3CD4TGCatTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3CD4positivemedianTG, data= ST1ST2patients)
summary(fitRRCD3CD4TGCatTCS_ST2) #Summarize the results
tidy(fitRRCD3CD4TGCatTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and TCS (i.e., binary outcome ST3)
fitRRCD3CD4TGCatTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3CD4positivemedianTG, data= ST1ST3patients)
summary(fitRRCD3CD4TGCatTCS_ST3) #Summarize the results
tidy(fitRRCD3CD4TGCatTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+CD4+(TG) [> or < median] [continuous] and TCS (i.e., binary outcome ST2) 
fitRRCD3CD4TGdensityTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+CD4+(TG)`, data= ST1ST2patients, approach= "logistic")
summary(fitRRCD3CD4TGdensityTCS_ST2) #Summarize the results
tidy(fitRRCD3CD4TGdensityTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+CD4+(TG) [> or < median] [continuous] and TCS (i.e., binary outcome ST3) 
fitRRCD3CD4TGdensityTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+CD4+(TG)`, data= ST1ST3patients, approach= "logistic")
summary(fitRRCD3CD4TGdensityTCS_ST3) #Summarize the results
tidy(fitRRCD3CD4TGdensityTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and TCS (i.e., binary outcome ST2)
fitRRCD3CD4TSCatTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3CD4positivemedianTS, data= ST1ST2patients)
summary(fitRRCD3CD4TSCatTCS_ST2) #Summarize the results
tidy(fitRRCD3CD4TSCatTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and TCS (i.e., binary outcome ST3)
fitRRCD3CD4TSCatTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ CD3CD4positivemedianTS, data= ST1ST3patients)
summary(fitRRCD3CD4TSCatTCS_ST3) #Summarize the results
tidy(fitRRCD3CD4TSCatTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] [continuous] and TCS (i.e., binary outcome ST2) 
fitRRCD3CD4TIQRensityTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+CD4+(TS)`, data= ST1ST2patients, approach= "logistic")
summary(fitRRCD3CD4TIQRensityTCS_ST2) #Summarize the results
tidy(fitRRCD3CD4TIQRensityTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] [continuous] and TCSM (i.e., binary outcome ST3) 
fitRRCD3CD4TIQRensityTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD3+CD4+(TS)`, data= ST1ST3patients, approach= "logistic")
summary(fitRRCD3CD4TIQRensityTCS_ST3) #Summarize the results
tidy(fitRRCD3CD4TIQRensityTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#RR: Estimating RR for CD68+ on predicting the risk of TCS:

#Perform RR analysis: CD68+(TG) [> or < median] and TCS (i.e., binary outcome ST2)
fitRRCD68TGCatTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ CD68positivemedianTG, data= ST1ST2patients)
summary(fitRRCD68TGCatTCS_ST2) #Summarize the results
tidy(fitRRCD68TGCatTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD68+(TG) [> or < median] and TCS (i.e., binary outcome ST3)
fitRRCD68TGCatTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ CD68positivemedianTG, data= ST1ST3patients)
summary(fitRRCD68TGCatTCS_ST3) #Summarize the results
tidy(fitRRCD68TGCatTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD68+(TG) [> or < median] [continuous] and TCS (i.e., binary outcome ST2) 
fitRRCD68TGdensityTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD68+(TG)`, data= ST1ST2patients, approach= "logistic")
summary(fitRRCD68TGdensityTCS_ST2) #Summarize the results
tidy(fitRRCD68TGdensityTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD68+(TG) [> or < median] [continuous] and TCS (i.e., binary outcome ST3) 
fitRRCD68TGdensityTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD68+(TG)`, data= ST1ST3patients, approach= "logistic")
summary(fitRRCD68TGdensityTCS_ST3) #Summarize the results
tidy(fitRRCD68TGdensityTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD68+(TS) [> or < median] and TCS (i.e., binary outcome ST2)
fitRRCD68TSCatTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ CD68positivemedianTS, data= ST1ST2patients)
summary(fitRRCD68TSCatTCS_ST2) #Summarize the results
tidy(fitRRCD68TSCatTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD68+(TS) [> or < median] and TCS (i.e., binary outcome ST3)
fitRRCD68TSCatTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ CD68positivemedianTS, data= ST1ST3patients)
summary(fitRRCD68TSCatTCS_ST3) #Summarize the results
tidy(fitRRCD68TSCatTCS_ST3, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD68+(TS) [> or < median] [continuous] and TCS (i.e., binary outcome ST2) 
fitRRCD68TIQRensityTCS_ST2 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD68+(TS)`, data= ST1ST2patients, approach= "logistic")
summary(fitRRCD68TIQRensityTCS_ST2) #Summarize the results
tidy(fitRRCD68TIQRensityTCS_ST2, exponentiate = TRUE) #access the models coefficient 

#Perform RR analysis: CD68+(TS) [> or < median] [continuous]  and TCS (i.e., binary outcome ST3) 
fitRRCD68TIQRensityTCS_ST3 <- riskratio(formula = `Tumour Clinical Stage` ~ `CD68+(TS)`, data= ST1ST3patients, approach= "logistic")
summary(fitRRCD68TIQRensityTCS_ST3) #Summarize the results
tidy(fitRRCD68TIQRensityTCS_ST3, exponentiate = TRUE) #access the models coefficient 


###Are immune cell densities altered in lethal vs. non-lethal PCa cases? Perform a unpaired Wilcox test to determine if the density of immune cell are associated with cohort status (i.e., lethal PCa/non-lethal PCa)

#CD3+ Immune Cell Phenotype

#perform a Wilcox test to determine if CD3+ (TG) immune cell density is associated with cohort status 
CD3TG <- wilcox.test(NI.study$`CD3+(TG)`~ NI.study$Cohort)  
CD3TG #p-value = 0.2911 (not significant)
#Summary statistics: compute the median and IQR of CD3+ (TG) density grouped by Cohort  
CD3TGLNsum <- NI.study %>% group_by(Cohort) %>%
  get_summary_stats(`CD3+(TG)`, type= "median_iqr")
#we will create a Boxplot compare CD3+ (TG) immune cell density: lethal vs non-lethal PCa
bpCD3TG <-  ggplot(NI.study, aes(x=Cohort, y= `CD3+(TG)`, fill= Cohort)) +geom_boxplot() 
bpCD3TG <- bpCD3TG + xlab("Cohort") + ylab("Density of CD3+ immune cell phenotype") + stat_compare_means() # Add title and Wilcox test result (i.e., p-value) 
bpCD3TG <- bpCD3TG +theme(legend.position = "top")
#turn into table and add to graph
texttableCD3TGLN <-ggtexttable(CD3TGLNsum, rows=NULL, cols=c("Cohort", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                               theme=ttheme("classic"))


#perform a wilcox test to determine if CD3+ (Tumour Stroma) immune cell density is associated with cohort status 
CDTS <- wilcox.test(NI.study$`CD3+(TS)`~NI.study$Cohort) 
CDTS #p-value = 0.02321 (significant)
#Summary statistics: compute the median and IQR of CD3+ (TS) density grouped by Cohort  
CD3TSLNsum<- NI.study %>% group_by(Cohort) %>%
  get_summary_stats(`CD3+(TS)`, type= "median_iqr")
#We will create a Boxplot comparing the CD3+ (TS) immune cell density: lethal PCa vs non-lethal PCa
bpCD3TS<- ggplot(NI.study, aes(x=Cohort, y= `CD3+(TS)`, fill= Cohort)) +geom_boxplot() 
bpCD3TS <- bpCD3TS + xlab("Cohort") + ylab("Density of CD3+ immune cell phenotype") + stat_compare_means() # Add title and Wilcox test result (i.e., p-value) 
bpCD3TS<- bpCD3TS + theme(legend.position = "top")
#turn into table and add to graph
texttableCD3LNTS <-ggtexttable(CD3TSLNsum, rows=NULL, cols=c("Cohort", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                             theme=ttheme("classic"))


#CD3+CD4+ Immune Cell Phenotype

#perform a Wilcox test to determine if CD3+CD4+ (Tumour Gland) immune cell density is associated with cohort status 
CD3CD4TG<- wilcox.test(NI.study$`CD3+CD4+(TG)`~NI.study$Cohort) 
CD3CD4TG #p-value = 0.0331 (significant)
#Summary statistics: compute the median and IQR of CD3+CD4+ (TG) density by Cohort 
CD3CD4TGLNsum<- NI.study %>% group_by(Cohort) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#We will create a Boxplot comparing the CD3+CD4+ Immune Cell density: lethal PCa vs non-lethal PCa
bpCD3CD4TG <- ggplot(NI.study, aes(x=Cohort, y= `CD3+CD4+(TG)`, fill=Cohort)) + geom_boxplot()
bpCD3CD4TG <- bpCD3CD4TG + xlab("Cohort") + ylab("Density of CD3+CD4+ immune cell phenotype") + stat_compare_means() # Add title and Wilcox test result (i.e., p-value) 
bpCD3CD4TG <- bpCD3CD4TG +  theme(legend.position = "top")
#turn into table and add to graph
texttableCD3CD4TGLN <-ggtexttable(CD3CD4TGLNsum, rows=NULL, cols=c("Cohort", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                               theme=ttheme("classic"))

#perform a wilcox test to determine if CD3+CD4+ (TS) immune cell density is associated with cohort status 
CD3CD4TS <- wilcox.test(NI.study$`CD3+CD4+(TS)`~ NI.study$Cohort) 
CD3CD4TS #p-value = 0.03124 (significant)
#Summary statistics: compute the median and IQR of CD3+CD4+ (TS) density by Cohort 
CD3CD4TSLNsum<- NI.study %>% group_by(Cohort) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#We will create a Boxplot comparing the CD3+CD4+ Immune Cell density: lethal PCa vs non-lethal PCa
bpCD3CD4TS <- ggplot(NI.study, aes(x=Cohort, y= `CD3+CD4+(TS)`, fill=Cohort)) + geom_boxplot()
bpCD3CD4TS<-bpCD3CD4TS  + xlab("Cohort") + ylab("Density of CD3+CD4+ immune cell phenotype") + stat_compare_means() # Add title and Wilcox test result (i.e., p-value) 
bpCD3CD4TS <- bpCD3CD4TS + theme(legend.position = "top")
#turn into table and add to graph
texttableCD3CD4TSLN <-ggtexttable(CD3CD4TSLNsum, rows=NULL, cols=c("Cohort", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme("classic"))


#CD68+ Immune Cell Phenotype

#perform a wilcox test to determine if CD68+ (TG) immune cell density is associated with cohort status
CD68TG<- wilcox.test(NI.study$`CD68+(TG)`~NI.study$Cohort) 
CD68TG #p-value = 0.02946 (significant)
#Summary statistics: compute the median and IQR of CD68+ (TG) immune cell density by Cohort 
CD68TGLNsum<- NI.study %>% group_by(Cohort) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#We will create a Boxplot comparing the CD68+ Immune cell density: Lethal PCa vs non-lethal PCa
bpCD68TG <- ggplot(NI.study, aes(x=Cohort, y= `CD68+(TG)`, fill=Cohort)) + geom_boxplot()
bpCD68TG <- bpCD68TG + xlab("Cohort") + ylab("Density of CD68+ immune cell phenotype") + stat_compare_means() # Add title and Wilcox test result (i.e., p-value) 
bpCD68TG <- bpCD68TG + theme(legend.position = "top")
#turn into table and add to graph
texttableCD68TGLN <-ggtexttable(CD68TGLNsum, rows=NULL, cols=c("Cohort", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme("classic"))

#perform a wilcox test to determine if CD68+ (TS) immune cell density is associated with cohort status
CD68TS <- wilcox.test(NI.study$`CD68+(TS)`~NI.study$Cohort)
CD68TS #p-value = 0.002658 (significant)
#Summary statistics: compute the median and IQR of CD68+ (TS) density by Cohort 
CD68TSLNsum<- NI.study %>% group_by(Cohort) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#We will create a Boxplot comparing the CD68+(TS) Immune cell density: Lethal PCa vs non-lethal PCa
bpCD68TS <- ggplot(NI.study, aes(x=Cohort, y= `CD68+(TS)`, fill=Cohort)) + geom_boxplot() 
bpCD68TS<- bpCD68TS  + xlab("Cohort") + ylab("Density of CD68+ immune cell phenotype") + stat_compare_means() # Add title and Wilcox test result (i.e., p-value) 
bpCD68TS <- bpCD68TS + theme(legend.position = "top")
#turn into table and add to graph
texttableCD68TSLN <-ggtexttable(CD68TSLNsum, rows=NULL, cols=c("Cohort", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))



#Are immune cell density associated with outcomes?

#Load R packages 
library("tidyverse")
library("survival")
library("survminer")
library(MASS)
library(rms)

#Initial want to summaries the Events based on Immune defined subtypes groups for each phenotype (i.e., > or < median)

#Summarize OS Event and Immune defined groups 
table(NI.study$CD3positivemedianTG) #CD3+ TG 
table(NI.study$CD3positivemedianTG, NI.study$EventOS) #CD3+ TG plus eventOS
table(NI.study$CD3positivemedianTS) #CD3+ TS
table(NI.study$CD3positivemedianTS, NI.study$EventOS) #CD3+ TS plus eventOS
table(NI.study$CD3CD4positivemedianTG) #CD3+CD4+ TG 
table(NI.study$CD3CD4positivemedianTG, NI.study$EventOS) #CD3+CD4+ TG plus eventOS
table(NI.study$CD3CD4positivemedianTS) #CD3+CD4+ TS
table(NI.study$CD3CD4positivemedianTS, NI.study$EventOS) #CD3+CD4+ TS plus eventOS
table(NI.study$CD68positivemedianTG) #CD68+ TG 
table(NI.study$CD68positivemedianTG, NI.study$EventOS) #CD68+ TG plus eventOS
table(NI.study$CD68positivemedianTS) #CD68+ TS
table(NI.study$CD68positivemedianTS, NI.study$EventOS) #CD68+ TS plus eventOS

#Summarize PCa-specific survival and Immune defined groups 
table(NI.study$CD3positivemedianTG) #CD3+ TG 
table(NI.study$CD3positivemedianTG, NI.study$EventLPCA) #CD3+ TG plus EventLPCA
table(NI.study$CD3positivemedianTS) #CD3+ TS
table(NI.study$CD3positivemedianTS, NI.study$EventLPCA) #CD3+ TS plus EventLPCA
table(NI.study$CD3CD4positivemedianTG) #CD3+CD4+ TG 
table(NI.study$CD3CD4positivemedianTG, NI.study$EventLPCA) #CD3+CD4+ TG plus EventLPCA
table(NI.study$CD3CD4positivemedianTS) #CD3+CD4+ TS
table(NI.study$CD3CD4positivemedianTS, NI.study$EventLPCA) #CD3+CD4+ TS plus EventLPCA
table(NI.study$CD68positivemedianTG) #CD68+ TG 
table(NI.study$CD68positivemedianTG, NI.study$EventLPCA) #CD68+ TG plus EventLPCA
table(NI.study$CD68positivemedianTS) #CD68+ TS
table(NI.study$CD68positivemedianTS, NI.study$EventLPCA) #CD68+ TS plus EventLPCA

attach(NI.study) #Attach to allow access to columns directly

#Initial perform a Coxph Survival fro OS and PCa-specific survival univariate and we need to check it meets the assumptions 

#Univarate Cox PH model: 

#Perform Coxph on OS and CD3+ TG (categorical)
coxphfitOSCD3TG <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventOS)~CD3positivemedianTG , method="efron")
#Check PH assumption 
coxphfitOSCD3TG_PH <- cox.zph(coxphfitOSCD3TG, transform= "log")
coxphfitOSCD3TG_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+ (TG) </> median (i.e., Check PH assumption)
plot(coxphfitOSCD3TG_PH) 

#Perform Coxph on OS and CD3+ TS (categorical)
coxphfitOSCD3TS <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventOS)~CD3positivemedianTS , method="efron")
#Check PH assumption 
coxphfitOSCD3TS_PH <- cox.zph(coxphfitOSCD3TS, transform= "log")
coxphfitOSCD3TS_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+ (TS) </> median (i.e., Check PH assumption)
plot(coxphfitOSCD3TS_PH) 

#Perform Coxph on OS and CD3+CD4+ TG (categorical)
coxphfitOSCD3CD4TG <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventOS)~CD3CD4positivemedianTG  , method="efron")
#Check PH assumption 
coxphfitOSCD3CD4TG_PH <- cox.zph(coxphfitOSCD3CD4TG, transform= "log")
coxphfitOSCD3CD4TG_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+CD4+ (TG) </> median (i.e., Check PH assumption)
plot(coxphfitOSCD3CD4TG_PH) 

#Perform Coxph on OS and CD3+CD4+ TS (categorical)
coxphfitOSCD3CD4TS <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventOS)~CD3CD4positivemedianTS , method="efron")
#Check PH assumption 
coxphfitOSCD3CD4TS_PH <- cox.zph(coxphfitOSCD3CD4TS, transform= "log")
coxphfitOSCD3CD4TS_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+Cd4+ (TS) </> median (i.e., Check PH assumption)
plot(coxphfitOSCD3CD4TS_PH) 

#Perform Coxph on OS and CD68+ TG (categorical)
coxphfitOSCD68TG <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventOS)~CD68positivemedianTG , method="efron")
#Check PH assumption 
coxphfitOSCD68TG_PH <- cox.zph(coxphfitOSCD68TG, transform= "log")
coxphfitOSCD68TG_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD68+ (TG) </> median (i.e., Check PH assumption)
plot(coxphfitOSCD68TG_PH) 

#Perform Coxph on OS and CD68+ TS (categorical)
coxphfitOSCD68TS <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventOS)~CD68positivemedianTS , method="efron")
#Check PH assumption 
coxphfitOSCD68TS_PH <- cox.zph(coxphfitOSCD68TS, transform= "log")
coxphfitOSCD68TS_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD68+ (TS) </> median (i.e., Check PH assumption)
plot(coxphfitOSCD68TS_PH) 

#Perform Coxph OS mulivaratie model (ie., includes all immune cell phenotypes)
coxfitmultivariateOS <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD68positivemedianTS + CD3positivemedianTG + CD3positivemedianTS + 
                                CD3CD4positivemedianTG +CD3CD4positivemedianTS +CD68positivemedianTG+ Age.at.diagnosis, method="efron")
coxfitmultivariateOS_PH <- cox.zph(coxfitmultivariateOS, transform = "log") 
coxfitmultivariateOS_PH #View assumption check 

#PCA-specific Survival: 

#Perform Coxph PCa-specific Survival  and CD3+ TG (categorical)
coxphfitLPCACD3TG <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventLPCA)~CD3positivemedianTG , method="efron")
#Check PH assumption 
coxphfitLPCACD3TG_PH <- cox.zph(coxphfitLPCACD3TG, transform= "log")
coxphfitLPCACD3TG_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+ (TG) </> median (i.e., Check PH assumption)
plot(coxphfitLPCACD3TG_PH) 

#Perform Coxph PCa-specific Survival  and CD3+ TS (categorical)
coxphfitLPCACD3TS <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventLPCA)~CD3positivemedianTS , method="efron")
#Check PH assumption 
coxphfitLPCACD3TS_PH <- cox.zph(coxphfitLPCACD3TS, transform= "log")
coxphfitLPCACD3TS_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+ (TS) </> median (i.e., Check PH assumption)
plot(coxphfitLPCACD3TS_PH) 

#Perform Coxph PCa-specific Survival  and CD3+CD4+ TG (categorical)
coxphfitLPCACD3CD4TG <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventLPCA)~CD3CD4positivemedianTG , method="efron")
#Check PH assumption 
coxphfitLPCACD3CD4TG_PH <- cox.zph(coxphfitLPCACD3CD4TG, transform= "log")
coxphfitLPCACD3CD4TG_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+CD4+ (TG) </> median (i.e., Check PH assumption)
plot(coxphfitLPCACD3CD4TG_PH) 

#Perform Coxph PCa-specific Survival  and CD3+CD4+ TS (categorical)
coxphfitLPCACD3CD4TS <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventLPCA)~CD3CD4positivemedianTS , method="efron")
#Check PH assumption 
coxphfitLPCACD3CD4TS_PH <- cox.zph(coxphfitLPCACD3CD4TS, transform= "log")
coxphfitLPCACD3CD4TS_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD3+Cd4+ (TS) </> median (i.e., Check PH assumption)
plot(coxphfitLPCACD3CD4TS_PH) 

#Perform Coxph PCa-specific Survival  and CD68+ TG (categorical)
coxphfitLPCACD68TG <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventLPCA)~CD68positivemedianTG , method="efron")
#Check PH assumption 
coxphfitLPCACD68TG_PH <- cox.zph(coxphfitLPCACD68TG, transform= "log")
coxphfitLPCACD68TG_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD68+ (TG) </> median (i.e., Check PH assumption)
plot(coxphfitLPCACD68TG_PH) 

#Perform Coxph PCa-specific Survival  and CD68+ TS (categorical)
coxphfitLPCACD68TS <- coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,event=EventLPCA)~CD68positivemedianTS , method="efron")
#Check PH assumption 
coxphfitLPCACD68TS_PH <- cox.zph(coxphfitLPCACD68TS, transform= "log")
coxphfitLPCACD68TS_PH #View assumption check 
#plot Schoenfeld residuals versus log(time): CD68+ (TS) </> median (i.e., Check PH assumption)
plot(coxphfitLPCACD68TS_PH) 

#Perform Coxph PCa-specific survival on mulivaratie model (ie., includes all immune cell phenotypes)
coxfitmultivariatePCa <- coxph(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD68positivemedianTS + CD3positivemedianTG + CD3positivemedianTS + 
                                 CD3CD4positivemedianTG +CD3CD4positivemedianTS +CD68positivemedianTG+ Age.at.diagnosis, method="efron")
coxfitmultivariatePCa_PH <- cox.zph(coxfitmultivariatePCa, transform = "log") 
coxfitmultivariatePCa_PH #View assumption check 

##Perform AIC determine if the survival model needs to be adjusted for co-variables

#Remove columns with NA as AIC can't handles NA data
NI.study_NAomit <- NI.study[-c(6, 7, 17, 20, 25, 27, 29, 33, 47)]
NI.study_NAomit <- NI.study_NAomit %>% na.omit() #omit the NA values 
View(NI.study_NAomit)

#Overall Survival#

#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3TGfitOS <-coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,EventOS)~CD3positivemedianTG+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3TGfitOS, direction="backward")
#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3TSfitOS <-coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,EventOS)~CD3positivemedianTS+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3TSfitOS, direction="backward")

#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3CD4TGfitOS <-coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,EventOS)~CD3CD4positivemedianTG+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3CD4TGfitOS, direction="backward")
#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3CD4TSfitOS <-coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,EventOS)~CD3CD4positivemedianTS+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3CD4TSfitOS, direction="backward")

#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD68TGfitOS <-coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,EventOS)~CD68positivemedianTG+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD68TGfitOS, direction="backward")
#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD68TSfitOS <-coxph(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review.,EventOS)~CD68positivemedianTS+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD68TSfitOS, direction="backward")

#PCa-specific Survival#

#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3TGfitLPCA <-coxph(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up,EventLPCA)~CD3positivemedianTG+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3TGfitLPCA, direction="backward")
#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3TSfitLPCA <-coxph(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up,EventLPCA)~CD3positivemedianTS+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3TSfitLPCA, direction="backward")

#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3CD4TGfitLPCA <-coxph(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up,EventLPCA)~CD3CD4positivemedianTG+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3CD4TGfitLPCA, direction="backward")
#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD3CD4TSfitLPCA <-coxph(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up,EventLPCA)~CD3CD4positivemedianTS+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD3CD4TSfitLPCA, direction="backward")

#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD68TGfitLPCA <-coxph(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up,EventLPCA)~CD68positivemedianTG+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD68TGfitLPCA, direction="backward")
#Perform survival analysis initial to check which if the weighted cch survival needs to be adjusted for
CD68TSfitLPCA <-coxph(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up,EventLPCA)~CD68positivemedianTS+ factor(`Tumour Clinical Stage`)+ Age.at.diagnosis + GS_status +Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N +history_cardiovascular_disease +history_of_hypercholesterolemia+ history_of_diabetes+ factor(smoking_status) +factor(alcohol_consumption) +erg_cutpoint+ PTEN_status ,data=NI.study_NAomit)
#perform a stepwise model selection by AIC to determine if weighted survival needs to be adjusted for co-variables 
stepAIC(CD68TSfitLPCA, direction="backward")

###Now data has met the assumption of Cox PH run more approprate case-cohort survival analysis: 

##Perform Weighted Survival cch Analysis as case-cohort study need uses cch()

#OS survival and immune cell density (Categorical-defined >/< median) 

#Need to add column to define subcohort (i.e., non-lethal PCa = TRUE (apart of the sub-cohort) | lethal PCa cases = FALSE (not apart of subcohort)
NI.study <- NI.study %>% add_column(subcohort= case_when(
  startsWith(Cohort, "l")~ FALSE, 
  startsWith(Cohort, "n")~ TRUE,  
))    
#View data 
View(NI.study)

#Univariate OS Analysis: 

#univariate OS and CD3+ (TG) Immune cell density  
fitcchCD3TG <- fitcchOSCD3TG <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3positivemedianTG, data=NI.study, 
                                    subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchOSCD3TG)

#univariate OS and CD3+(TS) Immune cell density 
fitcchOSCD3TS <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3positivemedianTS , data=NI.study, 
                     subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchOSCD3TS)

#univariate OS and CD3+CD4+ (TG) Immune cell density 
fitcchOSCD3CD4TG <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3CD4positivemedianTG, data=NI.study, 
                        subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchOSCD3CD4TG) 

#univariate OS CD3+CD4+ (TS) Immune cell density 
fitcchOSCD3CD4TS <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3CD4positivemedianTS , data=NI.study, 
                        subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchOSCD3CD4TS) 

#Univariate OS and CD68+ (TG) Immune cell density  
fitcchOSCD68TG <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD68positivemedianTG , data=NI.study, 
                      subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchOSCD68TG) 

#Univariate OS and CD68+ (TS) Immune cell density 
fitcchOSCD68TS <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD68positivemedianTS, data=NI.study, 
                      subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchOSCD68TS) 

#Multivariate OS and Immunce cell density (adjusted for Age.at.diagnosis) 

#Multivariate OS and CD3+ (TG) Immune cell density  (adjusted for Age.at.diagnosis) 
MfitcchOSCD3TG <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3positivemedianTG +Age.at.diagnosis, data=NI.study, 
                      subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchOSCD3TG)

#Multivariate OS and CD3+(TS) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchOSCD3TS <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3positivemedianTS +Age.at.diagnosis , data=NI.study, 
                      subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchOSCD3TS)

#Multivariate OS and CD3+CD4+ (TG) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchOSCD3CD4TG <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3CD4positivemedianTG +Age.at.diagnosis, data=NI.study, 
                         subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchOSCD3CD4TG) 

#Multivariate OS and CD3+CD4+ (TS)  Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchOSCD3CD4TS <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD3CD4positivemedianTS +Age.at.diagnosis, data=NI.study, 
                         subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchOSCD3CD4TS) 

#Multivariate OS and CD68+ (TG) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchOSCD68TG <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD68positivemedianTG +Age.at.diagnosis, data=NI.study, 
                       subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchOSCD68TG) 

#Multivariate OS and CD68+ (TS) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchOSCD68TS <- cch(Surv(Days.from.diagnosis.to.death.or.day.of.last.follow.up..this.includes.last.PSA.recorded.or.last.oncology.review., EventOS) ~ CD68positivemedianTS +Age.at.diagnosis, data=NI.study, 
                       subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchOSCD68TS) 

#PCA-specific Survival


#univariate Lethal-PCA specific survival and CD3+ (TG) Immune cell density 
fitcchlethalPCACD3TG <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3positivemedianTG, data=NI.study, 
                            subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchlethalPCACD3TG) 

#Lethal-PCA specific survival and CD3+(TS)  Immune cell density 
fitcchlethalPCACD3TS <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3positivemedianTS , data=NI.study, 
                            subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchlethalPCACD3TS)

#lethal-PCA specific survival and CD3+CD4+ (TG) Immune cell density   
fitcchlethalPCACD3CD4TG <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3CD4positivemedianTG, data=NI.study, 
                               subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchlethalPCACD3CD4TG) 

#lethal-PCA specific survival and CD3+CD4+ (TS) Immune cell density   
fitcchlethalPCACD3CD4TS <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3CD4positivemedianTS, data=NI.study, 
                               subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchlethalPCACD3CD4TS) 

#lethal-PCA specific survival and CD68+ (TG) Immune cell density   
fitcchlethalPCACD68TG <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD68positivemedianTG , data=NI.study, 
                             subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchlethalPCACD68TG) 

#lethal-PCA specific survival and CD3+CD4+ (TS) Immune cell density 
fitcchlethalPCACD68TS <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD68positivemedianTS , data=NI.study, 
                             subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(fitcchlethalPCACD68TS) 

##multivariate PCA-specific survival and immune cell density##

#Multivariate Lethal-PCA specific survival and CD3+ (TG) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchlethalPCACD3TG <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3positivemedianTG +Age.at.diagnosis, data=NI.study, 
                             subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchlethalPCACD3TG) 

#Multivariate Lethal-PCA specific survival and CD3+(TS) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchlethalPCACD3TS <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3positivemedianTS +Age.at.diagnosis , data=NI.study, 
                             subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchlethalPCACD3TS)

#Multivariate lethal-PCA specific survival and CD3+CD4+ (TG) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchlethalPCACD3CD4TG <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3CD4positivemedianTG +Age.at.diagnosis, data=NI.study, 
                                subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchlethalPCACD3CD4TG) 

#Multivariate lethal-PCA specific survival and CD3+CD4+ (TS) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchlethalPCACD3CD4TS <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD3CD4positivemedianTS +Age.at.diagnosis, data=NI.study, 
                                subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchlethalPCACD3CD4TS) 

#Multivariate lethal-PCA specific survival and CD68+ (TG) Immune cell density (adjusted for Age.at.diagnosis) 
MfitcchlethalPCACD68TG <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD68positivemedianTG +Age.at.diagnosis, data=NI.study, 
                              subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchlethalPCACD68TG) 

#Multivariate lethal-PCA specific survival and CD3+CD4+ (TS) Immune cell density ((adjusted for Age.at.diagnosis)) 
MfitcchlethalPCACD68TS <- cch(Surv(Days.from.primary.diagnosis.to.PCa.death.or.metastasis.or.last.follow.up, EventLPCA) ~ CD68positivemedianTS +Age.at.diagnosis , data=NI.study, 
                              subcoh = ~subcohort, id=~NIB.Number, cohort.size = 117, method = "Prentice")  
summary(MfitcchlethalPCACD68TS) 

#####################################################################################################################################################################################
####Part Two: How does immune cell infiltrate correspond with prostate tumour biology?##########

#Are immune cell densities associated with tumour subgroups defined by PTEN and ERG? (Sub-Cohort)

#CD3+ (TG) immune cell density associated with PTEN status 
#Summary statistics: compute the median and IQR of CD3+ (TG) by PTEN_status 
CD3TGPTsum<- Nonlethal %>% group_by(PTEN_status) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+ (TG) immune cell density by PTEN_status  
CD3plotPTENLossTG <- ggplot(data=subset(Nonlethal, !is.na(PTEN_status)), aes(x=PTEN_status, y=`CD3+(TG)`, fill=PTEN_status)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+ density (TG) by PTEN_status intact vs null 
CD3plotPTENLossTG <- CD3plotPTENLossTG+ xlab("PTEN status") + ylab("Density of CD3+ immune cell phenotype") + labs(fill= "PTEN Status")#Add title
CD3plotPTENLossTG <- CD3plotPTENLossTG + theme(legend.position = "top")
#turn into table and add to graph
texttableCD3TGPT <-ggtexttable(CD3TGPTsum, rows=NULL, cols=c("PTEN status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                            theme=ttheme("classic"))

#CD3+ (TS) immune cell density associated with PTEN_status 
#Summary statistics: compute the median and IQR of CD3+ (TS) by PTEN_status 
CD3TSPTsum<- Nonlethal %>% group_by(PTEN_status) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+ (TS) immune cell density by PTEN_status  
CD3plotPTENLossTS <- ggplot(data=subset(Nonlethal, !is.na(PTEN_status)), aes(x=PTEN_status, y=`CD3+(TS)`, fill=PTEN_status)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+ density (TS) by PTEN_status intact vs null 
CD3plotPTENLossTS <- CD3plotPTENLossTS + xlab("PTEN status") + ylab("Density of CD3+ immune cell phenotype") #Add title
CD3plotPTENLossTS <- CD3plotPTENLossTS + theme(legend.position = "top")
#turn into table and add to graph
texttableCD3TSPT <-ggtexttable(CD3TSPTsum, rows=NULL, cols=c("PTEN status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                               theme=ttheme("classic"))

#CD3+CD4+ (TG) immune cell density and PTEN_status 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TG) by PTEN_status 
CD3CD4TGPTsum<- Nonlethal %>% group_by(PTEN_status) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+CD4+ (TG) immune cell density by PTEN_status  
CD3CD4plotPTENLossTG <- ggplot(data=subset(Nonlethal, !is.na(PTEN_status)), aes(x=PTEN_status, y=`CD3+CD4+(TG)`, fill=PTEN_status)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+ density (TG) by PTEN_status intact vs null 
CD3CD4plotPTENLossTG <- CD3CD4plotPTENLossTG + xlab("PTEN status") + ylab("Density of CD3+CD4+ immune cell phenotype") #Add title
CD3CD4plotPTENLossTG<- CD3CD4plotPTENLossTG + theme(legend.position = "top")
#turn into table and add to graph
texttableCD3CD4TGPT <-ggtexttable(CD3CD4TGPTsum, rows=NULL, cols=c("PTEN status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                               theme=ttheme("classic"))

#CD3+CD4+ (TS) immune cell density and PTEN_status 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TS) by PTEN_status 
CD3CD4TSPTsum<- Nonlethal %>% group_by(PTEN_status) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+CD4+ (TS) immune cell density by PTEN_status
CD3CD4plotPTENLossTS <- ggplot(data=subset(Nonlethal, !is.na(PTEN_status)), aes(x=PTEN_status, y=`CD3+CD4+(TS)`, fill=PTEN_status)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+CD4+ (TS) density by PTEN_status intact vs null 
CD3CD4plotPTENLossTS <- CD3CD4plotPTENLossTS + xlab("PTEN status") + ylab("Density of CD3+CD4+ immune cell phenotype") #Add title
CD3CD4plotPTENLossTS <- CD3CD4plotPTENLossTS +theme(legend.position = "top")
#turn into table and add to graph
texttableCD3CD4TSPT <-ggtexttable(CD3CD4TSPTsum, rows=NULL, cols=c("PTEN status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme("classic"))

#CD68+ (TG) immune cell density and PTEN_status 
#Summary statistics: compute the median and IQR of CD68+ (TG) by PTEN_status
CD68TGPTsum<- Nonlethal %>% group_by(PTEN_status) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Visualize the data: create a boxplot of CD68+ (TG) immune cell density by PTEN_status
CD68plotPTENLossTG <- ggplot(data=subset(Nonlethal, !is.na(PTEN_status)), aes(x=PTEN_status, y=`CD68+(TG)`, fill=PTEN_status)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD68+ (TG) density by PTEN_status intact vs null 
CD68plotPTENLossTG <- CD68plotPTENLossTG + xlab("PTEN status") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotPTENLossTG <- CD68plotPTENLossTG +theme(legend.position = "top")
#turn into table and add to graph
texttableCD68TGPT <-ggtexttable(CD68TGPTsum, rows=NULL, cols=c("PTEN status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme("classic"))

#CD68+ (TS) immune cell density and PTEN_status 
#Summary statistics: compute the median and IQR of CD68+ (TS) by PTEN_status
CD68TSPTsum<- Nonlethal %>% group_by(PTEN_status) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Visualize the data: create a boxplot of CD68+ (TS) immune cell density by PTEN_status
CD68plotPTENLossTS <- ggplot(data=subset(Nonlethal, !is.na(PTEN_status)), aes(x=PTEN_status, y=`CD68+(TS)`, fill=PTEN_status)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD68+ (TG) density by PTEN_status intact vs null 
CD68plotPTENLossTS <- CD68plotPTENLossTS + xlab("PTEN status") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotPTENLossTS <- CD68plotPTENLossTS +theme(legend.position = "top")
#turn into table and add to graph
texttableCD68TSPT <-ggtexttable(CD68TSPTsum, rows=NULL, cols=c("PTEN status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))

##ERG associated with Immune cell densities

#CD3+ (TG) immune cell density and ERG cutpoint 
#Summary statistics: compute the median and IQR of CD3+ (TG) by ERG_Cutpoint  
CD3TGERGsum<- Nonlethal %>% group_by(erg_cutpoint) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+ (TG) immune cell density by ERG Cutpoint
CD3plotERGTG <- ggplot(data=subset(Nonlethal, !is.na(erg_cutpoint)), aes(x=erg_cutpoint, y=`CD3+(TG)`, fill=erg_cutpoint)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+ density (TG) by PTEN_status intact vs null 
CD3plotERGTG <- CD3plotERGTG + xlab("ERG Cutpoint") + ylab("Density of CD3+ immune cell phenotype") +  labs(fill=" ERG Cutpoint")#Add title
CD3plotERGTG <- CD3plotERGTG +theme(legend.position = "top")
#turn into table and add to graph
texttableCD3TGERG <-ggtexttable(CD3TGERGsum, rows=NULL, cols=c("ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))

#CD3+ (TS) immune cell density and ERG Cutpoint 
#Summary statistics: compute the median and IQR of CD3+ (TS) by ERG_Cutpoint  
CD3TSERGsum<- Nonlethal %>% group_by(erg_cutpoint) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+ (TS) immune cell density by ERG Cutpoint
CD3plotERGTS <- ggplot(data=subset(Nonlethal, !is.na(erg_cutpoint)), aes(x=erg_cutpoint, y=`CD3+(TS)`, fill=erg_cutpoint)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+ density (TG) by PTEN_status intact vs null 
CD3plotERGTS <- CD3plotERGTS  + xlab("ERG Cutpoint") + ylab("Density of CD3+ immune cell phenotype") #Add title
CD3plotERGTS<- CD3plotERGTS +theme(legend.position = "top")
#turn into table and add to graph
texttableCD3TSERG <-ggtexttable(CD3TSERGsum, rows=NULL, cols=c("ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                 theme=ttheme("classic"))

#CD3+CD4+ (TG) immune cell density  ERG cutpoint 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TG) by ERG_Cutpoint  
CD3CD4TGERGsum<- Nonlethal %>% group_by(erg_cutpoint) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+CD4+ (TG) immune cell density by ERG Cutpoint
CD3CD4plotERGTG <- ggplot(data=subset(Nonlethal, !is.na(erg_cutpoint)), aes(x=erg_cutpoint, y=`CD3+CD4+(TG)`, fill=erg_cutpoint)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+CD4+ (TG) density by PTEN_status intact vs null 
CD3CD4plotERGTG <- CD3CD4plotERGTG + xlab("ERG Cutpoint") + ylab("Density of CD3+CD4+ immune cell phenotype") #Add title
CD3CD4plotERGTG <- CD3CD4plotERGTG +theme(legend.position = "top")
#turn into table and add to graph
texttableCD3CD4TGERG <-ggtexttable(CD3CD4TGERGsum, rows=NULL, cols=c("ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))

#CD3+CD4+ (TS) immune cell density and ERG Cutpoint 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TS) by ERG_Cutpoint  
CD3CD4TSERGsum<- Nonlethal %>% group_by(erg_cutpoint) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Visualize the data: create a boxplot of CD3+CD4+ (TS) immune cell density by ERG Cutpoint
CD3CD4plotERGTS <- ggplot(data=subset(Nonlethal, !is.na(erg_cutpoint)), aes(x=erg_cutpoint, y=`CD3+CD4+(TS)`, fill=erg_cutpoint)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD3+CD4+ (TS) density by PTEN_status intact vs null 
CD3CD4plotERGTS <- CD3CD4plotERGTS + xlab("ERG Cutpoint") + ylab("Density of CD3+CD4+ immune cell phenotype") #Add title
CD3CD4plotERGTS <- CD3CD4plotERGTS + theme(legend.position = "top")
#turn into table and add to graph
texttableCD3CD4TSERG <-ggtexttable(CD3CD4TSERGsum, rows=NULL, cols=c("ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme("classic"))

#CD68+ (TG) immune cell density ERG cutpoint 
#Summary statistics: compute the median and IQR of CD68+ (TG) by ERG_Cutpoint  
CD68TGERGsum<- Nonlethal %>% group_by(erg_cutpoint) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Visualize the data: create a boxplot of CD68 (TG) immune cell density by ERG Cutpoint
CD68plotERGTG <- ggplot(data=subset(Nonlethal, !is.na(erg_cutpoint)), aes(x=erg_cutpoint, y=`CD68+(TG)`, fill=erg_cutpoint)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD68 + (TG) density by PTEN_status intact vs null 
CD68plotERGTG <- CD68plotERGTG  + xlab("ERG Cutpoint") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotERGTG <- CD68plotERGTG  +theme(legend.position = "top")
#turn into table and add to graph
texttableCD68TGERG <-ggtexttable(CD68TGERGsum, rows=NULL, cols=c("ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme("classic"))


#CD68+ (TS) immune cell density and ERG Cutpoint 
#Summary statistics: compute the median and IQR of CD3+CD4+ (TS) by ERG_Cutpoint  
CD68TSERGsum<- Nonlethal %>% group_by(erg_cutpoint) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Visualize the data: create a boxplot of CD68+ (TS) immune cell density by ERG Cutpoint
CD68plotERGTS <- ggplot(data=subset(Nonlethal, !is.na(erg_cutpoint)), aes(x=erg_cutpoint, y=`CD68+(TS)`, fill=erg_cutpoint)) + geom_boxplot(na.rm=T) + stat_compare_means() #run Wilcox comparing CD68+ (TS) density by PTEN_status intact vs null 
CD68plotERGTS <-  CD68plotERGTS + xlab("ERG Cutpoint") + ylab("Density of CD68+ immune cell phenotype") #Add title
CD68plotERGTS <- CD68plotERGTS + theme(legend.position = "top")
#turn into table and add to graph
texttableCD68TSERG <-ggtexttable(CD68TSERGsum, rows=NULL, cols=c("ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                 theme=ttheme("classic"))



#Perform/Estimating risk ratio (RR) and risk differences using regression in Subcohort (look if immune cell density can predict the risk of PTEN-status and ERG)

#Load package 
library(risks)
library(rifttable)
library(dplyr)
library(broom)

##RR: PTEN-status and immune cell densities

#need to set PTEN_status as a binary outcome 1 (i.e., binary outcome PTEN-null = 1) 
Nonlethal <- Nonlethal %>%  mutate(PTENbinaryoutcome = case_when(
  PTEN_status== "PTEN-intact"~ 0, 
  PTEN_status== "PTEN-null"~ 1,
))

#Perform RR analysis: CD3+(TG) [> or < median] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3TGCatPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ CD3positivemedianTG, data = Nonlethal)  
summary(fitRRCD3TGCatPTEN_status) #Summarize the results
tidy(fitRRCD3TGCatPTEN_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3TGdensityPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ `CD3+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TGdensityPTEN_status) #summarize the results
tidy(fitRRCD3TGdensityPTEN_status, exponentiate = T) #access the models coefficeints 

#Perform RR analysis: CD3+(TS) [> or < median] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3TSCatPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ CD3positivemedianTS, data = Nonlethal)  
summary(fitRRCD3TSCatPTEN_status) #Summarize the results
tidy(fitRRCD3TSCatPTEN_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3TIQRensityPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ `CD3+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TIQRensityPTEN_status) #summarize the results
tidy(fitRRCD3TIQRensityPTEN_status, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3CD4TGCatPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ CD3CD4positivemedianTG, data = Nonlethal)  
summary(fitRRCD3CD4TGCatPTEN_status) #Summarise the results
tidy(fitRRCD3CD4TGCatPTEN_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3CD4TGdensityPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ `CD3+CD4+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TGdensityPTEN_status) #sumrise the results
tidy(fitRRCD3CD4TGdensityPTEN_status, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3CD4TSCatPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ CD3CD4positivemedianTS, data = Nonlethal)  
summary(fitRRCD3CD4TSCatPTEN_status) #Summarise the results
tidy(fitRRCD3CD4TSCatPTEN_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD3CD4TIQRensityPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ `CD3+CD4+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TIQRensityPTEN_status) #summarize the results 
tidy(fitRRCD3CD4TIQRensityPTEN_status, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD68TGCatPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ CD68positivemedianTG, data = Nonlethal)  
summary(fitRRCD68TGCatPTEN_status) #Summarise the results
tidy(fitRRCD68TGCatPTEN_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD68TGdensityPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ `CD68+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TGdensityPTEN_status) #sumrise the results
tidy(fitRRCD68TGdensityPTEN_status, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD68TSCatPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ CD68positivemedianTS, data = Nonlethal)  
summary(fitRRCD68TSCatPTEN_status) #Summarise the results
tidy(fitRRCD68TSCatPTEN_status, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and PTEN_status (i.e., binary outcome PTEN-null)
fitRRCD68TIQRensityPTEN_status <- riskratio(formula = PTENbinaryoutcome ~ `CD68+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TIQRensityPTEN_status) #summarize the results 
tidy(fitRRCD68TIQRensityPTEN_status, exponentiate = T) #access the models coefficient


##RR: ERG status and immune cell density

#factor erg_cutpoint and add levels 
Nonlethal$erg_cutpoint <- factor(Nonlethal$erg_cutpoint, levels = c("ERG_positive", "ERG_negative"))
#view structure 
str(Nonlethal$erg_cutpoint)

#Perform RR analysis: CD3+(TG) [> or < median] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3TGCaterg_cutpoint <- riskratio(formula = erg_cutpoint ~ CD3positivemedianTG, data = Nonlethal)  
summary(fitRRCD3TGCaterg_cutpoint) #Summarize the results
tidy(fitRRCD3TGCaterg_cutpoint, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3TGdensityerg_cutpoint <- riskratio(formula = erg_cutpoint ~ `CD3+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TGdensityerg_cutpoint) #summarize the results
tidy(fitRRCD3TGdensityerg_cutpoint, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3TSCaterg_cutpoint <- riskratio(formula = erg_cutpoint ~ CD3positivemedianTS, data = Nonlethal)  
summary(fitRRCD3TSCaterg_cutpoint) #Summarize the results
tidy(fitRRCD3TSCaterg_cutpoint, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3TIQRensityerg_cutpoint <- riskratio(formula = erg_cutpoint ~ `CD3+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TIQRensityerg_cutpoint) #summarize the results
tidy(fitRRCD3TIQRensityerg_cutpoint, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3CD4TGCaterg_cutpoint <- riskratio(formula = erg_cutpoint ~ CD3CD4positivemedianTG, data = Nonlethal)  
summary(fitRRCD3CD4TGCaterg_cutpoint) #Summarise the results
tidy(fitRRCD3CD4TGCaterg_cutpoint, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3CD4TGdensityerg_cutpoint <- riskratio(formula = erg_cutpoint ~ `CD3+CD4+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TGdensityerg_cutpoint) #sumrise the results
tidy(fitRRCD3CD4TGdensityerg_cutpoint, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3CD4TSCaterg_cutpoint <- riskratio(formula = erg_cutpoint ~ CD3CD4positivemedianTS, data = Nonlethal)  
summary(fitRRCD3CD4TSCaterg_cutpoint) #Summarise the results
tidy(fitRRCD3CD4TSCaterg_cutpoint, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD3CD4TIQRensityerg_cutpoint <- riskratio(formula = erg_cutpoint ~ `CD3+CD4+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TIQRensityerg_cutpoint) #summarize the results 
tidy(fitRRCD3CD4TIQRensityerg_cutpoint, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD68TGCaterg_cutpoint <- riskratio(formula = erg_cutpoint ~ CD68positivemedianTG, data = Nonlethal)  
summary(fitRRCD68TGCaterg_cutpoint) #Summarise the results
tidy(fitRRCD68TGCaterg_cutpoint, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD68TGdensityerg_cutpoint <- riskratio(formula = erg_cutpoint ~ `CD68+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TGdensityerg_cutpoint) #sumrise the results
tidy(fitRRCD68TGdensityerg_cutpoint, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD68TSCaterg_cutpoint <- riskratio(formula = erg_cutpoint ~ CD68positivemedianTS, data = Nonlethal)  
summary(fitRRCD68TSCaterg_cutpoint) #Summarise the results
tidy(fitRRCD68TSCaterg_cutpoint, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and erg_cutpoint (i.e., binary outcome ERG_negative)
fitRRCD68TIQRensityerg_cutpoint <- riskratio(formula = erg_cutpoint ~ `CD68+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TIQRensityerg_cutpoint) #summarize the results 
tidy(fitRRCD68TIQRensityerg_cutpoint, exponentiate = T) #access the models coefficient

#Part two (b): Looking at the RNA expression in the case-control cohort (117 patients)

#Load packages 
library(SummarizedExperiment)
library(MatrixGenerics)
library(DESeq2)
library(ggfortify)
library(sva)
library(ggrepel)
library(EnhancedVolcano)
library(GSVA)
library(pheatmap)
library(limma)
library(dplyr)
library(readr)
library(tibble)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(pheatmap)
library(ComplexHeatmap)

#Import "PN0393_QCpass_adjusted_update.txt" which contains all the patients that pass QC (i.e., contain RNA-seq data) 
PN0393_QCpass_adjusted_update <- read_delim("PN0393_QCpass_adjusted_update.txt", 
                                            delim = "\t", escape_double = FALSE, 
                                            trim_ws = TRUE)
View(PN0393_QCpass_adjusted_update) #View imported data 

#intersect RNA-seq contain for all 117 patients used in this thesis (i.e., all patient in NI.study) 
inter_NIstudy <- intersect(colnames(PN0393_QCpass_adjusted_update),NI.study$Sample.ID)
coldataFull2_NI.study <- NI.study[NI.study$Sample.ID %in% inter_NIstudy,]
adjusted_update_NI.study <- PN0393_QCpass_adjusted_update[,colnames(PN0393_QCpass_adjusted_update) %in% inter_NIstudy]
dim(PN0393_QCpass_adjusted_update) #get the dimensions of the data 
dim(coldataFull2_NI.study) #get the dimensions dimension of the data 
NI.study <- coldataFull2_NI.study 
View(NI.study) #View data 
View(adjusted_update_NI.study) #View data
#Save "adjusted_update_NI.study" to working directory (open in excel and Add gene names into the file)
write.csv(adjusted_update_NI.study, file="adjusted_updateRNA_NIstudy.csv")
#import file "adjusted_update_NI.study" (with add gene names)
adjusted_updateRNA_NIstudy <- read_csv("adjusted_updateRNA_NIstudy.csv")
#Add "Gene" name column 
colnames(adjusted_updateRNA_NIstudy)[1]<- "Gene"
#View RNA-seq data 
View(adjusted_updateRNA_NIstudy)
#Change Column Gene to Rownames: 
adjusted_updateRNA_NIstudy<- data.frame(column_to_rownames(adjusted_updateRNA_NIstudy, var="Gene"))
View(adjusted_updateRNA_NIstudy) #View data 


#RNA-seq analysis: DGE associated with CD3+ (TG) immune defined tumors: 
#Create a Sample info table 
SampleTableCD3positiveTumourGland <- subset(NI.study, select= c("Sample.ID", "CD3positivemedianTG"))
SampleTableCD3positiveTumourGland
#Remove NA samples from SampleTableCD3postiveTumourGland
SampleTableCD3positiveTumourGland<- drop_na(SampleTableCD3positiveTumourGland)
View(SampleTableCD3positiveTumourGland)
#imputting the design
dds <- DESeqDataSetFromMatrix(countData = adjusted_updateRNA_NIstudy,
                              colData = SampleTableCD3positiveTumourGland,
                              design= ~ CD3positivemedianTG)
#runing DGE
dds <- DESeq(dds)
resultsNames(dds) #get names of results
res <- results(dds) 
resOrderedCD3TG <- res[order(res$padj),] #order results by padj 
head(resOrderedCD3TG) #head of ordered results by padj value
topres <- subset(res, res$padj<0.05) #subset DGE with padj <0.05
topres  #call results

#RNA-seq analysis: DGE associated with CD3+ (TS) immune defined tumours (i.e., above or below median)
#Create a Sample info table 
SampleTableCD3positiveTumourStroma <- subset(NI.study, select=c("Sample.ID", "CD3positivemedianTS"))
SampleTableCD3positiveTumourStroma
#Remove the NA samples from SampleTableCD3postiveTumourStroma
SampleTableCD3positiveTumourStroma<- drop_na(SampleTableCD3positiveTumourStroma)
View(SampleTableCD3positiveTumourStroma)
#imputting the design 
ddsCD3TS <- DESeqDataSetFromMatrix(countData = adjusted_updateRNA_NIstudy,
                                   colData = SampleTableCD3positiveTumourStroma,
                                   design= ~ CD3positivemedianTS)
#running DGE
ddsCD3TS <- DESeq(ddsCD3TS)
resultsNames(ddsCD3TS) #get names of results 
resCD3TS <- results(ddsCD3TS) 
resOrderedCD3TS <-resCD3TS[order(resCD3TS$padj),] #order results by padj
head(resOrderedCD3TS) #head of ordered result by padj value 
topresCD3TS <- subset(resCD3TS, resCD3TS$padj<0.05) #subset DGE with padj <0.05
topresCD3TS #call results

#RNA-seq analysis: DGE associated with CD3+CD4+ (TG) immune defined tumours (i.e., above or below median)
#Create a Sample info table 
SampleTableCD3CD4positiveTumourGland <- subset(NI.study, select=c("Sample.ID", "CD3CD4positivemedianTG"))
SampleTableCD3CD4positiveTumourGland
#Remove the NA samples from SampleTableCD3CD4positiveTumourGland
SampleTableCD3CD4positiveTumourGland<- drop_na(SampleTableCD3CD4positiveTumourGland)
View(SampleTableCD3CD4positiveTumourGland)
#inputting the design
ddsCD3CD4TG <- DESeqDataSetFromMatrix(countData = adjusted_updateRNA_NIstudy,
                                      colData = SampleTableCD3CD4positiveTumourGland,
                                      design= ~ CD3CD4positivemedianTG)
#running DGE
ddsCD3CD4TG <- DESeq(ddsCD3CD4TG)
resultsNames(ddsCD3CD4TG) #get names of results
resCD3CD4TG <- results(ddsCD3CD4TG) #get results
resOrderedCD3CD4TG <- resCD3CD4TG[order(resCD3CD4TG$padj),] #order results by p.adj value
head(resOrderedCD3CD4TG)
topresCD3CD4TG <- subset(resCD3CD4TG, resCD3CD4TG$padj<0.05) #Subset DGE by padj value <0.05
topresCD3CD4TG #Call results


#RNA-seq analysis: DGE associated with CD3+CD4+ (TS) immune defined tumours (i.e., above or below median)
#Create a Sample info table 
SampleTableCD3CD4positiveTumourStroma <- subset(NI.study, select=c("Sample.ID", "CD3CD4positivemedianTS"))
SampleTableCD3CD4positiveTumourStroma
#Remove the NA samples from SampleTableCD3CD4positiveTumourStroma
SampleTableCD3CD4positiveTumourStroma<- drop_na(SampleTableCD3CD4positiveTumourStroma)
View(SampleTableCD3CD4positiveTumourStroma)
#inputting the design
ddsCD3CD4TS <- DESeqDataSetFromMatrix(countData = adjusted_updateRNA_NIstudy,
                                      colData = SampleTableCD3CD4positiveTumourStroma,
                                      design= ~ CD3CD4positivemedianTS)
#running DGE
ddsCD3CD4TS <- DESeq(ddsCD3CD4TS)
resultsNames(ddsCD3CD4TS) #get names of results
resCD3CD4TS <- results(ddsCD3CD4TS) #get results
resOrderedCD3CD4TS <- resCD3CD4TS[order(resCD3CD4TS$padj),] #order results by p.adj value
head(resOrderedCD3CD4TS)
topresCD3CD4TS <- subset(resCD3CD4TS, resCD3CD4TS$padj<0.05) #Subset DGE by padj value <0.05
topresCD3CD4TS #Call results


#RNA-seq analysis: DGE associated with CD68+ (TG) immune defined tumours (i.e., above or below median)
#Create a Sample info table 
SampleTableCD68positiveTumourGland <- subset(NI.study, select= c("Sample.ID", "CD68positivemedianTG"))
SampleTableCD68positiveTumourGland
#Remove NA samples from SampleTableCD68positiveTumourGland
SampleTableCD68positiveTumourGland<- drop_na(SampleTableCD68positiveTumourGland)
View(SampleTableCD68positiveTumourGland)
#inputting the design
ddsCD68TG <- DESeqDataSetFromMatrix(countData = adjusted_updateRNA_NIstudy,
                                    colData = SampleTableCD68positiveTumourGland,
                                    design= ~ CD68positivemedianTG)
#running DGE 
ddsCD68TG <- DESeq(ddsCD68TG)
resultsNames(ddsCD68TG) #get names of results 
resCD68TG <- results(ddsCD68TG) #get results 
resOrderedCD68TG <- resCD68TG[order(resCD68TG$padj),] #order results by p.adj value
head(resOrderedCD68TG)
topresCD68TG <- subset(resCD68TG, resCD68TG$padj<0.05)
summary(topresCD68TG)

#RNA-seq analysis: DGE associated with CD68+ (TS) immune defined tumours (i.e., above or below median)
#Create a Sample info table 
SampleTableCD68positiveTumourStroma <- subset(NI.study, select=c("Sample.ID", "CD68positivemedianTS"))
SampleTableCD68positiveTumourStroma
#Remove the NA samples from SampleTableCD68positiveTumourStroma
SampleTableCD68positiveTumourStroma<- drop_na(SampleTableCD68positiveTumourStroma)
View(SampleTableCD68positiveTumourStroma)
#inputting the design
ddsCD68TS <- DESeqDataSetFromMatrix(countData = adjusted_updateRNA_NIstudy,
                                    colData = SampleTableCD68positiveTumourStroma,
                                    design= ~ CD68positivemedianTS)
#run DGE 
ddsCD68TS <- DESeq(ddsCD68TS)
resultsNames(ddsCD68TS) #get names of results
resCD68TS <- results(ddsCD68TS)
resOrderedCD68TS <- resCD68TS[order(resCD68TS$padj),] #order results by p.adj value
head(resOrderedCD68TS)
topresCD68TS <- subset(resCD68TS, resCD68TS$padj<0.05)
topresCD68TS

#Plot a heatmap of the DGE identified: 

#DGE covert to dataframe contains all the DGE from the defined phenotype:
significantDGECD3CD4TG <- data.frame(topresCD3CD4TG) #CD3+CD4+ (TG) significant DGE
significantDGECD3TG <-data.frame(topres) #CD3+ (TG) significant DGE 
significantDGECD68TS <- data.frame(topresCD68TS) #CD68+ (TS) significant DGE

#create a count for CD3+ (TG)
countCD3TG <- counts(dds, normalized= T)[rownames(significantDGECD3TG),]
view(countCD3TG)
#create a count of CD3+CD4+ (TG)
countCD3CD4TG <- counts(ddsCD3CD4TG, normalized=T)[rownames(significantDGECD3CD4TG),]
view(countCD3CD4TG)
#create a count of CD68+ (TG)
countCD68TS <- counts(ddsCD68TS, normalized = TRUE)[rownames(significantDGECD68TS),]
view(countCD68TS)
countCD68TS <- t(countCD68TS) #transpose the data 
rownames(countCD68TS) <- rownames(significantDGECD68TS) #set rownames 

#Merge Count for all DGE 
MergeDGECountData <- rbind(countCD3TG, countCD3CD4TG)
MergeDGECountData<- rbind(MergeDGECountData, countCD68TS)
View(MergeDGECountData) #View
#Get the Z-score for each row
MergeDGECountData.z <- t(apply(MergeDGECountData, 1, scale))
#Set colnames 
colnames(MergeDGECountData.z) <- colnames(MergeDGECountData)
View(MergeDGECountData.z) #View 
#Create annotations for heatmap (subset out the annotation interested in)
ann <- data.frame(NI.study$Cohort, NI.study$CD3positivemedianTG, NI.study$CD3CD4positivemedianTG, NI.study$CD68positivemedianTS)
#replace abovemedian with > median and equalbelow with ≤ median
ann <- replace(ann, ann=="abovemedian", "> median")
ann <-replace(ann,ann=="equalbelowmedian", "≤ median")
#set column names of the annotation for heatmap 
colnames(ann) <- c("Cohort", "CD3+(TG)","CD3+CD4+(TG)", "CD68+(TS)" )
#colour annotation for heatmap
colours <- list("Cohort" = c("lethal PCa"= "red2", "non-lethal PCa"= "green"), 
                "CD3+(TG)" = c("> median" = "orange", "≤ median"= "pink"), 
                "CD3+CD4+(TG)" = c("> median" = "blue", "≤ median"= "brown"), 
                "CD3+CD4+(TG)" = c("> median" = "purple", "≤ median"= "yellow"))
#Create a constuct whihc contains the heatmap annotation
colAnn <- HeatmapAnnotation(df=ann, 
                            which = 'col',
                            col = colours,
                            annotation_width = unit(c(1, 4), 'cm'),
                            gap = unit(1, 'mm'))
#Make a heatmap of the DEG (add in the annotation data)
Heatmap(MergeDGECountData.z, show_column_names = F, top_annotation = colAnn)

##Pathway enrichment analysis in R: Using R package PathfindR

#Load package
library(pathfindR)  

#Create a df of the DGE to be used in PathfindR
DEGimmunedefinedcells <- data.frame(Gene.symbol = c("ETV1", "TNNC1", "ACOX1", "ASIC3", "CYP1B1", "FOS", "IGFBP2", "LMLN", "PLPP3", "TXNDC5", "PLAT"), 
                                    logFC= c(1.27367, -1.34948, 0.432725, -0.545479, 0.74043, 1.161429, 0.596631, -0.467189 , 0.643353,0.609913, -0.855664),
                                    adj.P.Val = c(0.01214716,  0.00286008,  0.04329169, 0.048343,  0.00114155,  0.04329169,  0.0356153, 0.0356153, 0.0356153,  0.04329169, 0.02273))
#Run pathway analysis of DGE
outputR <- run_pathfindR(DEGimmunedefinedcells, gene_sets = "Reactome") #uses Reactome pathway database
outputK <- run_pathfindR(DEGimmunedefinedcells, gene_sets = "KEGG") #kegg database

#plot enrichment chart for KEGG and Reactome pathway 
enrichment_chart(result_df = outputK, 
                 top_terms = 20)
enrichment_chart(result_df = outputR, 
                 top_terms = 13)
#Generate an enrichment term diagngram 
input_processed <- input_processing(DEGimmunedefinedcells)
#visualise enriched term diagram 
visualize_terms(
  result_df = outputK, 
  input_processed = input_processed, 
  hsa_KEGG= T
)

#####################################################################################################################################################################################
####Part Three: : Are immune phenotypes potentially modifiable? Can we explore evidence for this by examining how lifestyle factors and co-morbid conditions are related to prostate immune cell densities?#####

#load R packages 
library(ggplot2)
library(ggpubr)
library(cowplot)
library(risks)
library(tidyverse)
library(rstatix)

###Pt. taking lipids reg. drugs###

#CD3+ (TG) immune cell density and Pt. taking Lipid reg drugs: 
#Summary stats: compute the median and IQR of CD3+(TG) density and Pt. taking lipid reg. drugs 
CD3TGLipidsum<-Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visulise the data: Create a box plot of the CD3+ (TG) density by patient taking Lipid reg drug  
CD3plotLipidsTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TG)`, fill=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)) +geom_boxplot() + stat_compare_means() #run Wilcox 
CD3plotLipidsTG <- CD3plotLipidsTG + xlab("Lipids Regulating Drug") + ylab("Density of CD3+ immune cell phenotype")+ labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsTG <- CD3plotLipidsTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGLipids <-ggtexttable(CD3TGLipidsum, rows=NULL, cols=c("Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))


#CD3+ (TS) immune cell density and Lipids reg drugs: 
#Summary stats: compute the median and IQR of CD3+(TS) density and Pt. taking lipid reg. drugs 
CD3TSLipidsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Visulise the data: Create a box plot of the CD3+ (TS) density by patient taking Lipid reg drug  
CD3plotLipidsTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TS)`, fill=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)) +geom_boxplot() + stat_compare_means() #run Wilcox 
CD3plotLipidsTS<- CD3plotLipidsTS + xlab("Lipids Regulating Drug") + ylab("Density of CD3+ immune cell phenotype")+ labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsTS <- CD3plotLipidsTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSLipids <-ggtexttable(CD3TSLipidsum, rows=NULL, cols=c("Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme("classic"))


#CD3+CD4+ (TG) immune cell density and Pt. taking Lipids reg. drugs 
#Summary stats: compute the median and IQR of CD3+CD4+(TG) density and Pt. taking lipid reg. drugs 
CD3CD4TGLipidsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Visulise the data: Create a box plot of the CD3+CD4+ (TG) density by patient taking Lipid reg drug  
CD3CD4plotLipidsTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TG)`, fill=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)) +geom_boxplot() + stat_compare_means() #run Wilcox 
CD3CD4plotLipidsTG <- CD3CD4plotLipidsTG + xlab("Lipids Regulating Drug") + ylab("Density of CD3+CD4+ immune cell phenotype")+ labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsTG <- CD3CD4plotLipidsTG +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGLipids <-ggtexttable(CD3CD4TGLipidsum, rows=NULL, cols=c("Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme("classic"))

#CD3+CD4+ (TS) immune cell density and Pt. taking Lipids reg. drugs 
#Summary stats: compute the median and IQR of CD3+CD4+(TS) density and Pt. taking lipid reg. drugs 
CD3CD4TSLipidsum <- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Visulise the data: Create a box plot of the CD3+CD4+ (TS) density by patient taking Lipid reg drug  
CD3CD4plotLipidsTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TS)`, fill=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)) +geom_boxplot() + stat_compare_means() #run Wilcox 
CD3CD4plotLipidsTS<- CD3CD4plotLipidsTS + xlab("Lipids Regulating Drug") + ylab("Density of CD3+CD4+ immune cell phenotype")+ labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsTS <- CD3CD4plotLipidsTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSLipids <-ggtexttable(CD3CD4TSLipidsum, rows=NULL, cols=c("Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme("classic"))

#CD68+ (TS) immune cell density and Pt. taking Lipids reg. drugs 
#Summary stats: compute the median and IQR of CD68+(TS) density and Pt. taking lipid reg. drugs 
CD68TGLipidsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Visulise the data: Create a box plot of the CD68+ (TG) density by patient taking Lipid reg drug  
CD68plotLipidsTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TG)`, fill=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)) +geom_boxplot() + stat_compare_means() #run Wilcox 
CD68plotLipidsTG<- CD68plotLipidsTG + xlab("Lipids Regulating Drug") + ylab("Density of CD68+ immune cell phenotype")+ labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsTG <- CD68plotLipidsTG +theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGLipids <-ggtexttable(CD68TGLipidsum, rows=NULL, cols=c("Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme("classic"))

#CD68+ (TS) immune cell density and Pt. taking Lipids reg. drugs 
#Summary stats: compute the median and IQR of CD68+(TS) density and Pt. taking lipid reg. drugs 
CD68TSLipidsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Visulise the data: Create a box plot of the CD68+ (TS) density by patient taking Lipid reg drug  
CD68plotLipidsTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TS)`, fill=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)) +geom_boxplot() + stat_compare_means() #run Wilcox 
CD68plotLipidsTS<- CD68plotLipidsTS + xlab("Lipids Regulating Drug") + ylab("Density of CD68+ immune cell phenotype")+ labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsTS <- CD68plotLipidsTS+theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSLipids <-ggtexttable(CD68TSLipidsum, rows=NULL, cols=c("Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                    theme=ttheme("classic"))

#Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage: 

#CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
#Perform statistical association test between CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
stat.testCD3lipidsTCS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsTCS #View results
#Summary stats:
CD3TGLipidTCSsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visualize the data: Create a boxplot of CD3+ (TG) density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
CD3plotLipidsTCSTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TG)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3plotLipidsTCSTG <- CD3plotLipidsTCSTG + stat_pvalue_manual(stat.testCD3lipidsTCS, label="p",  y.position= 6000) +xlab("Lipids Regulating Drug") + ylab("Density of CD3+ immune cell phenotype")+ labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsTCSTG <- CD3plotLipidsTCSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGLipidsTCS <-ggtexttable(CD3TGLipidTCSsum, rows=NULL,   cols=c("Tumour Clinical Stage", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                    theme=ttheme(base_size = 7, "classic"))




#CD3+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
#Summary stats: Compute the median and IQR of CD3+(TS) density and Pt. taking lipid reg. drugs stratified by Tumour Clinical Stage
CD3TSLipidTCSsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
stat.testCD3lipidsTCSTS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsTCSTS #View results
#Visualise the data: Create a boxplot of CD3+ (TS) density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
CD3plotLipidsTCSTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TS)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3plotLipidsTCSTS <- CD3plotLipidsTCSTS+ stat_pvalue_manual(stat.testCD3lipidsTCSTS, label="p", y.position = 920) + ylab("Density of CD3+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs")
CD3plotLipidsTCSTS <- CD3plotLipidsTCSTS+ theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSLipidsTCS <-ggtexttable(CD3TSLipidTCSsum, rows=NULL,  cols=c("Tumour Clinical Stage", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7,"classic"))

#CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
#Summary stats: Compute the median and IQR of CD3+(TS) density and Pt. taking lipid reg. drugs stratified by Tumour Clinical Stage
CD3CD4TGLipidTCSsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
stat.testCD3CD4lipidsTCS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+CD4+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsTCS #View results 
#Visualise the data: Create a boxplot of CD3+CD4+ (TG) density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
CD3CD4plotLipidsTCSTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TG)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3CD4plotLipidsTCSTG <- CD3CD4plotLipidsTCSTG+ stat_pvalue_manual(stat.testCD3CD4lipidsTCS, label="p",  y.position= 4000) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsTCSTG <- CD3CD4plotLipidsTCSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGLipidsTCS <-ggtexttable(CD3CD4TGLipidTCSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))


#CD3+CD4+ (TS)  immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage

#Summary stats: Compute the median and IQR of CD3+CD4+(TS) density and Pt. taking lipid reg. drugs stratified by Tumour Clinical Stage
CD3CD4TSLipidTCSsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
stat.testCD3CD4lipidsTCSTS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+CD4+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsTCSTS #View results
#Visualise the data: Create a boxplot of CD3+CD4+ (TS) density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
CD3CD4plotLipidsTCSTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TS)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3CD4plotLipidsTCSTS<- CD3CD4plotLipidsTCSTS + stat_pvalue_manual(stat.testCD3CD4lipidsTCSTS, label="p", y.position = 620) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsTCSTS <- CD3CD4plotLipidsTCSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSLipidsTCS <-ggtexttable(CD3CD4TSLipidTCSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                         theme=ttheme(base_size = 7,"classic"))

#CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
#Summary stats: Compute the median and IQR of CD68+(TG) density and Pt. taking lipid reg. drugs stratified by Tumour Clinical Stage
CD68TGLipidTCSsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Perform statistical association test between CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
stat.testCD68lipidsTCS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD68+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsTCS #View results 
#Visualize the data: Create a boxplot of CD68+ (TG) density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
CD68plotLipidsTCSTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TG)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD68plotLipidsTCSTG<- CD68plotLipidsTCSTG + stat_pvalue_manual(stat.testCD68lipidsTCS, label="p",  y.position= 1500) + ylab("Density of CD68+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsTCSTG<- CD68plotLipidsTCSTG +theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGLipidsTCS <-ggtexttable(CD68TGLipidTCSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                         theme=ttheme(base_size = 7, "classic"))
  
#CD68+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
#Summary Stat:
CD68TSLipidTCSsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test between CD68+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
stat.testCD68lipidsTCSTS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD68+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsTCSTS #View results
#Visualize the data: Create a boxplot of CD68+ (TS) density and Pt. taking lipid regulating drugs stratified by Tumour Clinical Stage
CD68plotLipidsTCSTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TS)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD68plotLipidsTCSTS<- CD68plotLipidsTCSTS + stat_pvalue_manual(stat.testCD68lipidsTCSTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsTCSTS <- CD68plotLipidsTCSTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSLipidsTCS <-ggtexttable(CD68TSLipidTCSsum, rows=NULL, cols=c("Tumour Clinical Stage", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                       theme=ttheme(base_size = 7, "classic"))

#Immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade 

#CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
#Summary stats: compute the median and IQR of CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3TGLipidsGSsum <- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
stat.testCD3lipidsGS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsGS #View results 
#Visualize the data: Create a boxplot of CD3+ (TG) density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3plotLipidsGSTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TG)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3plotLipidsGSTG<- CD3plotLipidsGSTG + stat_pvalue_manual(stat.testCD3lipidsGS, label="p",  y.position= 6000) + ylab("Density of CD3+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsGSTG <- CD3plotLipidsGSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGLipidsGS <-ggtexttable(CD3TGLipidsGSsum, rows=NULL, cols=c("Gleason Score", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                       theme=ttheme(base_size = 7, "classic"))


#CD3+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
#Summary stats: compute the median and IQR of CD3+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3TSLipidsGSsum <- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
##Perform statistical association test between CD3+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
stat.testCD3lipidsGSTS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsGSTS #View results
#Visualize the data: Create a boxplot of CD3+ (TS) density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3plotLipidsGSTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TS)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3plotLipidsGSTS <- CD3plotLipidsGSTS + stat_pvalue_manual(stat.testCD3lipidsGSTS, label="p", y.position = 920) + ylab("Density of CD3+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsGSTS <- CD3plotLipidsGSTS  + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSLipidsGS <-ggtexttable(CD3TSLipidsGSsum, rows=NULL, cols=c("Gleason Score", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))


#CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
#Summary stats: compute the median and IQR of CD3+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3CD4TGLipidsGSsum <- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
stat.testCD3CD4lipidsGS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+CD4+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsGS #View results 
#Visualize the data: Create a boxplot of CD3+CD4+ (TG) density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3CD4plotLipidsGSTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TG)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3CD4plotLipidsGSTG<- CD3CD4plotLipidsGSTG + stat_pvalue_manual(stat.testCD3CD4lipidsGS, label="p",  y.position= 4000) +ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsGSTG <- CD3CD4plotLipidsGSTG +   theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGLipidsGS <-ggtexttable(CD3CD4TGLipidsGSsum, rows=NULL, cols=c("Gleason Score", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                     theme=ttheme(base_size = 7, "classic"))


#CD3+CD4+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade 
#Summary stats: compute the median and IQR of CD3+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3CD4TSLipidsGSsum <- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
stat.testCD3CD4lipidsGSTS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+CD4+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsGSTS #View results
#Visualize the data: Create a boxplot of CD3+CD4+ (TS) density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD3CD4plotLipidsGSTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TS)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3CD4plotLipidsGSTS<- CD3CD4plotLipidsGSTS + stat_pvalue_manual(stat.testCD3CD4lipidsGSTS, label="p", y.position = 620) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsGSTS <- CD3CD4plotLipidsGSTS+ theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSLipidsGS <-ggtexttable(CD3CD4TSLipidsGSsum, rows=NULL, cols=c("Gleason Score", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                        theme=ttheme(base_size = 7, "classic"))


#CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
#Summary stats: compute the median and IQR of CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD68TGLipidsGSsum <- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test between CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
stat.testCD68lipidsGS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD68+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsGS #View results 
#Visualize the data: Create a boxplot of CD68+ (TG) density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD68plotLipidsGSTG <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TG)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD68plotLipidsGSTG <- CD68plotLipidsGSTG + stat_pvalue_manual(stat.testCD68lipidsGS, label="p",  y.position= 1500) +ylab("Density of CD68+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsGSTG <- CD68plotLipidsGSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGLipidsGS <-ggtexttable(CD68TGLipidsGSsum, rows=NULL, cols=c("Gleason Score", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                        theme=ttheme(base_size = 7, "classic"))


#CD68+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
#Summary stats: compute the median and IQR of CD68+(TS) immune cell density by lipid reg. drugs and GS_status 
CD68TSLipidsGSsum<- Nonlethal %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test between CD68+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
stat.testCD68lipidsGSTS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD68+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsGSTS #View results
#Visualize the data: Create a boxplot of CD68+ (TS) density and Pt. taking lipid regulating drugs stratified by Gleason Score/Grade
CD68plotLipidsGSTS <- ggplot(Nonlethal, aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TS)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD68plotLipidsGSTS<- CD68plotLipidsGSTS + stat_pvalue_manual(stat.testCD68lipidsGSTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("Lipid Regulating Drugs") + labs(fill="Lipid Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsGSTS <- CD68plotLipidsGSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSLipidsGS <-ggtexttable(CD68TSLipidsGSsum, rows=NULL, cols=c("Gleason Score", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))


###Pt. taking lipid regulating drugs Stratified by PTEN Status:

#CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status 
#Summary stats: compute the median and IQR of CD68+(TS) immune cell density by lipid reg. drugs and GS_status 
CD3TGLipidsPTENSTATUSsum<- Nonlethal %>% filter(!is.na(PTEN_status)) %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
stat.testCD3lipidsPTENSTATUS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsPTENSTATUS #View results 
#Visualize the data: Create a boxplot of CD3+ (TG) density and Pt. taking lipid regulating drugs stratified by PTEN_status
CD3plotLipidsPTENSTATUSTG <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%
  ggplot(aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TG)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3plotLipidsPTENSTATUSTG<- CD3plotLipidsPTENSTATUSTG + stat_pvalue_manual(stat.testCD3lipidsPTENSTATUS, label="p",  y.position= 6000) +ylab("Density of CD3+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") + theme(legend.position =  "top") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsPTENSTATUSTG 
#Turn into table add to graph
texttableCD3TGLipidsPTENSTATUS <-ggtexttable(CD3TGLipidsPTENSTATUSsum, rows=NULL, cols=c("PTEN Status", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))

#CD3+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
#Summary stats: compute the median and IQR of CD68+(TS) immune cell density by lipid reg. drugs and GS_status 
CD3TSLipidsPTENSTATUSsum<- Nonlethal %>% filter(!is.na(PTEN_status)) %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
stat.testCD3lipidsPTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsPTENSTATUSTS #View results
#Visualize the data: Create a boxplot of CD3+ (TS) density and Pt. taking lipid regulating drugs stratified by PTEN_status
CD3plotLipidsPTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TS)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3plotLipidsPTENSTATUSTS<- CD3plotLipidsPTENSTATUSTS + stat_pvalue_manual(stat.testCD3lipidsPTENSTATUSTS, label="p", y.position = 920) +ylab("Density of CD3+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsPTENSTATUSTS <- CD3plotLipidsPTENSTATUSTS+ theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSLipidsPTENSTATUS <-ggtexttable(CD3TSLipidsPTENSTATUSsum, rows=NULL, cols=c("PTEN Status", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                             theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
#Summary stats: compute the median and IQR of CD68+(TS) immune cell density by lipid reg. drugs and GS_status 
CD3CD4TGLipidsPTENSTATUSsum<- Nonlethal %>% filter(!is.na(PTEN_status)) %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
stat.testCD3CD4lipidsPTENSTATUS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+CD4+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsPTENSTATUS #View results 
#Visualize the data: Create a boxplot of CD3+CD4+ (TG) density and Pt. taking lipid regulating drugs stratified by PTEN_status
CD3CD4plotLipidsPTENSTATUSTG <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TG)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3CD4plotLipidsPTENSTATUSTG<- CD3CD4plotLipidsPTENSTATUSTG + stat_pvalue_manual(stat.testCD3CD4lipidsPTENSTATUS, label="p",  y.position= 4000) +ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsPTENSTATUSTG<- CD3CD4plotLipidsPTENSTATUSTG+theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGLipidsPTENSTATUS <-ggtexttable(CD3CD4TGLipidsPTENSTATUSsum, rows=NULL, cols=c("PTEN Status", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                             theme=ttheme(base_size = 7, "classic"))


#CD3+CD4+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
#Summary stats: compute the median and IQR of CD3+CD4+(TS) immune cell density by lipid reg. drugs and GS_status 
CD3CD4TSLipidsPTENSTATUSsum<- Nonlethal %>% filter(!is.na(PTEN_status)) %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
stat.testCD3CD4lipidsPTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+CD4+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsPTENSTATUSTS #View results
#Visualize the data: Create a boxplot of CD3+CD4+ (TS) density and Pt. taking lipid regulating drugs stratified by PTEN_status
CD3CD4plotLipidsPTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TS)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3CD4plotLipidsPTENSTATUSTS<- CD3CD4plotLipidsPTENSTATUSTS + stat_pvalue_manual(stat.testCD3CD4lipidsPTENSTATUSTS, label="p", y.position = 620) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsPTENSTATUSTS <-CD3CD4plotLipidsPTENSTATUSTS+theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSLipidsPTENSTATUS <-ggtexttable(CD3CD4TSLipidsPTENSTATUSsum, rows=NULL, cols=c("PTEN Status", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                                theme=ttheme(base_size = 7, "classic"))


#CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status
#Summary stats: compute the median and IQR of CD68+(TS) immune cell density by lipid reg. drugs and GS_status 
CD68TGLipidsPTENSTATUSsum<- Nonlethal %>% filter(!is.na(PTEN_status)) %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, GS_status) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Perform statistical association test between CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status 
stat.testCD68lipidsPTENSTATUS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  group_by(`PTEN_status`) %>% 
  t_test(`CD68+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsPTENSTATUS #View results 
#Visualize the data: Create a boxplot of CD68+ (TG) density and Pt. taking lipid regulating drugs stratified by PTEN_status
CD68plotLipidsPTENSTATUSTG <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TG)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD68plotLipidsPTENSTATUSTG<- CD68plotLipidsPTENSTATUSTG + stat_pvalue_manual(stat.testCD68lipidsPTENSTATUS, label="p",  y.position= 1500) +ylab("Density of CD68+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsPTENSTATUSTG<- CD68plotLipidsPTENSTATUSTG +theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGLipidsPTENSTATUS <-ggtexttable(CD68TGLipidsPTENSTATUSsum, rows=NULL, cols=c("PTEN Status", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                                theme=ttheme(base_size = 7, "classic"))


#CD68+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status 
#Summary stats: Compute the median and IQR of CD68+(TS) density by lipid reg. drugs and PTEN_status
CD68TSLipidsPTENSTATUSsum<- Nonlethal %>% filter(!is.na(PTEN_status)) %>% group_by(Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, PTEN_status) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test between CD68+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by PTEN Status 
stat.testCD68lipidsPTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  group_by(`PTEN_status`) %>% 
  t_test(`CD68+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsPTENSTATUSTS #View results
#Visualize the data: Create a boxplot of CD68+ (TS) density and Pt. taking lipid regulating drugs stratified by PTEN_status
CD68plotLipidsPTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TS)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD68plotLipidsPTENSTATUSTS<- CD68plotLipidsPTENSTATUSTS + stat_pvalue_manual(stat.testCD68lipidsPTENSTATUSTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("Lipid Regulating Drugs") + labs(fill="Lipid Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsPTENSTATUSTS <- CD68plotLipidsPTENSTATUSTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSLipidsPTENSTATUS <-ggtexttable(CD68TSLipidsPTENSTATUSsum, rows=NULL, cols=c("PTEN Status", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                              theme=ttheme(base_size = 7, "classic"))

 
  
### Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 

#CD3+ (TG) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 
#Summary statistics: Compute the median and IQR of CD3+(TG) density by lipid reg. drugs and ERG_cutpoint
CD3TGLipidsERGsum<- Nonlethal %>% filter(!is.na(erg_cutpoint))%>% group_by(erg_cutpoint,Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Perform statistical association test : CD3+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 
stat.testCD3lipidsERG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsERG #View results 
#Visualize the data: Create a boxplot of CD3+ (TG) density and Pt. taking lipid regulating drugs stratified by ERG_cutpoint 
CD3plotLipidsERGTG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%
  ggplot(aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TG)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3plotLipidsERGTG<- CD3plotLipidsERGTG + stat_pvalue_manual(stat.testCD3lipidsERG, label="p",  y.position= 6000) +ylab("Density of CD3+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsERGTG <- CD3plotLipidsERGTG+ theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGLipidsERG <-ggtexttable(CD3TGLipidsERGsum, rows=NULL, cols=c("ERG Cutpoint", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                              theme=ttheme(base_size = 7, "classic"))


#CD3+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint  
#Summary statistics: Compute the median and IQR of CD3+(TS) density by lipid reg. drugs and ERG_cutpoint
CD3TSLipidsERGsum<- Nonlethal %>% filter(!is.na(erg_cutpoint))%>% group_by(erg_cutpoint,Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Perform statistical association test: CD3+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint  
stat.testCD3lipidsERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3lipidsERGTS #View results
#Visualize the data: Create a boxplot of CD3+ (TS) density and Pt. taking lipid regulating drugs stratified by ERG_cutpoint 
CD3plotLipidsERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+(TS)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3plotLipidsERGTS<- CD3plotLipidsERGTS + stat_pvalue_manual(stat.testCD3lipidsERGTS, label="p", y.position = 920) +ylab("Density of CD3+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3plotLipidsERGTS<-CD3plotLipidsERGTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSLipidsERG <-ggtexttable(CD3TSLipidsERGsum, rows=NULL, cols=c("ERG Cutpoint", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint
#Summary statistics: Compute the median and IQR of CD3+CD4+(TG) density by lipid reg. drugs and ERG_cutpoint
CD3CD4TGLipidsERGsum<- Nonlethal %>% filter(!is.na(erg_cutpoint))%>% group_by(erg_cutpoint,Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Perform statistical association test: CD3+CD4+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 
stat.testCD3CD4lipidsERG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+CD4+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsERG #View results 
#Visualize the data: Create a boxplot of CD3+CD4+ (TG) density and Pt. taking lipid regulating drugs stratified by ERG_cutpoint 
CD3CD4plotLipidsERGTG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TG)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3CD4plotLipidsERGTG<- CD3CD4plotLipidsERGTG + stat_pvalue_manual(stat.testCD3CD4lipidsERG, label="p",  y.position= 4000) +ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsERGTG <- CD3CD4plotLipidsERGTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGLipidsERG <-ggtexttable(CD3CD4TGLipidsERGsum, rows=NULL, cols=c("ERG Cutpoint", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))


#CD3+CD4+ (TS) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 
#Summary statistics: Compute the median and IQR of CD3+CD4+(TS) density by lipid reg. drugs and ERG_cutpoint
CD3CD4TSLipidsERGsum<- Nonlethal %>% filter(!is.na(erg_cutpoint))%>% group_by(erg_cutpoint,Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test: CD3+CD4+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 
stat.testCD3CD4lipidsERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+CD4+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4lipidsERGTS #View results
#Visualize the data: Create a boxplot of CD3+CD4+ (TS) density and Pt. taking lipid regulating drugs stratified by ERG_cutpoint 
CD3CD4plotLipidsERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD3+CD4+(TS)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3CD4plotLipidsERGTS<- CD3CD4plotLipidsERGTS + stat_pvalue_manual(stat.testCD3CD4lipidsERGTS, label="p", y.position = 620) +ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plotLipidsERGTS <- CD3CD4plotLipidsERGTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSLipidsERG <-ggtexttable(CD3CD4TSLipidsERGsum, rows=NULL, cols=c("ERG Cutpoint", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                        theme=ttheme(base_size = 7, "classic"))


#CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 
#Summary statistics: Compute the median and IQR of CD68+(TG) density by lipid reg. drugs and ERG_cutpoint
CD68TGLipidsERGsum<- Nonlethal %>% filter(!is.na(erg_cutpoint))%>% group_by(erg_cutpoint,Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Perform statistical association test: CD68+(TG) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint
stat.testCD68lipidsERG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD68+(TG)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsERG #View results 
#Visualize the data: Create a boxplot of CD68+ (TG) density and Pt. taking lipid regulating drugs stratified by ERG_cutpoint 
CD68plotLipidsERGTG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TG)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD68plotLipidsERGTG<- CD68plotLipidsERGTG + stat_pvalue_manual(stat.testCD68lipidsERG, label="p",  y.position= 1500) + ylab("Density of CD68+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsERGTG <- CD68plotLipidsERGTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGLipidsERG <-ggtexttable(CD68TGLipidsERGsum, rows=NULL, cols=c("ERG Cutpoint", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                         theme=ttheme(base_size = 7, "classic"))

  
#CD68+ (TS)  immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint 
#Summary statistics: Compute the median and IQR of CD68+(TG) density by lipid reg. drugs and ERG_cutpoint
CD68TSLipidsERGsum<- Nonlethal %>% filter(!is.na(erg_cutpoint))%>% group_by(erg_cutpoint,Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test: CD68+(TS) immune cell density and Pt. taking lipid regulating drugs stratified by ERG_Cutpoint
stat.testCD68lipidsERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD68+(TS)`~ Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68lipidsERGTS #View results
#Visualize the data: Create a boxplot of CD68+ (TS) density and Pt. taking lipid regulating drugs stratified by ERG_cutpoint 
CD68plotLipidsERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>%   ggplot( aes(x=Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, y=`CD68+(TS)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD68plotLipidsERGTS<- CD68plotLipidsERGTS + stat_pvalue_manual(stat.testCD68lipidsERGTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("Lipids Regulating Drugs") + labs(fill="Lipids Regulating Drugs") #Add title and statistical test (i.e., p-value) into graph
CD68plotLipidsERGTS <- CD68plotLipidsERGTS+theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSLipidsERG <-ggtexttable(CD68TSLipidsERGsum, rows=NULL, cols=c("ERG Cutpoint", "Lipids Regulating Drug", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                       theme=ttheme(base_size = 7, "classic"))


###Immune cell density and history_cardiovascular_disease:

#CD3+ (TG) immune cell density and history_cardiovascular_disease
#summary statistics: compute the median and IQR  of CD3+ (TG) immune cell density by history_cardiovascular_disease
CD3TGCVDsum<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visualize the data: Create a boxplot of the CD3+ (TG) density by history of cardiovascular disease
CD3plotCVDTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+(TG)` ,fill=history_cardiovascular_disease)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis 
CD3plotCVDTG<-CD3plotCVDTG + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title 
CD3plotCVDTG <-CD3plotCVDTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGCVD <-ggtexttable(CD3TGCVDsum, rows=NULL, cols=c("History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))

#CD3+ (TS) immune cell density and history_cardiovascular_disease
#summary statistics: compute the median and IQR  of CD3+ (TS) immune cell density by history_cardiovascular_disease
CD3TSCVDsum<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Visualize the data: Create a boxplot of the CD3+ (TS) density by history of cardiovascular disease
CD3plotCVDTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+(TS)` ,fill=history_cardiovascular_disease)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis 
CD3plotCVDTS<- CD3plotCVDTS + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #add title 
CD3plotCVDTS <- CD3plotCVDTS+ theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSCVD <-ggtexttable(CD3TSCVDsum, rows=NULL, cols=c("History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                theme=ttheme("classic"))


#CD3+CD4+ (TG) immune cell density and history_cardiovascular_disease
#summary statistics: comput the median and IQR  of CD3+CD4+ (TG) by history_cardiovascular_disease
CD3CD4TGCVDsum<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+CD4+ (TG) immune cell density by history of cardiovascular disease
CD3CD4plotCVDTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TG)` ,fill=history_cardiovascular_disease)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis 
CD3CD4plotCVDTG <- CD3CD4plotCVDTG + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #add title 
CD3CD4plotCVDTG <- CD3CD4plotCVDTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGCVD <-ggtexttable(CD3CD4TGCVDsum, rows=NULL, cols=c("History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme("classic"))


#CD3+CD4+(TS) immune cell density and history_cardiovascular_disease
#summary statistics: comput the median and IQR  of CD3+CD4+ (TS) by history_cardiovascular_disease
CD3CD4TSCVDsum<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Visualise the data: Create a box plot of the CD3+ (TS) immune cell density and history_cardiovascular_disease
CD3CD4plotCVDTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TS)` ,fill=history_cardiovascular_disease)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis 
CD3CD4plotCVDTS<- CD3CD4plotCVDTS + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #add title 
CD3CD4plotCVDTS<- CD3CD4plotCVDTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSCVD <-ggtexttable(CD3CD4TSCVDsum, rows=NULL, cols=c("History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme("classic"))


#CD68+ (TG) immune cell density and history_cardiovascular_disease
#summary statistics: compute the median and IQR  of CD68+ (TG) immune cell density and history_cardiovascular_disease 
CD68TGCVDsum<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease))%>% group_by(history_cardiovascular_disease) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Visualise the data: Create a box plot of the CD68+ (TG) immune cell density and history_cardiovascular_disease
CD68plotCVDTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD68+(TG)` ,fill=history_cardiovascular_disease)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis 
CD68plotCVDTG<- CD68plotCVDTG + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #add title 
CD68plotCVDTG <- CD68plotCVDTG +theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGCVD <-ggtexttable(CD68TGCVDsum, rows=NULL, cols=c("History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                 theme=ttheme("classic"))

#CD68+(TS) immune cell density and history_cardiovascular_disease
#summary statistics: compute the median and IQR  of CD68+ (TS) immune cell density and history_cardiovascular_disease 
CD68TSCVDsum<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Visualise the data: Create a box plot of the CD68+ (TS) immune cell density and history_cardiovascular_disease
CD68plotCVDTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD68+(TS)` ,fill=history_cardiovascular_disease)) + geom_boxplot(na.rm=T) + stat_compare_means() #run kruskal Wallis 
CD68plotCVDTS <- CD68plotCVDTS + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #add title 
CD68plotCVDTS <- CD68plotCVDTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSCVD <-ggtexttable(CD68TSCVDsum, rows=NULL, cols=c("History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                 theme=ttheme("classic"))


#Immune cell density and history_cardiovascular_disease stratified by TCS 

#CD3+(TG) immune cell density and history_cardiovascular_disease stratified by TCS  
#Perform statistical association test between CD3+(TG) immune cell density and history_cardiovascular_disease stratified by TCS  
stat.testCD3history_cardiovascular_diseaseTCS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseaseTCS #View results 
#summary statistics: compute the median and SD of CD3+ (TG) immune cell density and history_cardiovascular_disease stratified TCS
CD3TGCVDsumTCS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease))%>% group_by(history_cardiovascular_disease, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+ (TG) immune cell density and history_cardiovascular_disease stratified by TCS 
CD3plothistory_cardiovascular_diseaseTCSTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+(TG)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3plothistory_cardiovascular_diseaseTCSTG<- CD3plothistory_cardiovascular_diseaseTCSTG + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseaseTCS, label="p",  y.position= 6000) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseaseTCSTG <- CD3plothistory_cardiovascular_diseaseTCSTG +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGCVDTCS <-ggtexttable(CD3TGCVDsumTCS, rows=NULL, cols=c("Tumour Clinical Stage", "History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size = 7, "classic"))


#CD3+ (TS) immune cell density and history_cardiovascular_disease stratified by TCS  
#summary statistics: compute the median and sd  of CD3+ (TS) immune cell density and history_cardiovascular_disease stratified by TCS 
CD3TSCVDsumTCS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+(TS) immune cell density and history_cardiovascular_disease stratified by TCS  
stat.testCD3history_cardiovascular_diseaseTCSTS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseaseTCSTS #View results
#Visualize the data: Create a box plot of the CD3+ (TS) immune cell density and history_cardiovascular_disease stratified  by TCS
CD3plothistory_cardiovascular_diseaseTCSTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+(TS)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3plothistory_cardiovascular_diseaseTCSTS<- CD3plothistory_cardiovascular_diseaseTCSTS + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseaseTCSTS, label="p", y.position = 920) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseaseTCSTS <- CD3plothistory_cardiovascular_diseaseTCSTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSCVDTCS <-ggtexttable(CD3TSCVDsumTCS, rows=NULL, cols=c("Tumour Clinical Stage", "History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified by TCS
#summary statistics: compute the median and sd  of CD3+CD4+ (TG) immune cell density by history_cardiovascular_disease stratified TCS
CD3CD4TGCVDsumTCS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TG) and by history_cardiovascular_disease stratified TCS
stat.testCD3CD4history_cardiovascular_diseaseTCS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+CD4+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseaseTCS #View results 
#Visualize the data: Create a box plot of the CD3+CD4+ (TG) immune cell density and history_cardiovascular_disease stratified  by TCS
CD3CD4plothistory_cardiovascular_diseaseTCSTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TG)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3CD4plothistory_cardiovascular_diseaseTCSTG <- CD3CD4plothistory_cardiovascular_diseaseTCSTG + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseaseTCS, label="p",  y.position= 4000) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseaseTCSTG <- CD3CD4plothistory_cardiovascular_diseaseTCSTG +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGCVDTCS <-ggtexttable(CD3CD4TGCVDsumTCS, rows=NULL, cols=c("Tumour Clinical Stage", "History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified by TCS
#summary statistics: compute the median and sd  of CD3+CD4+ (TG) immune cell density by history_cardiovascular_disease stratified TCS
CD3CD4TSCVDsumTCS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TS) immune cell density and history_cardiovascular_disease stratified by TCS
stat.testCD3CD4history_cardiovascular_diseaseTCSTS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD3+CD4+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseaseTCSTS #View results
#Visualize the data: Create a box plot of the CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified  by TCS
CD3CD4plothistory_cardiovascular_diseaseTCSTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TS)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD3CD4plothistory_cardiovascular_diseaseTCSTS<- CD3CD4plothistory_cardiovascular_diseaseTCSTS + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseaseTCSTS, label="p", y.position = 620) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseaseTCSTS <- CD3CD4plothistory_cardiovascular_diseaseTCSTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSCVDTCS <-ggtexttable(CD3CD4TSCVDsumTCS, rows=NULL, cols=c("Tumour Clinical Stage", "History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))

#CD68+(TG) immune cell density and history_cardiovascular_disease stratified by TCS
#summary statistics: compute the median and sd  of CD3+CD4+ (TG) immune cell density by history_cardiovascular_disease stratified TCS
CD68TGCVDsumTCS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Perform statistical association test between CD68+(TG) immune cell density and history_cardiovascular_disease stratified by TCS 
stat.testCD68history_cardiovascular_diseaseTCS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD68+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseaseTCS #View results 
#Visualize the data: Create a box plot of the CD68+ (TG) immune cell density and history_cardiovascular_disease stratified  by TCS
CD68plothistory_cardiovascular_diseaseTCSTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD68+(TG)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD68plothistory_cardiovascular_diseaseTCSTG <- CD68plothistory_cardiovascular_diseaseTCSTG + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseaseTCS, label="p",  y.position= 1500) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseaseTCSTG <- CD68plothistory_cardiovascular_diseaseTCSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGCVDTCS <-ggtexttable(CD68TGCVDsumTCS, rows=NULL, cols=c("Tumour Clinical Stage", "History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                    theme=ttheme(base_size = 7, "classic"))

#CD68+ (TS) immune cell density and history_cardiovascular_disease stratified by TCS
#summary statistics: compute the median and sd  of CD68+ (TG) immune cell density by history_cardiovascular_disease stratified TCS
CD68TSCVDsumTCS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, `Tumour Clinical Stage`) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test between CD68+(TS) immune cell density and history_cardiovascular_disease stratified by TCS 
stat.testCD68history_cardiovascular_diseaseTCSTS <- Nonlethal %>% 
  group_by(`Tumour Clinical Stage`) %>% 
  t_test(`CD68+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseaseTCSTS #View results
#Visualize the data: Create a box plot of the CD68+ (TS) immune cell density and history_cardiovascular_disease stratified  by TCS
CD68plothistory_cardiovascular_diseaseTCSTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD68+(TS)`, fill=`Tumour Clinical Stage`)) +geom_boxplot() +  facet_wrap(~`Tumour Clinical Stage`)
CD68plothistory_cardiovascular_diseaseTCSTS <- CD68plothistory_cardiovascular_diseaseTCSTS + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseaseTCSTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseaseTCSTS <- CD68plothistory_cardiovascular_diseaseTCSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSCVDTCS <-ggtexttable(CD68TSCVDsumTCS, rows=NULL, cols=c("Tumour Clinical Stage", "History of CVD", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                    theme=ttheme(base_size = 7, "classic"))

#Immune cell density and history_cardiovascular_disease stratified by GS  

#CD3+ (TG) immune cell density and history_cardiovascular_disease stratified by GS
#summary statistics: compute the median and SDof CD3+ (TG) immune cell density by history_cardiovascular_disease stratified GS
CD3TGCVDsumGS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, GS_status) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+(TG) immune cell density and history_cardiovascular_disease stratified GS
stat.testCD3history_cardiovascular_diseaseGS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseaseGS #View results 
#Visualize the data: Create a box plot of the CD3+ (TG) immune cell density and history_cardiovascular_disease stratified  by GS
CD3plothistory_cardiovascular_diseaseGSTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+(TG)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3plothistory_cardiovascular_diseaseGSTG<- CD3plothistory_cardiovascular_diseaseGSTG + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseaseGS, label="p",  y.position= 6000) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseaseGSTG <- CD3plothistory_cardiovascular_diseaseGSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGCVDGS <-ggtexttable(CD3TGCVDsumGS, rows=NULL, cols=c("History of CVD", "Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme(base_size = 7, "classic"))

#CD3+ (TS) immune cell density and history_cardiovascular_disease stratified by GS
#summary statistics: compute the median and sd  of CD3+ (TS) immune cell density by history_cardiovascular_disease stratified GS
CD3TSCVDsumGS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, GS_status) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+(TS) immune cell density and history_cardiovascular_disease stratified by GS
stat.testCD3history_cardiovascular_diseaseGSTS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseaseGSTS #View results
#Visualize the data: Create a box plot of the CD3+ (TS) immune cell density and history_cardiovascular_disease stratified  by GS
CD3plothistory_cardiovascular_diseaseGSTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+(TS)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3plothistory_cardiovascular_diseaseGSTS <- CD3plothistory_cardiovascular_diseaseGSTS + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseaseGSTS, label="p", y.position = 920) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseaseGSTS  <- CD3plothistory_cardiovascular_diseaseGSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSCVDGS <-ggtexttable(CD3TSCVDsumGS, rows=NULL, cols=c("History of CVD", "Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                  theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified by GS
#summary statistics: compute the median and sd  of CD3+CD4+ (TG) immune cell density by history_cardiovascular_disease stratified GS
CD3CD4TGCVDsumGS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, GS_status) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified GS
stat.testCD3CD4history_cardiovascular_diseaseGS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+CD4+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseaseGS #View results 
#Visualize the data: Create a box plot of the CD3+CD4+ (TG) immune cell density and history_cardiovascular_disease stratified  by GS
CD3CD4plothistory_cardiovascular_diseaseGSTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TG)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3CD4plothistory_cardiovascular_diseaseGSTG<- CD3CD4plothistory_cardiovascular_diseaseGSTG + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseaseGS, label="p",  y.position= 4000) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseaseGSTG <- CD3CD4plothistory_cardiovascular_diseaseGSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGCVDGS <-ggtexttable(CD3CD4TGCVDsumGS, rows=NULL, cols=c("History of CVD", "Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                     theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified by GS
#summary statistics: compute the median and sd  of CD3+CD4+ (TS) immune cell density by history_cardiovascular_disease stratified GS
CD3CD4TSCVDsumGS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, GS_status) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TS) immune cell density and history_cardiovascular_disease stratified GS
stat.testCD3CD4history_cardiovascular_diseaseGSTS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD3+CD4+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseaseGSTS #View results
#Visualize the data: Create a box plot of the CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified  by GS
CD3CD4plothistory_cardiovascular_diseaseGSTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TS)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD3CD4plothistory_cardiovascular_diseaseGSTS<- CD3CD4plothistory_cardiovascular_diseaseGSTS + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseaseGSTS, label="p", y.position = 620) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseaseGSTS <- CD3CD4plothistory_cardiovascular_diseaseGSTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSCVDGS <-ggtexttable(CD3CD4TSCVDsumGS, rows=NULL, cols=c("History of CVD", "Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                     theme=ttheme(base_size = 7, "classic"))

#CD68+(TG) immune cell density and history_cardiovascular_disease stratified by GS
#summary statistics: compute the median and sd  of CD68+ (TG) immune cell density by history_cardiovascular_disease stratified GS
CD68TGCVDsumGS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, GS_status) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Perform statistical association test between CD68+(TG) immune cell density and history_cardiovascular_disease stratified by GS  
stat.testCD68history_cardiovascular_diseaseGS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD68+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseaseGS #View results 
#Visualize the data: Create a box plot of the CD68+ (TG) immune cell density and history_cardiovascular_disease stratified  by GS
CD68plothistory_cardiovascular_diseaseGSTG <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD68+(TG)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD68plothistory_cardiovascular_diseaseGSTG<- CD68plothistory_cardiovascular_diseaseGSTG + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseaseGS, label="p",  y.position= 1500) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseaseGSTG <- CD68plothistory_cardiovascular_diseaseGSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGCVDGS <-ggtexttable(CD68TGCVDsumGS, rows=NULL, cols=c("History of CVD", "Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size = 7, "classic"))

#CD68+ (TS) immune cell density and history_cardiovascular_disease stratified by GS
#summary statistics: compute the median and sd  of CD68+ (TS) immune cell density by history_cardiovascular_disease stratified GS
CD68TSCVDsumGS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% group_by(history_cardiovascular_disease, GS_status) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test between CD68+(TS) immune cell density and history_cardiovascular_disease stratified GS
stat.testCD68history_cardiovascular_diseaseGSTS <- Nonlethal %>% 
  group_by(`GS_status`) %>% 
  t_test(`CD68+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseaseGSTS #View results
#Visualize the data: Create a box plot of the CD68+ (TS) immune cell density and history_cardiovascular_disease stratified  by GS
CD68plothistory_cardiovascular_diseaseGSTS <- ggplot(data=subset(Nonlethal, !is.na(history_cardiovascular_disease)), aes(x=history_cardiovascular_disease, y=`CD68+(TS)`, fill=`GS_status`)) +geom_boxplot() +  facet_wrap(~`GS_status`)
CD68plothistory_cardiovascular_diseaseGSTS<- CD68plothistory_cardiovascular_diseaseGSTS + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseaseGSTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseaseGSTS <- CD68plothistory_cardiovascular_diseaseGSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSCVDGS <-ggtexttable(CD68TSCVDsumGS, rows=NULL, cols=c("History of CVD", "Gleason Score", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size = 7, "classic"))

#Immune cell density and History of CVD stratified by PTEN_STATUS 

#CD3+ (TG) immune cell density and history_cardiovascular_disease stratified by PTEN Status 
#Perform statistical association test between CD3+(TG) immune cell density and history_cardiovascular_disease stratified by PTEN Status 
stat.testCD3history_cardiovascular_diseasePTENSTATUS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseasePTENSTATUS #View results 
#summary stats: 
CD3TGCVDsumPTENSTATUS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(PTEN_status)) %>% group_by(history_cardiovascular_disease, PTEN_status) %>%
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD3+ (TS) immune cell density and history_cardiovascular_disease stratified  by PTEN_status
CD3plothistory_cardiovascular_diseasePTENSTATUSTG <-Nonlethal %>% filter(!is.na(PTEN_status)) %>% filter(!is.na(history_cardiovascular_disease)) %>% 
  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+(TG)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3plothistory_cardiovascular_diseasePTENSTATUSTG<- CD3plothistory_cardiovascular_diseasePTENSTATUSTG + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseasePTENSTATUS, label="p",  y.position= 6000) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseasePTENSTATUSTG <- CD3plothistory_cardiovascular_diseasePTENSTATUSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGCVDPTENSTATUS <-ggtexttable(CD3TGCVDsumPTENSTATUS, rows=NULL, cols=c("History of CVD", "PTEN Status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                          theme=ttheme(base_size = 7, "classic"))

#CD3+ (TS) immune cell density and history_cardiovascular_disease stratified by PTEN Status
#Summary stats: Compute the mean and SD of CD3+(TS) density by history CVD and PTEN status  
CD3TSCVDsumPTENSTATUS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(PTEN_status)) %>% 
  group_by(history_cardiovascular_disease, PTEN_status) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+(TS) immune cell dnesity and history_cardiovascular_disease stratified by PTEN Status
stat.testCD3history_cardiovascular_diseasePTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseasePTENSTATUSTS #View results
#Visualize the data: Create a box plot of the CD3+ (TS) immune cell density and history_cardiovascular_disease stratified  by PTEN_status
CD3plothistory_cardiovascular_diseasePTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+(TS)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3plothistory_cardiovascular_diseasePTENSTATUSTS<- CD3plothistory_cardiovascular_diseasePTENSTATUSTS + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseasePTENSTATUSTS, label="p", y.position = 920) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseasePTENSTATUSTS <- CD3plothistory_cardiovascular_diseasePTENSTATUSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSCVDPTENSTATUS <-ggtexttable(CD3TSCVDsumPTENSTATUS, rows=NULL, cols=c("History of CVD", "PTEN Status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                          theme=ttheme(base_size = 7, "classic"))


#CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified by PTEN Status
#Perform statistical association test between CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified by PTEN Status
stat.testCD3CD4history_cardiovascular_diseasePTENSTATUS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>% 
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+CD4+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseasePTENSTATUS #View results 
#Summary stats: Compute the mean and SD of CD3+CD4+(TG) density by history CVD and PTEN status  
CD3CD4TGCVDsumPTENSTATUS <- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(PTEN_status)) %>% 
  group_by(history_cardiovascular_disease, PTEN_status) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")


#Visualize the data: Create a box plot of the CD3+CD4+ (TG) immune cell density and history_cardiovascular_disease stratified by PTEN_status
CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTG <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TG)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTG<- CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTG + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseasePTENSTATUS, label="p",  y.position= 4000) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTG <- CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGCVDPTENSTATUS <-ggtexttable(CD3CD4TGCVDsumPTENSTATUS, rows=NULL, cols=c("History of CVD", "PTEN Status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                             theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified by PTEN Status
#Summary stats: Compute the mean and SD of CD3+CD4+(TS) density by history CVD and PTEN status  
CD3CD4TSCVDsumPTENSTATUS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(PTEN_status)) %>% 
  group_by(history_cardiovascular_disease, PTEN_status) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TS) immune cell density and history_cardiovascular_disease stratified by PTEN Status
stat.testCD3CD4history_cardiovascular_diseasePTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`PTEN_status`) %>% 
  t_test(`CD3+CD4+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseasePTENSTATUSTS #View results
#Visualize the data: Create a box plot of the CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified by PTEN_status
CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TS)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTS<- CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTS + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseasePTENSTATUSTS, label="p", y.position = 620) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTS<- CD3CD4plothistory_cardiovascular_diseasePTENSTATUSTS +theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSCVDPTENSTATUS <-ggtexttable(CD3CD4TSCVDsumPTENSTATUS, rows=NULL, cols=c("History of CVD", "PTEN Status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                             theme=ttheme(base_size = 7, "classic"))


#CD68+(TG) immune cell density and history_cardiovascular_disease stratified by PTEN Status
#Summary stats: 
CD68TGCVDsumPTENSTATUS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(PTEN_status)) %>% 
  group_by(history_cardiovascular_disease, PTEN_status) %>% 
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Perform statistical association test between CD68+(TG) immune cell density and history_cardiovascular_disease stratified by PTEN Status 
stat.testCD68history_cardiovascular_diseasePTENSTATUS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`PTEN_status`) %>% 
  t_test(`CD68+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseasePTENSTATUS #View results
#Visualize the data: Create a box plot of the CD68+ (TG) immune cell density and history_cardiovascular_disease stratified by PTEN_status
CD68plothistory_cardiovascular_diseasePTENSTATUSTG <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD68+(TG)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD68plothistory_cardiovascular_diseasePTENSTATUSTG<- CD68plothistory_cardiovascular_diseasePTENSTATUSTG + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseasePTENSTATUS, label="p",  y.position= 1500) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseasePTENSTATUSTG <- CD68plothistory_cardiovascular_diseasePTENSTATUSTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGCVDPTENSTATUS <-ggtexttable(CD68TGCVDsumPTENSTATUS, rows=NULL, cols=c("History of CVD", "PTEN Status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                           theme=ttheme(base_size = 7, "classic"))

#CD68+ (TS) immune cell density and history_cardiovascular_disease stratified by PTEN Status
#Perform statistical association test between CD68+(TS) immune cell density and history_cardiovascular_disease stratified by PTEN Status
stat.testCD68history_cardiovascular_diseasePTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%   
  group_by(`PTEN_status`) %>% 
  t_test(`CD68+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseasePTENSTATUSTS #View results
#Summary stats: 
CD68TSCVDsumPTENSTATUS<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(PTEN_status)) %>% 
  group_by(history_cardiovascular_disease, PTEN_status) %>% 
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Visualize the data: Create a box plot of the CD68+ (TS) immune cell density and history_cardiovascular_disease stratified by PTEN_status
CD68plothistory_cardiovascular_diseasePTENSTATUSTS <- Nonlethal %>% filter(!is.na(PTEN_status)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD68+(TS)`, fill=`PTEN_status`)) +geom_boxplot() +  facet_wrap(~`PTEN_status`)
CD68plothistory_cardiovascular_diseasePTENSTATUSTS<- CD68plothistory_cardiovascular_diseasePTENSTATUSTS + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseasePTENSTATUSTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseasePTENSTATUSTS <- CD68plothistory_cardiovascular_diseasePTENSTATUSTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSCVDPTENSTATUS <-ggtexttable(CD68TSCVDsumPTENSTATUS, rows=NULL, cols=c("History of CVD", "PTEN Status", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                           theme=ttheme(base_size = 7, "classic"))

#Immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint 

#CD3+(TG) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint 
#Summary stats: 
CD3TGCVDsumERG<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(erg_cutpoint)) %>% 
  group_by(history_cardiovascular_disease, erg_cutpoint) %>% 
  get_summary_stats(`CD3+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+(TG) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
stat.testCD3history_cardiovascular_diseaseERG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseaseERG #View results 
#Visualize the data: Create a box plot of the CD3+ (TG) immune cell density and history_cardiovascular_disease stratified by ERG_Cutpoint 
CD3plothistory_cardiovascular_diseaseERGTG <-Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% filter(!is.na(history_cardiovascular_disease)) %>% 
  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+(TG)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3plothistory_cardiovascular_diseaseERGTG<- CD3plothistory_cardiovascular_diseaseERGTG + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseaseERG, label="p",  y.position= 6000) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseaseERGTG <- CD3plothistory_cardiovascular_diseaseERGTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TGCVDERG <-ggtexttable(CD3TGCVDsumERG, rows=NULL, cols=c("History of CVD", "ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size = 7, "classic"))



#CD3+ (TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint 
#Summary stats: Compute the mean and SD of CD3+(TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
CD3TSCVDsumERG<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(erg_cutpoint)) %>%  group_by(history_cardiovascular_disease, erg_cutpoint) %>%
  get_summary_stats(`CD3+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+(TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
stat.testCD3history_cardiovascular_diseaseERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3history_cardiovascular_diseaseERGTS #View results
#Visualize the data: Create a box plot of the CD3+ (TS) immune cell density and history_cardiovascular_disease stratified by ERG_Cutpoint 
CD3plothistory_cardiovascular_diseaseERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+(TS)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3plothistory_cardiovascular_diseaseERGTS<- CD3plothistory_cardiovascular_diseaseERGTS + stat_pvalue_manual(stat.testCD3history_cardiovascular_diseaseERGTS, label="p", y.position = 920) + ylab("Density of CD3+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3plothistory_cardiovascular_diseaseERGTS <- CD3plothistory_cardiovascular_diseaseERGTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3TSCVDERG <-ggtexttable(CD3TSCVDsumERG, rows=NULL, cols=c("History of CVD", "ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                   theme=ttheme(base_size = 7, "classic"))


#CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
#Summary stats: Compute the mean and SD of CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
CD3CD4TGCVDsumERG<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(erg_cutpoint)) %>%  group_by(history_cardiovascular_disease, erg_cutpoint) %>%
  get_summary_stats(`CD3+CD4+(TG)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TG) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
stat.testCD3CD4history_cardiovascular_diseaseERG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>% 
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+CD4+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseaseERG #View results 
#Visualize the data: Create a box plot of the CD3+CD4+ (TG) immune cell density and history_cardiovascular_disease stratified by ERG_Cutpoint 
CD3CD4plothistory_cardiovascular_diseaseERGTG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TG)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3CD4plothistory_cardiovascular_diseaseERGTG<- CD3CD4plothistory_cardiovascular_diseaseERGTG + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseaseERG, label="p",  y.position= 4000) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseaseERGTG <- CD3CD4plothistory_cardiovascular_diseaseERGTG + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TGCVDERG <-ggtexttable(CD3CD4TGCVDsumERG, rows=NULL, cols=c("History of CVD", "ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))

#CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
#Summary stats: Compute the mean and SD of CD3+CD4+(TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
CD3CD4TSCVDsumERG<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(erg_cutpoint)) %>% group_by(history_cardiovascular_disease, erg_cutpoint) %>%
  get_summary_stats(`CD3+CD4+(TS)`, type="median_iqr")
#Perform statistical association test between CD3+CD4+(TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint 
stat.testCD3CD4history_cardiovascular_diseaseERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD3+CD4+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD3CD4history_cardiovascular_diseaseERGTS #View results
#Visualize the data: Create a box plot of the CD3+CD4+ (TS) immune cell density and history_cardiovascular_disease stratified by ERG_Cutpoint 
CD3CD4plothistory_cardiovascular_diseaseERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD3+CD4+(TS)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD3CD4plothistory_cardiovascular_diseaseERGTS<- CD3CD4plothistory_cardiovascular_diseaseERGTS + stat_pvalue_manual(stat.testCD3CD4history_cardiovascular_diseaseERGTS, label="p", y.position = 620) + ylab("Density of CD3+CD4+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD3CD4plothistory_cardiovascular_diseaseERGTS <- CD3CD4plothistory_cardiovascular_diseaseERGTS  + theme(legend.position = "top")
#Turn into table add to graph
texttableCD3CD4TSCVDERG <-ggtexttable(CD3CD4TSCVDsumERG, rows=NULL, cols=c("History of CVD", "ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                      theme=ttheme(base_size = 7, "classic"))

#CD68+(TG)  immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
#Summary stats: Compute the mean and SD of CD3+CD4+(TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
CD68TGCVDsumERG<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(erg_cutpoint)) %>% group_by(history_cardiovascular_disease, erg_cutpoint) %>%
  get_summary_stats(`CD68+(TG)`, type="median_iqr")
#Perform statistical association test between CD68+(TG) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
stat.testCD68history_cardiovascular_diseaseERG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%  
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD68+(TG)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseaseERG #View results 
#Visualize the data: Create a box plot of the CD68+ (TG) immune cell density and history_cardiovascular_disease stratified by ERG_Cutpoint 
CD68plothistory_cardiovascular_diseaseERGTG <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD68+(TG)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD68plothistory_cardiovascular_diseaseERGTG <- CD68plothistory_cardiovascular_diseaseERGTG + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseaseERG, label="p",  y.position= 1500) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseaseERGTG  <- CD68plothistory_cardiovascular_diseaseERGTG  + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TGCVDERG <-ggtexttable(CD68TGCVDsumERG, rows=NULL, cols=c("History of CVD", "ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                    theme=ttheme(base_size = 7, "classic"))


#CD68+ (TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
#Summary stats: Compute the mean and SD of CD3+CD4+(TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
CD68TSCVDsumERG<- Nonlethal %>% filter(!is.na(history_cardiovascular_disease)) %>% filter(!is.na(erg_cutpoint)) %>% group_by(history_cardiovascular_disease, erg_cutpoint) %>%
  get_summary_stats(`CD68+(TS)`, type="median_iqr")
#Perform statistical association test between CD68+(TS) immune cell density and history_cardiovascular_disease stratified ERG_Cutpoint
stat.testCD68history_cardiovascular_diseaseERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% 
  filter(!is.na(history_cardiovascular_disease)) %>%   
  group_by(`erg_cutpoint`) %>% 
  t_test(`CD68+(TS)`~ history_cardiovascular_disease) %>% adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.testCD68history_cardiovascular_diseaseERGTS #View results
#Visualize the data: Create a box plot of the CD68+ (TS) immune cell density and history_cardiovascular_disease stratified by ERG_Cutpoint 
CD68plothistory_cardiovascular_diseaseERGTS <- Nonlethal %>% filter(!is.na(erg_cutpoint)) %>% filter(!is.na(history_cardiovascular_disease)) %>%  ggplot(aes(x=history_cardiovascular_disease, y=`CD68+(TS)`, fill=`erg_cutpoint`)) +geom_boxplot() +  facet_wrap(~`erg_cutpoint`)
CD68plothistory_cardiovascular_diseaseERGTS<- CD68plothistory_cardiovascular_diseaseERGTS + stat_pvalue_manual(stat.testCD68history_cardiovascular_diseaseERGTS, label="p", y.position = 450) + ylab("Density of CD68+ immune cell phenotype") + xlab("History of CVD") + labs(fill="History of CVD") #Add title and statistical test (i.e., p-value) into graph
CD68plothistory_cardiovascular_diseaseERGTS <- CD68plothistory_cardiovascular_diseaseERGTS + theme(legend.position = "top")
#Turn into table add to graph
texttableCD68TSCVDERG <-ggtexttable(CD68TSCVDsumERG, rows=NULL, cols=c("History of CVD", "ERG Cutpoint", "Immune Phenotype", "N", "Median (per cells/mm2)", "IQR"), 
                                    theme=ttheme(base_size = 7, "classic"))







###Part 3 (b) RR: Estimating the risk ratio of lifestyle factors and immune cell density

##RR: Lipids Reg. Drug and immune cell density 

#factor pt.taking lipids reg. drugs 
Nonlethal$Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- factor(Nonlethal$Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, levels = c("Y", "N"))
#view structure 
str(Nonlethal$Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N)

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = Nonlethal)  
summary(fitRRCD3TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #Summarize the results
tidy(fitRRCD3TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #summarize the results
tidy(fitRRCD3TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = Nonlethal)  
summary(fitRRCD3TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #Summarize the results
tidy(fitRRCD3TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #summarize the results
tidy(fitRRCD3TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = Nonlethal)  
summary(fitRRCD3CD4TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #Summarise the results
tidy(fitRRCD3CD4TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #sumrise the results
tidy(fitRRCD3CD4TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = Nonlethal)  
summary(fitRRCD3CD4TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #Summarise the results
tidy(fitRRCD3CD4TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #summarize the results 
tidy(fitRRCD3CD4TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = Nonlethal)  
summary(fitRRCD68TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #Summarise the results
tidy(fitRRCD68TGCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #sumrise the results
tidy(fitRRCD68TGdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = Nonlethal)  
summary(fitRRCD68TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #Summarise the results
tidy(fitRRCD68TSCatPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N) #summarize the results 
tidy(fitRRCD68TSdensityPt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N, exponentiate = T) #access the models coefficient

####Lipids Reg. drugs Stratified by Tumour Clinical stage###

#Need to subset out Stage 1 and stage 2 patients
ST1patients <- subset(Nonlethal, Nonlethal$`Tumour Clinical Stage`=="ST1")
ST2patients <- subset(Nonlethal, Nonlethal$`Tumour Clinical Stage`=="ST2")
#Need to subset out stage 3 patients 
ST3patients <- subset(Nonlethal, Nonlethal$`Tumour Clinical Stage`=="ST3")

##RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by Stage 1: 

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = ST1patients)  
summary(fitRRCD3TGCatLipidsST1patients) #Summarize the results
tidy(fitRRCD3TGCatLipidsST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsST1patients) #summarize the results
tidy(fitRRCD3TGCatLipidsST1patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = ST1patients)  
summary(fitRRCD3TSCatLipidsST1patients) #Summarize the results
tidy(fitRRCD3TSCatLipidsST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsST1patients) #summarize the results
tidy(fitRRCD3TSCatLipidsST1patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = ST1patients)  
summary(fitRRCD3CD4TGCatLipidsST1patients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsST1patients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsST1patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = ST1patients)  
summary(fitRRCD3CD4TSCatLipidsST1patients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsST1patients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsST1patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = ST1patients)  
summary(fitRRCD68TGCatLipidsST1patients) #Summarise the results
tidy(fitRRCD68TGCatLipidsST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsST1patients) #sumrise the results
tidy(fitRRCD68TGCatLipidsST1patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = ST1patients)  
summary(fitRRCD68TSCatLipidsST1patients) #Summarise the results
tidy(fitRRCD68TSCatLipidsST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsST1patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsST1patients) #summarize the results 
tidy(fitRRCD68TSCatLipidsST1patients, exponentiate = T) #access the models coefficient


#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by Stage 2: 

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = ST2patients)  
summary(fitRRCD3TGCatLipidsST2patients) #Summarize the results
tidy(fitRRCD3TGCatLipidsST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsST2patients) #summarize the results
tidy(fitRRCD3TGCatLipidsST2patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = ST2patients, approach="all")  
summary(fitRRCD3TSCatLipidsST2patients) #Summarize the results
tidy(fitRRCD3TSCatLipidsST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsST2patients) #summarize the results
tidy(fitRRCD3TSCatLipidsST2patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = ST2patients)  
summary(fitRRCD3CD4TGCatLipidsST2patients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsST2patients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsST2patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = ST2patients)  
summary(fitRRCD3CD4TSCatLipidsST2patients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsST2patients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsST2patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = ST2patients)  
summary(fitRRCD68TGCatLipidsST2patients) #Summarise the results
tidy(fitRRCD68TGCatLipidsST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsST2patients) #sumrise the results
tidy(fitRRCD68TGCatLipidsST2patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = ST2patients)  
summary(fitRRCD68TSCatLipidsST2patients) #Summarise the results
tidy(fitRRCD68TSCatLipidsST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsST2patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsST2patients) #summarize the results 
tidy(fitRRCD68TSCatLipidsST2patients, exponentiate = T) #access the models coefficient

#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by Stage 3  

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = ST3patients)  
summary(fitRRCD3TGCatLipidsST3patients) #Summarize the results
tidy(fitRRCD3TGCatLipidsST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsST3patients) #summarize the results
tidy(fitRRCD3TGCatLipidsST3patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = ST3patients)  
summary(fitRRCD3TSCatLipidsST3patients) #Summarize the results
tidy(fitRRCD3TSCatLipidsST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsST3patients) #summarize the results
tidy(fitRRCD3TSCatLipidsST3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = ST3patients)  
summary(fitRRCD3CD4TGCatLipidsST3patients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsST3patients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsST3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = ST3patients)  
summary(fitRRCD3CD4TSCatLipidsST3patients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsST3patients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsST3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = ST3patients)  
summary(fitRRCD68TGCatLipidsST3patients) #Summarise the results
tidy(fitRRCD68TGCatLipidsST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsST3patients) #sumrise the results
tidy(fitRRCD68TGCatLipidsST3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = ST3patients)  
summary(fitRRCD68TSCatLipidsST3patients) #Summarise the results
tidy(fitRRCD68TSCatLipidsST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsST3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsST3patients) #summarize the results 
tidy(fitRRCD68TSCatLipidsST3patients, exponentiate = T) #access the models coefficient

###RR: Estimating the RR Immune cell densities and outcome Lipids reg. drugs stratified by GS_status 

#Subset out 3+4 patients 
G3_4patients <- subset(Nonlethal, Nonlethal$`Gleason primary pattern`==3)
#Subset out 4+3 patients 
G4_3patients <- subset(Nonlethal, Nonlethal$`Gleason primary pattern`==4)

#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by Gleason Score 3+4

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = G3_4patients)  
summary(fitRRCD3TGCatLipidsG3_4patients) #Summarize the results
tidy(fitRRCD3TGCatLipidsG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsG3_4patients) #summarize the results
tidy(fitRRCD3TGCatLipidsG3_4patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = G3_4patients)  
summary(fitRRCD3TSCatLipidsG3_4patients) #Summarize the results
tidy(fitRRCD3TSCatLipidsG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsG3_4patients) #summarize the results
tidy(fitRRCD3TSCatLipidsG3_4patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = G3_4patients)  
summary(fitRRCD3CD4TGCatLipidsG3_4patients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsG3_4patients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsG3_4patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = G3_4patients)  
summary(fitRRCD3CD4TSCatLipidsG3_4patients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsG3_4patients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsG3_4patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = G3_4patients)  
summary(fitRRCD68TGCatLipidsG3_4patients) #Summarise the results
tidy(fitRRCD68TGCatLipidsG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsG3_4patients) #sumrise the results
tidy(fitRRCD68TGCatLipidsG3_4patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = G3_4patients)  
summary(fitRRCD68TSCatLipidsG3_4patients) #Summarise the results
tidy(fitRRCD68TSCatLipidsG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsG3_4patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsG3_4patients) #summarize the results 
tidy(fitRRCD68TSCatLipidsG3_4patients, exponentiate = T) #access the models coefficient

#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by Gleason Score 4+3 :

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = G4_3patients)  
summary(fitRRCD3TGCatLipidsG4_3patients) #Summarize the results
tidy(fitRRCD3TGCatLipidsG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsG4_3patients) #summarize the results
tidy(fitRRCD3TGCatLipidsG4_3patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = G4_3patients)  
summary(fitRRCD3TSCatLipidsG4_3patients) #Summarize the results
tidy(fitRRCD3TSCatLipidsG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsG4_3patients) #summarize the results
tidy(fitRRCD3TSCatLipidsG4_3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = G4_3patients)  
summary(fitRRCD3CD4TGCatLipidsG4_3patients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsG4_3patients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsG4_3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = G4_3patients)  
summary(fitRRCD3CD4TSCatLipidsG4_3patients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsG4_3patients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsG4_3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = G4_3patients)  
summary(fitRRCD68TGCatLipidsG4_3patients) #Summarise the results
tidy(fitRRCD68TGCatLipidsG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsG4_3patients) #sumrise the results
tidy(fitRRCD68TGCatLipidsG4_3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = G4_3patients)  
summary(fitRRCD68TSCatLipidsG4_3patients) #Summarise the results
tidy(fitRRCD68TSCatLipidsG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsG4_3patients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsG4_3patients) #summarize the results 
tidy(fitRRCD68TSCatLipidsG4_3patients, exponentiate = T) #access the models coefficient


###RR: Estimating the RR Immune cell densities and outcome Lipids reg. drugs stratified by PTEN_status 

#Subset out PTEN_null patients 
PTEN_nullpatients <- subset(Nonlethal, Nonlethal$PTEN_status=="PTEN-null")
#Subset out PTEN_intact patients  
PTEN_intactpatients <- subset(Nonlethal, Nonlethal$PTEN_status=="PTEN-intact")

#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by PTEN_intact:


#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = PTEN_intactpatients)  
summary(fitRRCD3TGCatLipidsPTEN_intactpatients) #Summarize the results
tidy(fitRRCD3TGCatLipidsPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsPTEN_intactpatients) #summarize the results
tidy(fitRRCD3TGCatLipidsPTEN_intactpatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = PTEN_intactpatients)  
summary(fitRRCD3TSCatLipidsPTEN_intactpatients) #Summarize the results
tidy(fitRRCD3TSCatLipidsPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsPTEN_intactpatients) #summarize the results
tidy(fitRRCD3TSCatLipidsPTEN_intactpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = PTEN_intactpatients)  
summary(fitRRCD3CD4TGCatLipidsPTEN_intactpatients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsPTEN_intactpatients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsPTEN_intactpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = PTEN_intactpatients)  
summary(fitRRCD3CD4TSCatLipidsPTEN_intactpatients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsPTEN_intactpatients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsPTEN_intactpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = PTEN_intactpatients)  
summary(fitRRCD68TGCatLipidsPTEN_intactpatients) #Summarise the results
tidy(fitRRCD68TGCatLipidsPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsPTEN_intactpatients) #sumrise the results
tidy(fitRRCD68TGCatLipidsPTEN_intactpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = PTEN_intactpatients)  
summary(fitRRCD68TSCatLipidsPTEN_intactpatients) #Summarise the results
tidy(fitRRCD68TSCatLipidsPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsPTEN_intactpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsPTEN_intactpatients) #summarize the results 
tidy(fitRRCD68TSCatLipidsPTEN_intactpatients, exponentiate = T) #access the models coefficient


#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by PTEN_null:

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = PTEN_nullpatients)  
summary(fitRRCD3TGCatLipidsPTEN_nullpatients) #Summarize the results
tidy(fitRRCD3TGCatLipidsPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsPTEN_nullpatients) #summarize the results
tidy(fitRRCD3TGCatLipidsPTEN_nullpatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = PTEN_nullpatients)  
summary(fitRRCD3TSCatLipidsPTEN_nullpatients) #Summarize the results
tidy(fitRRCD3TSCatLipidsPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsPTEN_nullpatients) #summarize the results
tidy(fitRRCD3TSCatLipidsPTEN_nullpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = PTEN_nullpatients)  
summary(fitRRCD3CD4TGCatLipidsPTEN_nullpatients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsPTEN_nullpatients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsPTEN_nullpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = PTEN_nullpatients)  
summary(fitRRCD3CD4TSCatLipidsPTEN_nullpatients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsPTEN_nullpatients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsPTEN_nullpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = PTEN_nullpatients)  
summary(fitRRCD68TGCatLipidsPTEN_nullpatients) #Summarise the results
tidy(fitRRCD68TGCatLipidsPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsPTEN_nullpatients) #sumrise the results
tidy(fitRRCD68TGCatLipidsPTEN_nullpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = PTEN_nullpatients)  
summary(fitRRCD68TSCatLipidsPTEN_nullpatients) #Summarise the results
tidy(fitRRCD68TSCatLipidsPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsPTEN_nullpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsPTEN_nullpatients) #summarize the results 
tidy(fitRRCD68TSCatLipidsPTEN_nullpatients, exponentiate = T) #access the models coefficient


#Subset out ERG_neg patients 
ERG_negpatients <- subset(Nonlethal, Nonlethal$erg_cutpoint=="ERG_negative")
#Subset out ERG_positivepatients patients  
ERG_positivepatients <- subset(Nonlethal, Nonlethal$erg_cutpoint=="ERG_positive")

#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by ERG_positivepatients:

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = ERG_positivepatients)  
summary(fitRRCD3TGCatLipidsERG_positivepatients) #Summarize the results
tidy(fitRRCD3TGCatLipidsERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsERG_positivepatients) #summarize the results
tidy(fitRRCD3TGCatLipidsERG_positivepatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = ERG_positivepatients)  
summary(fitRRCD3TSCatLipidsERG_positivepatients) #Summarize the results
tidy(fitRRCD3TSCatLipidsERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsERG_positivepatients) #summarize the results
tidy(fitRRCD3TSCatLipidsERG_positivepatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = ERG_positivepatients)  
summary(fitRRCD3CD4TGCatLipidsERG_positivepatients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsERG_positivepatients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsERG_positivepatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = ERG_positivepatients)  
summary(fitRRCD3CD4TSCatLipidsERG_positivepatients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsERG_positivepatients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsERG_positivepatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = ERG_positivepatients)  
summary(fitRRCD68TGCatLipidsERG_positivepatients) #Summarise the results
tidy(fitRRCD68TGCatLipidsERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsERG_positivepatients) #sumrise the results
tidy(fitRRCD68TGCatLipidsERG_positivepatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = ERG_positivepatients)  
summary(fitRRCD68TSCatLipidsERG_positivepatients) #Summarise the results
tidy(fitRRCD68TSCatLipidsERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsERG_positivepatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsERG_positivepatients) #summarize the results 
tidy(fitRRCD68TSCatLipidsERG_positivepatients, exponentiate = T) #access the models coefficient


#RR: Estimating the RR Immune cell density on lipid. reg drugs outcome stratified by ERG_neg:

#Perform RR analysis: CD3+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTG, data = ERG_negpatients)  
summary(fitRRCD3TGCatLipidsERG_negpatients) #Summarize the results
tidy(fitRRCD3TGCatLipidsERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TGCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TG)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3TGCatLipidsERG_negpatients) #summarize the results
tidy(fitRRCD3TGCatLipidsERG_negpatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3positivemedianTS, data = ERG_negpatients)  
summary(fitRRCD3TSCatLipidsERG_negpatients) #Summarize the results
tidy(fitRRCD3TSCatLipidsERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3TSCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+(TS)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3TSCatLipidsERG_negpatients) #summarize the results
tidy(fitRRCD3TSCatLipidsERG_negpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTG, data = ERG_negpatients)  
summary(fitRRCD3CD4TGCatLipidsERG_negpatients) #Summarise the results
tidy(fitRRCD3CD4TGCatLipidsERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TGCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TG)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatLipidsERG_negpatients) #sumrise the results
tidy(fitRRCD3CD4TGCatLipidsERG_negpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD3CD4positivemedianTS, data = ERG_negpatients)  
summary(fitRRCD3CD4TSCatLipidsERG_negpatients) #Summarise the results
tidy(fitRRCD3CD4TSCatLipidsERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD3CD4TSCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD3+CD4+(TS)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatLipidsERG_negpatients) #summarize the results 
tidy(fitRRCD3CD4TSCatLipidsERG_negpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTG, data = ERG_negpatients)  
summary(fitRRCD68TGCatLipidsERG_negpatients) #Summarise the results
tidy(fitRRCD68TGCatLipidsERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TGCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TG)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD68TGCatLipidsERG_negpatients) #sumrise the results
tidy(fitRRCD68TGCatLipidsERG_negpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ CD68positivemedianTS, data = ERG_negpatients)  
summary(fitRRCD68TSCatLipidsERG_negpatients) #Summarise the results
tidy(fitRRCD68TSCatLipidsERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N (i.e., binary outcome Lipids reg. drugs (Y))
fitRRCD68TSCatLipidsERG_negpatients <- riskratio(formula = Pt.taking.BNF65.category.2.12.Lipid.regulating.drugs.at.time.of.diagnosis.or.prior.to.diagnosis...Y.N ~ `CD68+(TS)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD68TSCatLipidsERG_negpatients) #summarize the results 
tidy(fitRRCD68TSCatLipidsERG_negpatients, exponentiate = T) #access the models coefficient



###Part 3 (b) RR: Estimating the risk ratio of lifestyle factors and immune cell density


##RR: history_cardiovascular_disease and immune cell density 

#factor history_cardiovascular_disease  
Nonlethal$history_cardiovascular_disease <- factor(Nonlethal$history_cardiovascular_disease, levels = c("yes", "no"))
#view structure 
str(Nonlethal$history_cardiovascular_disease)

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCathistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = Nonlethal)  
summary(fitRRCD3TGCathistory_cardiovascular_disease) #Summarize the results
tidy(fitRRCD3TGCathistory_cardiovascular_disease, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGdensityhistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TGdensityhistory_cardiovascular_disease) #summarize the results
tidy(fitRRCD3TGdensityhistory_cardiovascular_disease, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCathistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = Nonlethal)  
summary(fitRRCD3TSCathistory_cardiovascular_disease) #Summarize the results
tidy(fitRRCD3TSCathistory_cardiovascular_disease, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSdensityhistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3TSdensityhistory_cardiovascular_disease) #summarize the results
tidy(fitRRCD3TSdensityhistory_cardiovascular_disease, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCathistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = Nonlethal)  
summary(fitRRCD3CD4TGCathistory_cardiovascular_disease) #Summarise the results
tidy(fitRRCD3CD4TGCathistory_cardiovascular_disease, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGdensityhistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TGdensityhistory_cardiovascular_disease) #sumrise the results
tidy(fitRRCD3CD4TGdensityhistory_cardiovascular_disease, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCathistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = Nonlethal)  
summary(fitRRCD3CD4TSCathistory_cardiovascular_disease) #Summarise the results
tidy(fitRRCD3CD4TSCathistory_cardiovascular_disease, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSdensityhistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD3CD4TSdensityhistory_cardiovascular_disease) #summarize the results 
tidy(fitRRCD3CD4TSdensityhistory_cardiovascular_disease, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCathistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = Nonlethal)  
summary(fitRRCD68TGCathistory_cardiovascular_disease) #Summarise the results
tidy(fitRRCD68TGCathistory_cardiovascular_disease, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGdensityhistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TGdensityhistory_cardiovascular_disease) #sumrise the results
tidy(fitRRCD68TGdensityhistory_cardiovascular_disease, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCathistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = Nonlethal)  
summary(fitRRCD68TSCathistory_cardiovascular_disease) #Summarise the results
tidy(fitRRCD68TSCathistory_cardiovascular_disease, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSdensityhistory_cardiovascular_disease <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = Nonlethal, approach = "logistic") 
summary(fitRRCD68TSdensityhistory_cardiovascular_disease) #summarize the results 
tidy(fitRRCD68TSdensityhistory_cardiovascular_disease, exponentiate = T) #access the models coefficient

####history_cardiovascular_disease Stratified by Tumour Clinical stage###


##RR: Estimating the RR Immune cell density on history_cardiovascular_disease (Y) outcome stratified by Stage 1: 

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = ST1patients)  
summary(fitRRCD3TGCatHCVDST1patients) #Summarize the results
tidy(fitRRCD3TGCatHCVDST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDST1patients) #summarize the results
tidy(fitRRCD3TGCatHCVDST1patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = ST1patients)  
summary(fitRRCD3TSCatHCVDST1patients) #Summarize the results
tidy(fitRRCD3TSCatHCVDST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDST1patients) #summarize the results
tidy(fitRRCD3TSCatHCVDST1patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = ST1patients)  
summary(fitRRCD3CD4TGCatHCVDST1patients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDST1patients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDST1patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = ST1patients)  
summary(fitRRCD3CD4TSCatHCVDST1patients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDST1patients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDST1patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = ST1patients)  
summary(fitRRCD68TGCatHCVDST1patients) #Summarise the results
tidy(fitRRCD68TGCatHCVDST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDST1patients) #sumrise the results
tidy(fitRRCD68TGCatHCVDST1patients, exponentiate = T) #access the models 

#PerformRR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = ST1patients)  
summary(fitRRCD68TSCatHCVDST1patients) #Summarise the results
tidy(fitRRCD68TSCatHCVDST1patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDST1patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = ST1patients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDST1patients) #summarize the results 
tidy(fitRRCD68TSCatHCVDST1patients, exponentiate = T) #access the models coefficient


#RR: Estimating the RR Immune cell density on history_cardiovascular_disease (Y) outcome stratified by Stage 2: 

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = ST2patients)  
summary(fitRRCD3TGCatHCVDST2patients) #Summarize the results
tidy(fitRRCD3TGCatHCVDST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDST2patients) #summarize the results
tidy(fitRRCD3TGCatHCVDST2patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = ST2patients, approach = "all")  
summary(fitRRCD3TSCatHCVDST2patients) #Summarize the results
tidy(fitRRCD3TSCatHCVDST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDST2patients) #summarize the results
tidy(fitRRCD3TSCatHCVDST2patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = ST2patients)  
summary(fitRRCD3CD4TGCatHCVDST2patients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDST2patients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDST2patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = ST2patients)  
summary(fitRRCD3CD4TSCatHCVDST2patients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDST2patients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDST2patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = ST2patients)  
summary(fitRRCD68TGCatHCVDST2patients) #Summarise the results
tidy(fitRRCD68TGCatHCVDST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDST2patients) #sumrise the results
tidy(fitRRCD68TGCatHCVDST2patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = ST2patients)  
summary(fitRRCD68TSCatHCVDST2patients) #Summarise the results
tidy(fitRRCD68TSCatHCVDST2patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDST2patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = ST2patients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDST2patients) #summarize the results 
tidy(fitRRCD68TSCatHCVDST2patients, exponentiate = T) #access the models coefficient

#RR: Estimating the RR Immune cell density on history_cardiovascular_disease outcome stratified by Stage 3  

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = ST3patients)  
summary(fitRRCD3TGCatHCVDST3patients) #Summarize the results
tidy(fitRRCD3TGCatHCVDST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDST3patients) #summarize the results
tidy(fitRRCD3TGCatHCVDST3patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = ST3patients)  
summary(fitRRCD3TSCatHCVDST3patients) #Summarize the results
tidy(fitRRCD3TSCatHCVDST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDST3patients) #summarize the results
tidy(fitRRCD3TSCatHCVDST3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = ST3patients)  
summary(fitRRCD3CD4TGCatHCVDST3patients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDST3patients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDST3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = ST3patients)  
summary(fitRRCD3CD4TSCatHCVDST3patients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDST3patients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDST3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = ST3patients)  
summary(fitRRCD68TGCatHCVDST3patients) #Summarise the results
tidy(fitRRCD68TGCatHCVDST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDST3patients) #sumrise the results
tidy(fitRRCD68TGCatHCVDST3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = ST3patients)  
summary(fitRRCD68TSCatHCVDST3patients) #Summarise the results
tidy(fitRRCD68TSCatHCVDST3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDST3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = ST3patients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDST3patients) #summarize the results 
tidy(fitRRCD68TSCatHCVDST3patients, exponentiate = T) #access the models coefficient

###RR: Estimating the RR Immune cell densities and outcome history_cardiovascular_disease stratified by GS_status 

#Subset out 3+4 patients 
G3_4patients <- subset(Nonlethal, Nonlethal$`Gleason primary pattern`==3)
#Subset out 4+3 patients 
G4_3patients <- subset(Nonlethal, Nonlethal$`Gleason primary pattern`==4)

#RR: Estimating the RR Immune cell density on history_cardiovascular_disease outcome stratified by Gleason Score 3+4

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = G3_4patients)  
summary(fitRRCD3TGCatHCVDG3_4patients) #Summarize the results
tidy(fitRRCD3TGCatHCVDG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDG3_4patients) #summarize the results
tidy(fitRRCD3TGCatHCVDG3_4patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = G3_4patients)  
summary(fitRRCD3TSCatHCVDG3_4patients) #Summarize the results
tidy(fitRRCD3TSCatHCVDG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDG3_4patients) #summarize the results
tidy(fitRRCD3TSCatHCVDG3_4patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = G3_4patients)  
summary(fitRRCD3CD4TGCatHCVDG3_4patients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDG3_4patients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDG3_4patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = G3_4patients)  
summary(fitRRCD3CD4TSCatHCVDG3_4patients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDG3_4patients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDG3_4patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = G3_4patients)  
summary(fitRRCD68TGCatHCVDG3_4patients) #Summarise the results
tidy(fitRRCD68TGCatHCVDG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDG3_4patients) #sumrise the results
tidy(fitRRCD68TGCatHCVDG3_4patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = G3_4patients)  
summary(fitRRCD68TSCatHCVDG3_4patients) #Summarise the results
tidy(fitRRCD68TSCatHCVDG3_4patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDG3_4patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = G3_4patients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDG3_4patients) #summarize the results 
tidy(fitRRCD68TSCatHCVDG3_4patients, exponentiate = T) #access the models coefficient

#RR: Estimating the RR Immune cell density on history_cardiovascular_disease outcome stratified by Gleason Score 4+3 :

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = G4_3patients)  
summary(fitRRCD3TGCatHCVDG4_3patients) #Summarize the results
tidy(fitRRCD3TGCatHCVDG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDG4_3patients) #summarize the results
tidy(fitRRCD3TGCatHCVDG4_3patients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = G4_3patients)  
summary(fitRRCD3TSCatHCVDG4_3patients) #Summarize the results
tidy(fitRRCD3TSCatHCVDG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDG4_3patients) #summarize the results
tidy(fitRRCD3TSCatHCVDG4_3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = G4_3patients)  
summary(fitRRCD3CD4TGCatHCVDG4_3patients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDG4_3patients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDG4_3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = G4_3patients)  
summary(fitRRCD3CD4TSCatHCVDG4_3patients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDG4_3patients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDG4_3patients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = G4_3patients)  
summary(fitRRCD68TGCatHCVDG4_3patients) #Summarise the results
tidy(fitRRCD68TGCatHCVDG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDG4_3patients) #sumrise the results
tidy(fitRRCD68TGCatHCVDG4_3patients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = G4_3patients)  
summary(fitRRCD68TSCatHCVDG4_3patients) #Summarise the results
tidy(fitRRCD68TSCatHCVDG4_3patients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDG4_3patients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = G4_3patients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDG4_3patients) #summarize the results 
tidy(fitRRCD68TSCatHCVDG4_3patients, exponentiate = T) #access the models coefficient


###RR: Estimating the RR Immune cell densities and outcome history_cardiovascular_disease stratified by PTEN_status 

#Subset out PTEN_null patients 
PTEN_nullpatients <- subset(Nonlethal, Nonlethal$PTEN_status=="PTEN-null")
#Subset out PTEN_intact patients  
PTEN_intactpatients <- subset(Nonlethal, Nonlethal$PTEN_status=="PTEN-intact")

#RR: Estimating the RR Immune cell density on history_cardiovascular_disease outcome stratified by PTEN_intact:

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = PTEN_intactpatients)  
summary(fitRRCD3TGCatHCVDPTEN_intactpatients) #Summarize the results
tidy(fitRRCD3TGCatHCVDPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDPTEN_intactpatients) #summarize the results
tidy(fitRRCD3TGCatHCVDPTEN_intactpatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = PTEN_intactpatients)  
summary(fitRRCD3TSCatHCVDPTEN_intactpatients) #Summarize the results
tidy(fitRRCD3TSCatHCVDPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDPTEN_intactpatients) #summarize the results
tidy(fitRRCD3TSCatHCVDPTEN_intactpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = PTEN_intactpatients)  
summary(fitRRCD3CD4TGCatHCVDPTEN_intactpatients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDPTEN_intactpatients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDPTEN_intactpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = PTEN_intactpatients)  
summary(fitRRCD3CD4TSCatHCVDPTEN_intactpatients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDPTEN_intactpatients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDPTEN_intactpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = PTEN_intactpatients)  
summary(fitRRCD68TGCatHCVDPTEN_intactpatients) #Summarise the results
tidy(fitRRCD68TGCatHCVDPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDPTEN_intactpatients) #sumrise the results
tidy(fitRRCD68TGCatHCVDPTEN_intactpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = PTEN_intactpatients)  
summary(fitRRCD68TSCatHCVDPTEN_intactpatients) #Summarise the results
tidy(fitRRCD68TSCatHCVDPTEN_intactpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDPTEN_intactpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = PTEN_intactpatients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDPTEN_intactpatients) #summarize the results 
tidy(fitRRCD68TSCatHCVDPTEN_intactpatients, exponentiate = T) #access the models coefficient


#RR: Estimating the RR Immune cell density on history_cardiovascular_disease outcome stratified by PTEN_null:

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = PTEN_nullpatients)  
summary(fitRRCD3TGCatHCVDPTEN_nullpatients) #Summarize the results
tidy(fitRRCD3TGCatHCVDPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TGCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDPTEN_nullpatients) #summarize the results
tidy(fitRRCD3TGCatHCVDPTEN_nullpatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = PTEN_nullpatients)  
summary(fitRRCD3TSCatHCVDPTEN_nullpatients) #Summarize the results
tidy(fitRRCD3TSCatHCVDPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3TSCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDPTEN_nullpatients) #summarize the results
tidy(fitRRCD3TSCatHCVDPTEN_nullpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = PTEN_nullpatients)  
summary(fitRRCD3CD4TGCatHCVDPTEN_nullpatients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TGCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDPTEN_nullpatients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDPTEN_nullpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = PTEN_nullpatients)  
summary(fitRRCD3CD4TSCatHCVDPTEN_nullpatients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD3CD4TSCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDPTEN_nullpatients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDPTEN_nullpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = PTEN_nullpatients)  
summary(fitRRCD68TGCatHCVDPTEN_nullpatients) #Summarise the results
tidy(fitRRCD68TGCatHCVDPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TGCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDPTEN_nullpatients) #sumrise the results
tidy(fitRRCD68TGCatHCVDPTEN_nullpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = PTEN_nullpatients)  
summary(fitRRCD68TSCatHCVDPTEN_nullpatients) #Summarise the results
tidy(fitRRCD68TSCatHCVDPTEN_nullpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome history_cardiovascular_disease (Y))
fitRRCD68TSCatHCVDPTEN_nullpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = PTEN_nullpatients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDPTEN_nullpatients) #summarize the results 
tidy(fitRRCD68TSCatHCVDPTEN_nullpatients, exponentiate = T) #access the models coefficient

#RR: Estimating the RR Immune cell density on histoyr of CVD stratified by ERG_neg:

#Subset out ERG_neg patients 
ERG_negpatients <- subset(Nonlethal, Nonlethal$erg_cutpoint=="ERG_negative")
#Subset out ERG_positivepatients patients  
ERG_positivepatients <- subset(Nonlethal, Nonlethal$erg_cutpoint=="ERG_positive")

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TGCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = ERG_negpatients)  
summary(fitRRCD3TGCatHCVDERG_negpatients) #Summarize the results
tidy(fitRRCD3TGCatHCVDERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TGCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDERG_negpatients) #summarize the results
tidy(fitRRCD3TGCatHCVDERG_negpatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TSCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = ERG_negpatients)  
summary(fitRRCD3TSCatHCVDERG_negpatients) #Summarize the results
tidy(fitRRCD3TSCatHCVDERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TSCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDERG_negpatients) #summarize the results
tidy(fitRRCD3TSCatHCVDERG_negpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TGCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = ERG_negpatients)  
summary(fitRRCD3CD4TGCatHCVDERG_negpatients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TGCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDERG_negpatients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDERG_negpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TSCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = ERG_negpatients)  
summary(fitRRCD3CD4TSCatHCVDERG_negpatients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TSCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDERG_negpatients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDERG_negpatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TGCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = ERG_negpatients)  
summary(fitRRCD68TGCatHCVDERG_negpatients) #Summarise the results
tidy(fitRRCD68TGCatHCVDERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TGCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDERG_negpatients) #sumrise the results
tidy(fitRRCD68TGCatHCVDERG_negpatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TSCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = ERG_negpatients)  
summary(fitRRCD68TSCatHCVDERG_negpatients) #Summarise the results
tidy(fitRRCD68TSCatHCVDERG_negpatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TSCatHCVDERG_negpatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = ERG_negpatients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDERG_negpatients) #summarize the results 
tidy(fitRRCD68TSCatHCVDERG_negpatients, exponentiate = T) #access the models coefficient

#RR: Estimating the RR Immune cell density on history of cardiovascular disease:

#Perform RR analysis: CD3+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TGCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTG, data = ERG_positivepatients)  
summary(fitRRCD3TGCatHCVDERG_positivepatients) #Summarize the results
tidy(fitRRCD3TGCatHCVDERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TGCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TG)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3TGCatHCVDERG_positivepatients) #summarize the results
tidy(fitRRCD3TGCatHCVDERG_positivepatients, exponentiate = T) #access the models coefficients 

#Perform RR analysis: CD3+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TSCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ CD3positivemedianTS, data = ERG_positivepatients)  
summary(fitRRCD3TSCatHCVDERG_positivepatients) #Summarize the results
tidy(fitRRCD3TSCatHCVDERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3TSCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+(TS)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3TSCatHCVDERG_positivepatients) #summarize the results
tidy(fitRRCD3TSCatHCVDERG_positivepatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD3+CD4+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TGCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTG, data = ERG_positivepatients)  
summary(fitRRCD3CD4TGCatHCVDERG_positivepatients) #Summarise the results
tidy(fitRRCD3CD4TGCatHCVDERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TGCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TG)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3CD4TGCatHCVDERG_positivepatients) #sumrise the results
tidy(fitRRCD3CD4TGCatHCVDERG_positivepatients, exponentiate = T) #access the models 

#Perform RR analysis: CD3+CD4+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TSCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ CD3CD4positivemedianTS, data = ERG_positivepatients)  
summary(fitRRCD3CD4TSCatHCVDERG_positivepatients) #Summarise the results
tidy(fitRRCD3CD4TSCatHCVDERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD3+CD4+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD3CD4TSCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ `CD3+CD4+(TS)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD3CD4TSCatHCVDERG_positivepatients) #summarize the results 
tidy(fitRRCD3CD4TSCatHCVDERG_positivepatients, exponentiate = T) #access the models coefficient

#Perform RR analysis: CD68+(TG) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TGCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTG, data = ERG_positivepatients)  
summary(fitRRCD68TGCatHCVDERG_positivepatients) #Summarise the results
tidy(fitRRCD68TGCatHCVDERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TG) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TGCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TG)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD68TGCatHCVDERG_positivepatients) #sumrise the results
tidy(fitRRCD68TGCatHCVDERG_positivepatients, exponentiate = T) #access the models 

#Perform RR analysis: CD68+(TS) [> or < median] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TSCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ CD68positivemedianTS, data = ERG_positivepatients)  
summary(fitRRCD68TSCatHCVDERG_positivepatients) #Summarise the results
tidy(fitRRCD68TSCatHCVDERG_positivepatients, exponentiate = TRUE) #access the models coefficient 

#Preform RR analysis: CD68+ (TS) [continuous] and history_cardiovascular_disease (i.e., binary outcome HCVD)
fitRRCD68TSCatHCVDERG_positivepatients <- riskratio(formula = history_cardiovascular_disease ~ `CD68+(TS)`, data = ERG_positivepatients, approach = "logistic") 
summary(fitRRCD68TSCatHCVDERG_positivepatients) #summarize the results 
tidy(fitRRCD68TSCatHCVDERG_positivepatients, exponentiate = T) #access the models coefficient

######################################################################################################################################################################################################################################################################################
##Part four: Spatial Distribution of Immune cell in PCa


#Initial have to perform data wrangling to get the data into a format sutiable for PhenoptRReport Shiny App 

#load R packages  
library("devtools")
library(phenoptr)
library(tidyverse)
library(readxl)
library(openxlsx)
library(phenoptrReports)
library(readxl)
library(readr)


#Import all file for CRU00291689_001_Scane1_ome 
Copy_of_mIF_test_CRU00291689_001_Scan1_tumour_stroma <- read_excel("PhenoptR/CDU00291689-001_Scan1ome.tif/Copy of mIF test-CRU00291689-001_Scan1-tumour stroma.xlsx")
View(Copy_of_mIF_test_CRU00291689_001_Scan1_tumour_stroma)#View data
mIF_test <- read_excel("PhenoptR/CDU00291689-001_Scan1ome.tif/mIF test.xlsx")
View(mIF_test) #View data
mIF_test_CRU00291689_001_Scan1_tumour_stroma_CD68_1 <- read_excel("PhenoptR/CDU00291689-001_Scan1ome.tif/mIF test-CRU00291689-001_Scan1-tumour stroma-CD68 1.xlsx")
View(mIF_test_CRU00291689_001_Scan1_tumour_stroma_CD68_1)  #View data                                          
mIF_test_CRU00291689_001_Scan1_tumour_gland_CD68 <- read_excel("PhenoptR/CDU00291689-001_Scan1ome.tif/mIF test-CRU00291689-001_Scan1-tumour gland-CD68.xlsx")
View(mIF_test_CRU00291689_001_Scan1_tumour_gland_CD68) #View data
#Merge all files for CRU00291689_001_Scan1_ome 
CRU00291689_001_Scan1_merge <- rbind(Copy_of_mIF_test_CRU00291689_001_Scan1_tumour_stroma, mIF_test)
CRU00291689_001_Scan1_merge <- rbind(CRU00291689_001_Scan1_merge, mIF_test_CRU00291689_001_Scan1_tumour_gland_CD68)
CRU00291689_001_Scan1_merge <- rbind(CRU00291689_001_Scan1_merge, mIF_test_CRU00291689_001_Scan1_tumour_stroma_CD68_1)
#View the merge file 
View(CRU00291689_001_Scan1_merge)
#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR)
write.xlsx(CRU00291689_001_Scan1_merge, "CRU00291689_001_Scan1_merge.xlsx")
#Import Excel file compatible with phenoptR  
CRU00291689_001_Scan1_merge_PhenoptRFormat <- read_excel("PhenoptR/CDU00291689-001_Scan1ome.tif/CRU00291689_001_Scan1_merge_PhenoptRFormat.xlsx")
View(CRU00291689_001_Scan1_merge_PhenoptRFormat) #view the data
#View all Phenotypes 
table(CRU00291689_001_Scan1_merge_PhenoptRFormat$`Tissue Category`, CRU00291689_001_Scan1_merge_PhenoptRFormat$Phenotype)

#clean the data: 

#replace PathCellObject with other 
CRU00291689_001_Scan1_merge_PhenoptRFormat <-replace(CRU00291689_001_Scan1_merge_PhenoptRFormat, CRU00291689_001_Scan1_merge_PhenoptRFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00291689_001_Scan1_merge_PhenoptRFormat<- replace(CRU00291689_001_Scan1_merge_PhenoptRFormat, CRU00291689_001_Scan1_merge_PhenoptRFormat=="CRU00291689-001_Scan1.ome.tif (CD68)", "CRU00291689-001_Scan1.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00291689_001_Scan1_merge_PhenoptRFormat<- CRU00291689_001_Scan1_merge_PhenoptRFormat %>% filter(Phenotype!="other")


#Split the phenotypes into there own column (ie. Add column for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00291689_001_Scan1_merge_PhenoptRFormat<- CRU00291689_001_Scan1_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00291689_001_Scan1_merge_PhenoptRFormat<- CRU00291689_001_Scan1_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00291689_001_Scan1_merge_PhenoptRFormat<- CRU00291689_001_Scan1_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00291689_001_Scan1_merge_PhenoptRFormat<- CRU00291689_001_Scan1_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00291689_001_Scan1_merge_PhenoptRFormat<- CRU00291689_001_Scan1_merge_PhenoptRFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00291689_001_Scan1_merge_PhenoptRFormat<- CRU00291689_001_Scan1_merge_PhenoptRFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))  

#View split Column data file 
View(CRU00291689_001_Scan1_merge_PhenoptRFormat)
#save file tow orking directory 
write.csv(CRU00291689_001_Scan1_merge_PhenoptRFormat, file="CRU00291689_001_Scan1_merge_PhenoptRFormat_splitphenotypes.csv") #save as csv

#####CRU00290792_001 MIF Data####

#Import files: 
CRU00290792_001_tumour_gland <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290792-001/CRU00290792-001--tumour gland.xlsx")
View(CRU00290792_001_tumour_gland) #View to check R imported correctly 
CRU00290792_001_tumour_gland_CD68 <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290792-001/CRU00290792-001--tumour gland-CD68.xlsx")
View(CRU00290792_001_tumour_gland_CD68) #View to check R imported correctly 
CRU00290792_001_tumour_stroma <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290792-001/CRU00290792-001--tumour stroma.xlsx")
View(CRU00290792_001_tumour_stroma) #View to check R imported correctly 
CRU00290792_001_tumour_stroma_CD68 <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290792-001/CRU00290792-001--tumour stroma-CD68.xlsx")
View(CRU00290792_001_tumour_stroma_CD68) #View to check R imported correctly 

#Merge files for CRU00290792_001
CRU00290792_001_merge<- rbind(CRU00290792_001_tumour_gland, CRU00290792_001_tumour_gland_CD68)
CRU00290792_001_merge <- rbind(CRU00290792_001_merge, CRU00290792_001_tumour_stroma)
CRU00290792_001_merge <- rbind(CRU00290792_001_merge,CRU00290792_001_tumour_stroma_CD68 )
#View the merge file 
View(CRU00290792_001_merge)
#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR)
write.xlsx(CRU00290792_001_merge, "CRU00290792_001_merge.xlsx")

#Import Excel file compatible with phenoptR  
CRU00290792_001_merge_PhenoptRFormat <- read_excel("PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290792-001/CRU00290792_001_merge_PhenoptRFormat.xlsx")
View(CRU00290792_001_merge_PhenoptRFormat) #View the data 
#View all Phenotypes 
table(CRU00290792_001_merge_PhenoptRFormat$`Tissue Category`, CRU00290792_001_merge_PhenoptRFormat$Phenotype)
#clean the data:
#replace PathCellObject with other 
CRU00290792_001_merge_PhenoptRFormat <-replace(CRU00290792_001_merge_PhenoptRFormat, CRU00290792_001_merge_PhenoptRFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00290792_001_merge_PhenoptRFormat<- replace(CRU00290792_001_merge_PhenoptRFormat, CRU00290792_001_merge_PhenoptRFormat=="CRU00291689-001_Scan1.ome.tif (CD68)", "CRU00291689-001_Scan1.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00290792_001_merge_PhenoptRFormat<- CRU00290792_001_merge_PhenoptRFormat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add column for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00290792_001_merge_PhenoptRFormat<- CRU00290792_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00290792_001_merge_PhenoptRFormat<- CRU00290792_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00290792_001_merge_PhenoptRFormat<- CRU00290792_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00290792_001_merge_PhenoptRFormat<- CRU00290792_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00290792_001_merge_PhenoptRFormat<- CRU00290792_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00290792_001_merge_PhenoptRFormat<- CRU00290792_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))  

View(CRU00290792_001_merge_PhenoptRFormat)
#Save split phenotypes file to working directory 
write.csv(CRU00290792_001_merge_PhenoptRFormat, file="CRU00290792_001_merge_PhenoptRFormat_splitphenotypes.csv") #save as csv


####CRU00290821_001####

#Import files: 
CRU00290821_001_tumour_gland <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290821-001/CRU00290821-001--tumour gland.xlsx")
View(CRU00290821_001_tumour_gland) #View to check R imported correctly 
CRU00290821_001_tumour_gland_CD68 <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290821-001/CRU00290821-001--tumour gland-CD68.xlsx")
View(CRU00290821_001_tumour_gland_CD68) #View to check R imported correctly 
CRU00290821_001_tumour_stroma <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290821-001/CRU00290821-001--tumour stroma.xlsx")
View(CRU00290821_001_tumour_stroma) #View to check R imported correctly 
CRU00290821_001_tumour_stroma_CD68 <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290821-001/CRU00290821-001--tumour stroma-CD68.xlsx")
View(CRU00290821_001_tumour_stroma_CD68) #View to check R imported correctly 

#Merge files for CRU00290821_001
CRU00290821_001_merge<- rbind(CRU00290821_001_tumour_gland, CRU00290821_001_tumour_gland_CD68)
CRU00290821_001_merge <- rbind(CRU00290821_001_merge, CRU00290821_001_tumour_stroma)
CRU00290821_001_merge <- rbind(CRU00290821_001_merge,CRU00290821_001_tumour_stroma_CD68 )
#View the merge file 
View(CRU00290821_001_merge)
#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR)
write.xlsx(CRU00290821_001_merge, "CRU00290821_001_merge.xlsx")

#Import phenoptR compatible file 
CRU00290821_001_merge_PhenoptRFormat <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290821-001/CRU00290821_001_merge_PhenoptRFormat.txt", 
                                                   delim = "\t", escape_double = FALSE, trim_ws = TRUE)


#View all Phenotypes 
table(CRU00290821_001_merge_PhenoptRFormat$`Tissue Category`, CRU00290821_001_merge_PhenoptRFormat$Phenotype)

#clean the data:
#replace PathCellObject with other 
CRU00290821_001_merge_PhenoptRFormat <-replace(CRU00290821_001_merge_PhenoptRFormat, CRU00290821_001_merge_PhenoptRFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00290821_001_merge_PhenoptRFormat<- replace(CRU00290821_001_merge_PhenoptRFormat, CRU00290821_001_merge_PhenoptRFormat=="	
CRU00290821-001_part1_leftScan2.ome.tif (CD68)", "CRU00290821-001_part1_leftScan2.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00290821_001_merge_PhenoptRFormat<- CRU00290821_001_merge_PhenoptRFormat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add column for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00290821_001_merge_PhenoptRFormat<- CRU00290821_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00290821_001_merge_PhenoptRFormat<- CRU00290821_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00290821_001_merge_PhenoptRFormat<- CRU00290821_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00290821_001_merge_PhenoptRFormat<- CRU00290821_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00290821_001_merge_PhenoptRFormat<- CRU00290821_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00290821_001_merge_PhenoptRFormat<- CRU00290821_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))  

View(CRU00290821_001_merge_PhenoptRFormat)

#Save split phenotypes file 
write.csv(CRU00290821_001_merge_PhenoptRFormat, file= "CRU00290821_001_merge_PhenoptRFormat_splitphenotypes.csv")

###CRU00290825_001###

#import files 
CRU00290825_001_tumour_gland <- read_excel("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR/Subset for Cathy (10 cases)/2. Subset for Cathy (10 cases)/CRU00290825-001/CRU00290825-001-tumour gland.xlsx")
View(CRU00290825_001_tumour_gland) #view data
CRU00290825_001_tumour_gland_CD68 <- read_excel("CRU00290825-001-tumour gland-CD68.xlsx")
View(CRU00290825_001_tumour_gland_CD68) #view data
CRU00290825_001_tumour_stroma <- read_excel("CRU00290825-001-tumour stroma.xlsx")
View(CRU00290825_001_tumour_stroma) #view data
CRU00290825_001_tumour_stroma_CD68 <- read_excel("CRU00290825-001-tumour stroma-CD68.xlsx")
View(CRU00290825_001_tumour_stroma_CD68)  #view data 

#Merge files 
CRU00290825_001_merge <- rbind(CRU00290825_001_tumour_gland, CRU00290825_001_tumour_gland_CD68)
CRU00290825_001_merge <- rbind(CRU00290825_001_merge, CRU00290825_001_tumour_stroma)
CRU00290825_001_merge <- rbind(CRU00290825_001_merge, CRU00290825_001_tumour_stroma_CD68)
View(CRU00290825_001_merge) #view merge file 

#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR)
write.csv(CRU00290825_001_merge, file="CRU00290825_001_merge.csv")

#import phenoptR compatible file  
CRU00290825_001_merge_PhenoptRFormat <- read_delim("CRU00290825_001_merge_PhenoptRFormat.txt", 
                                                   delim = "\t", escape_double = FALSE, trim_ws = TRUE)
View(CRU00290825_001_merge_PhenoptRFormat)#view data 


#View all Phenotypes 
table(CRU00290825_001_merge_PhenoptRFormat$`Tissue Category`, CRU00290825_001_merge_PhenoptRFormat$Phenotype)

#clean the data#

#replace PathCellObject with other 
CRU00290825_001_merge_PhenoptRFormat <-replace(CRU00290825_001_merge_PhenoptRFormat, CRU00290825_001_merge_PhenoptRFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00290825_001_merge_PhenoptRFormat<- replace(CRU00290825_001_merge_PhenoptRFormat, CRU00290825_001_merge_PhenoptRFormat=="	
CRU00290825-001.ome.tif (CD68)", "CRU00290825-001.ome.tif")

#the other cells are not of interest, so filter them out 
CRU00290825_001_merge_PhenoptRFormat<- CRU00290825_001_merge_PhenoptRFormat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add column for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00290825_001_merge_PhenoptRFormat<- CRU00290825_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00290825_001_merge_PhenoptRFormat<- CRU00290825_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00290825_001_merge_PhenoptRFormat<- CRU00290825_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00290825_001_merge_PhenoptRFormat<- CRU00290825_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00290825_001_merge_PhenoptRFormat<- CRU00290825_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00290825_001_merge_PhenoptRFormat<- CRU00290825_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))

View(CRU00290825_001_merge_PhenoptRFormat)

#Save the split phenotypes files to working directory 
write.csv(CRU00290825_001_merge_PhenoptRFormat, file="CRU00290825_001_merge_PhenoptRFormat.csv")

#CRU00290828_001##

#import the file for CRU00290828_001: 
CRU00290828_001_tumour_gland <- read_excel("CRU00290828-001-tumour gland.xlsx")
View(CRU00290828_001_tumour_gland) #view data
CRU00290828_001_tumour_gland_CD68 <- read_excel("CRU00290828-001-tumour gland-CD68.xlsx")
View(CRU00290828_001_tumour_gland_CD68) #view data 
CRU00290828_001_tumour_stroma <- read_excel("CRU00290828-001-tumour stroma.xlsx")
View(CRU00290828_001_tumour_stroma) #view data 
CRU00290828_001_tumour_stroma_CD68 <- read_excel("CRU00290828-001-tumour stroma-CD68.xlsx")
View(CRU00290828_001_tumour_stroma_CD68) #view data 

#merge file for CRU00290828_001
CRU00290828_001_merge <- rbind(CRU00290828_001_tumour_gland, CRU00290828_001_tumour_gland_CD68)
CRU00290828_001_merge <- rbind(CRU00290828_001_merge, CRU00290828_001_tumour_stroma)
CRU00290828_001_merge <- rbind(CRU00290828_001_merge, CRU00290828_001_tumour_stroma_CD68)
view(CRU00290828_001_merge) #View merge file 

#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR)
write.csv(CRU00290828_001_merge, file="CRU00290828_001_merge.csv")

#import phenoptR compatible file  
CRU00290828_001_merge_phenoptrformat <- read_delim("CRU00290828_001_merge_phenoptrformat.txt", 
                                                   delim = "\t", escape_double = FALSE, trim_ws = TRUE)
View(CRU00290828_001_merge_phenoptrformat) #view data 
#View all Phenotypes 
table(CRU00290828_001_merge_phenoptrformat$`Tissue Category`, CRU00290828_001_merge_phenoptrformat$Phenotype)

#clean the data:
#replace PathCellObject with other 
CRU00290828_001_merge_phenoptrformat <-replace(CRU00290828_001_merge_phenoptrformat, CRU00290828_001_merge_phenoptrformat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00290828_001_merge_phenoptrformat<- replace(CRU00290828_001_merge_phenoptrformat, CRU00290828_001_merge_phenoptrformat=="	
CRU00290825-001.ome.tif (CD68)", "CRU00290825-001.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00290828_001_merge_phenoptrformat<- CRU00290828_001_merge_phenoptrformat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add columns for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00290828_001_merge_phenoptrformat<- CRU00290828_001_merge_phenoptrformat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00290828_001_merge_phenoptrformat<- CRU00290828_001_merge_phenoptrformat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00290828_001_merge_phenoptrformat<- CRU00290828_001_merge_phenoptrformat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00290828_001_merge_phenoptrformat<- CRU00290828_001_merge_phenoptrformat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00290828_001_merge_phenoptrformat<- CRU00290828_001_merge_phenoptrformat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00290828_001_merge_phenoptrformat<- CRU00290828_001_merge_phenoptrformat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))

View(CRU00290828_001_merge_phenoptrformat)

#Save split phenotypes files to working directory 
write.csv(CRU00290828_001_merge_phenoptrformat, file="CRU00290828_001_merge_phenoptrformat.csv" )


#####CRU00290845_001#####

#import CRU00290845_001 files 
CRU00290845_001_Part2_tumour_gland <- read_excel("CRU00290845-001_Part2-tumour gland.xlsx")
View(CRU00290845_001_Part2_tumour_gland) #view data                                                                  
CRU00290845_001_Part2_tumour_gland_CD68 <- read_excel("CRU00290845-001_Part2-tumour gland-CD68.xlsx")
View(CRU00290845_001_Part2_tumour_gland_CD68) #view data                                                                  
CRU00290845_001_Part2_tumour_stroma <- read_excel("CRU00290845-001_Part2-tumour stroma.xlsx")
View(CRU00290845_001_Part2_tumour_stroma) #view data                                                                    
CRU00290845_001_Part2_tumour_stroma_CD68 <- read_excel("CRU00290845-001_Part2-tumour stroma-CD68.xlsx")
View(CRU00290845_001_Part2_tumour_stroma_CD68)  #view data 

#merge CRU00290845_001 MIF data files 
CRU00290845_001_merge <- rbind(CRU00290845_001_Part2_tumour_gland, CRU00290845_001_Part2_tumour_gland_CD68)
CRU00290845_001_merge <- rbind(CRU00290845_001_merge, CRU00290845_001_Part2_tumour_stroma)
CRU00290845_001_merge <- rbind(CRU00290845_001_merge, CRU00290845_001_Part2_tumour_stroma_CD68)
View(CRU00290845_001_merge) #view merge data 


#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR)
write.csv(CRU00290845_001_merge, file="CRU00290845_001_merge.csv")

#Import the phenoptR compatible file 
CRU00290845_001_merge_phenoptRFormat <- read_delim("CRU00290845_001_merge_phenoptRFormat.txt", 
                                                   delim = "\t", escape_double = FALSE, trim_ws = TRUE)
View(CRU00290845_001_merge_phenoptRFormat)#view data 

#View all Phenotypes 
table(CRU00290845_001_merge_phenoptRFormat$`Tissue Category`, CRU00290845_001_merge_phenoptRFormat$Phenotype)

#clean the data:
#replace PathCellObject with other 
CRU00290845_001_merge_phenoptRFormat <-replace(CRU00290845_001_merge_phenoptRFormat, CRU00290845_001_merge_phenoptRFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00290845_001_merge_phenoptRFormat<- replace(CRU00290845_001_merge_phenoptRFormat, CRU00290845_001_merge_phenoptRFormat=="	
CRU00290845-001_Part2.ome.tif (CD68)", "CRU00290845-001_Part2.ome.tif
")
#the other cells are not of interest, so filter them out 
CRU00290845_001_merge_phenoptRFormat<- CRU00290845_001_merge_phenoptRFormat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add columns for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00290845_001_merge_phenoptRFormat<- CRU00290845_001_merge_phenoptRFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00290845_001_merge_phenoptRFormat<- CRU00290845_001_merge_phenoptRFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00290845_001_merge_phenoptRFormat<- CRU00290845_001_merge_phenoptRFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00290845_001_merge_phenoptRFormat<- CRU00290845_001_merge_phenoptRFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00290845_001_merge_phenoptRFormat<- CRU00290845_001_merge_phenoptRFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00290845_001_merge_phenoptRFormat<- CRU00290845_001_merge_phenoptRFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))

View(CRU00290845_001_merge_phenoptRFormat)

#Save split phenotype file to working directory  
write.csv(CRU00290845_001_merge_phenoptRFormat, file= "CRU00290845_001_merge_phenoptRFormat.csv")


#####CRU00290870_001####

#Import CRU00290870_001 files 
CRU00290870_001_tumour_gland_CD68 <- read_excel("CRU00290870-001-tumour gland-CD68.xlsx")
View(CRU00290870_001_tumour_gland_CD68) 
CRU00290870_001_tumour_gland <- read_excel("CRU00290870-001-tumour gland.xlsx")
View(CRU00290870_001_tumour_gland)
CRU00290870_001_tumour_stroma <- read_excel("CRU00290870-001-tumour stroma.xlsx")
View(CRU00290870_001_tumour_stroma)
CRU00290870_001_tumour_stroma_CD68 <- read_excel("CRU00290870-001-tumour stroma-CD68.xlsx")
View(CRU00290870_001_tumour_stroma_CD68) 

#Merge CRU00290870_001 files 
CRU00290870_001_merge <- rbind(CRU00290870_001_tumour_gland, CRU00290870_001_tumour_gland_CD68, CRU00290870_001_tumour_stroma, CRU00290870_001_tumour_stroma_CD68)
View(CRU00290870_001_merge) #view merge file 
#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR)
write.csv(CRU00290870_001_merge, file= "CRU00290870_001_merge.csv")

#Import PhenoptR compatible file 
CRU00290870_001_merge_PhenoptRFormat <- read_delim("CRU00290870_001_merge_PhenoptRFormat.txt", 
                                                   delim = "\t", escape_double = FALSE,  trim_ws = TRUE)
View(CRU00290870_001_merge_PhenoptRFormat) #View file 

#View all Phenotypes 
table(CRU00290870_001_merge_PhenoptRFormat$`Tissue Category`, CRU00290870_001_merge_PhenoptRFormat$Phenotype)

#clean the data:
#replace PathCellObject with other 
CRU00290870_001_merge_PhenoptRFormat <-replace(CRU00290870_001_merge_PhenoptRFormat, CRU00290870_001_merge_PhenoptRFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00290870_001_merge_PhenoptRFormat<- replace(CRU00290870_001_merge_PhenoptRFormat, CRU00290870_001_merge_PhenoptRFormat=="	
CRU00290870-001.ome.tif (CD68)", "CRU00290870-001.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00290870_001_merge_PhenoptRFormat<- CRU00290870_001_merge_PhenoptRFormat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add columns for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00290870_001_merge_PhenoptRFormat<- CRU00290870_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00290870_001_merge_PhenoptRFormat<- CRU00290870_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00290870_001_merge_PhenoptRFormat<- CRU00290870_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00290870_001_merge_PhenoptRFormat<- CRU00290870_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00290870_001_merge_PhenoptRFormat<- CRU00290870_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00290870_001_merge_PhenoptRFormat<- CRU00290870_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))

View(CRU00290870_001_merge_PhenoptRFormat)

#Save split phenotype 
write.csv(CRU00290870_001_merge_PhenoptRFormat, file="CRU00290870_001_merge_PhenoptRFormat.csv")


#########CRU00291725_001####


#Import CRU00291725_001 files 
CRU00291725_001_Scan1_tumour_gland <- read_excel("CRU00291725-001_Scan1-tumour gland.xlsx")
View(CRU00291725_001_Scan1_tumour_gland)  #View data                                                                      
CRU00291725_001_Scan1_tumour_gland_CD68 <- read_excel("CRU00291725-001_Scan1-tumour gland-CD68.xlsx")
View(CRU00291725_001_Scan1_tumour_gland_CD68)  #View data                                                                 
CRU00291725_001_Scan1_tumour_stroma <- read_excel("CRU00291725-001_Scan1-tumour stroma.xlsx")
View(CRU00291725_001_Scan1_tumour_stroma)     #View data                                                                  
CRU00291725_001_Scan1_tumour_stroma_CD68 <- read_excel("CRU00291725-001_Scan1-tumour stroma-CD68.xlsx")
View(CRU00291725_001_Scan1_tumour_stroma_CD68) #View data 

#merge CRU00291725_001 files 
CRU00291725_001_merge <- rbind(CRU00291725_001_Scan1_tumour_gland, CRU00291725_001_Scan1_tumour_gland_CD68, CRU00291725_001_Scan1_tumour_stroma, CRU00291725_001_Scan1_tumour_stroma_CD68)
View(CRU00291725_001_merge) #View merge data 

#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR) 
write.csv(CRU00291725_001_merge, file="CRU00291725_001_merge.csv")

#Import phenoptR compatible file 
CRU00291725_001_merge_phenoptR_Format <- read_delim("CRU00291725_001_merge_phenoptR_Format.txt", 
                                                    delim = "\t", escape_double = FALSE, 
                                                    trim_ws = TRUE)
View(CRU00291725_001_merge_phenoptR_Format) #View data 

#View all Phenotypes 
table(CRU00291725_001_merge_phenoptR_Format$`Tissue Category`, CRU00291725_001_merge_phenoptR_Format$Phenotype)

#clean the data:
#replace PathCellObject with other 
CRU00291725_001_merge_phenoptR_Format <-replace(CRU00291725_001_merge_phenoptR_Format, CRU00291725_001_merge_phenoptR_Format=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00291725_001_merge_phenoptR_Format<- replace(CRU00291725_001_merge_phenoptR_Format, CRU00291725_001_merge_phenoptR_Format=="	
CRU00291725-001_Scan1.ome.tif (CD68)", "CRU00291725-001_Scan1.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00291725_001_merge_phenoptR_Format<- CRU00291725_001_merge_phenoptR_Format %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add columns for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00291725_001_merge_phenoptR_Format<- CRU00291725_001_merge_phenoptR_Format %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00291725_001_merge_phenoptR_Format<- CRU00291725_001_merge_phenoptR_Format %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00291725_001_merge_phenoptR_Format<- CRU00291725_001_merge_phenoptR_Format %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00291725_001_merge_phenoptR_Format<- CRU00291725_001_merge_phenoptR_Format %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00291725_001_merge_phenoptR_Format<- CRU00291725_001_merge_phenoptR_Format %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00291725_001_merge_phenoptR_Format<- CRU00291725_001_merge_phenoptR_Format %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))

View(CRU00291725_001_merge_phenoptR_Format)

#Save split phenotype file 
write.csv(CRU00291725_001_merge_phenoptR_Format, file="CRU00291725_001_merge_phenoptR_Format_splitphenotypes.csv")


#####CRU00291732_001#####

#import CRU00291732_001 files 
CRU00291732_001_Scan2_tumour_gland <- read_excel("CRU00291732-001_Scan2-tumour gland.xlsx")
View(CRU00291732_001_Scan2_tumour_gland) #View data                                                                       
CRU00291732_001_Scan2_tumour_gland_CD68 <- read_excel("CRU00291732-001_Scan2-tumour gland-CD68.xlsx")
View(CRU00291732_001_Scan2_tumour_gland_CD68) #view data                                                                  
CRU00291732_001_Scan2_tumour_stroma <- read_excel("CRU00291732-001_Scan2-tumour stroma.xlsx")
View(CRU00291732_001_Scan2_tumour_stroma) #view data                                                                     
CRU00291732_001_Scan2_tumour_stroma_CD68 <- read_excel("CRU00291732-001_Scan2-tumour stroma-CD68.xlsx")
View(CRU00291732_001_Scan2_tumour_stroma_CD68)#view data  

#Merge CRU00291732_001 files 
CRU00291732_001_merge <- rbind(CRU00291732_001_Scan2_tumour_gland, CRU00291732_001_Scan2_tumour_gland_CD68, CRU00291732_001_Scan2_tumour_stroma, CRU00291732_001_Scan2_tumour_stroma_CD68)
View(CRU00291732_001_merge) #view merge data 

#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR) 
write.csv(CRU00291732_001_merge, file="CRU00291732_001_merge.csv")

#Import phenoptR compatible file 
CRU00291732_001_merge_PhenoptRFormat <- read_delim("CRU00291732_001_merge_PhenoptRFormat.txt", 
                                                   delim = "\t", escape_double = FALSE, trim_ws = TRUE)
View(CRU00291732_001_merge_PhenoptRFormat) #View data 

#View all Phenotypes 
table(CRU00291732_001_merge_PhenoptRFormat$`Tissue Category`, CRU00291732_001_merge_PhenoptRFormat$Phenotype)

#clean the data:
#replace PathCellObject with other 
CRU00291732_001_merge_PhenoptRFormat <-replace(CRU00291732_001_merge_PhenoptRFormat, CRU00291732_001_merge_PhenoptRFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00291732_001_merge_PhenoptRFormat<- replace(CRU00291732_001_merge_PhenoptRFormat, CRU00291732_001_merge_PhenoptRFormat=="	
CRU00291732-001_Scan2.ome.tif (CD68)", "	
CRU00291732-001_Scan2.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00291732_001_merge_PhenoptRFormat<- CRU00291732_001_merge_PhenoptRFormat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add columns for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00291732_001_merge_PhenoptRFormat<- CRU00291732_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00291732_001_merge_PhenoptRFormat<- CRU00291732_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00291732_001_merge_PhenoptRFormat<- CRU00291732_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00291732_001_merge_PhenoptRFormat<- CRU00291732_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00291732_001_merge_PhenoptRFormat<- CRU00291732_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00291732_001_merge_PhenoptRFormat<- CRU00291732_001_merge_PhenoptRFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))

View(CRU00291732_001_merge_PhenoptRFormat)

#Save split phenotype file to working directory 
write.csv(CRU00291732_001_merge_PhenoptRFormat, file="CRU00291732_001_merge_PhenoptRFormat.csv")

####CRU00291743_001###

#Import CRU00291743_001 files 
CRU00291743_001_Scan1_tumour_gland <- read_excel("CRU00291743-001_Scan1-tumour gland.xlsx")
View(CRU00291743_001_Scan1_tumour_gland)  #view data                                                                   
CRU00291743_001_Scan1_tumour_gland_CD68 <- read_excel("CRU00291743-001_Scan1-tumour gland-CD8.xlsx")
View(CRU00291743_001_Scan1_tumour_gland_CD68)  #view data                                                                  
CRU00291743_001_Scan1_tumour_stroma <- read_excel("CRU00291743-001_Scan1-tumour stroma.xlsx")
View(CRU00291743_001_Scan1_tumour_stroma) #view data                                                                  
CRU00291743_001_Scan1_tumour_stroma_CD68 <- read_excel("CRU00291743-001_Scan1-tumour stroma-CD68.xlsx")
View(CRU00291743_001_Scan1_tumour_stroma_CD68) #view data 

#Merge CRU00291743_001 files 
CRU00291743_001_merge = rbind(CRU00291743_001_Scan1_tumour_gland, CRU00291743_001_Scan1_tumour_gland_CD68, CRU00291743_001_Scan1_tumour_stroma, CRU00291743_001_Scan1_tumour_stroma_CD68)
View(CRU00291743_001_merge) #view data 

#Save file to work directory to format file in excel (in excel file wrangled into a format compatible with phenoptR) 
write.csv(CRU00291743_001_merge, file="CRU00291743_001_merge.csv")

#Import PhenoptR compatible file 
CRU00291743_001_merge_PhenoptrFormat <- read_delim("CRU00291743_001_merge_PhenoptrFormat.txt", 
                                                   delim = "\t", escape_double = FALSE, trim_ws = TRUE)
View(CRU00291743_001_merge_PhenoptrFormat) #view data 

#View all Phenotypes 
table(CRU00291743_001_merge_PhenoptrFormat$`Tissue Category`, CRU00291743_001_merge_PhenoptrFormat$Phenotype)

#clean the data
#replace PathCellObject with other 
CRU00291743_001_merge_PhenoptrFormat <-replace(CRU00291743_001_merge_PhenoptrFormat, CRU00291743_001_merge_PhenoptrFormat=="PathCellObject", "other")
#rename CRU00291689-001_Scan1.ome.tif (CD68) as CRU00291689-001_Scan1.ome.tif
CRU00291743_001_merge_PhenoptrFormat<- replace(CRU00291743_001_merge_PhenoptrFormat, CRU00291743_001_merge_PhenoptrFormat=="	
CRU00291743-001_Scan1.ome.tif (CD68)", "CRU00291743-001_Scan1.ome.tif")
#the other cells are not of interest, so filter them out 
CRU00291743_001_merge_PhenoptrFormat<- CRU00291743_001_merge_PhenoptrFormat %>% filter(Phenotype!="other")

#Split the phenotypes into there own column (ie. Add columns for each immune cell and place + or - in in column depending if immune cell is expressed or not)

#CD68
CRU00291743_001_merge_PhenoptrFormat<- CRU00291743_001_merge_PhenoptrFormat %>% 
  mutate("Phenotype CD68"= case_when(
    Phenotype == "CD68" ~ "CD68+",
    Phenotype != "CD68" ~ "CD68-"))

#CD3 
CRU00291743_001_merge_PhenoptrFormat<- CRU00291743_001_merge_PhenoptrFormat %>% 
  mutate("Phenotype CD3" = case_when(
    Phenotype== "CD3" ~ "CD3+", 
    Phenotype== "CD3: CD4" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD3+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD3+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD3+", 
    Phenotype== "CD3: FoxP3"~ "CD3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD3+",
    Phenotype== "CD3: PD1" ~ "CD3+", 
    Phenotype== "CD4" ~ "CD3-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD3-", 
    Phenotype== "CD4: FoxP3" ~ "CD3-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD3-", 
    Phenotype== "CD4: PD1"~ "CD3-", 
    Phenotype== "CD68" ~ "CD3-", 
    Phenotype== "CD8" ~ "CD3-", 
    Phenotype== "CD8: FoxP3" ~ "CD3-", 
    Phenotype== "CD8: PD1" ~ "CD3-", 
    Phenotype== "FoxP3" ~ "CD3-", 
    Phenotype== "FoxP3: PD1" ~ "CD3-", 
    Phenotype== "PD1" ~ "CD3-"))  

#CD4 
CRU00291743_001_merge_PhenoptrFormat<- CRU00291743_001_merge_PhenoptrFormat %>% 
  mutate("Phenotype CD4" = case_when(
    Phenotype== "CD3" ~ "CD4-", 
    Phenotype== "CD3: CD4" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD4+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD4: PD1" ~ "CD4+", 
    Phenotype== "CD3: CD8" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD4-", 
    Phenotype== "CD3: CD8: PD1" ~ "CD4-", 
    Phenotype== "CD3: FoxP3"~ "CD4-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD4-",
    Phenotype== "CD3: PD1" ~ "CD4-", 
    Phenotype== "CD4" ~ "CD4+", 
    Phenotype== "CD4: CD8: PD1" ~ "CD4+", 
    Phenotype== "CD4: FoxP3" ~ "CD4+", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD4+", 
    Phenotype== "CD4: PD1"~ "CD4+", 
    Phenotype== "CD68" ~ "CD4-", 
    Phenotype== "CD8" ~ "CD4-", 
    Phenotype== "CD8: FoxP3" ~ "CD4-", 
    Phenotype== "CD8: PD1" ~ "CD4-", 
    Phenotype== "FoxP3" ~ "CD4-", 
    Phenotype== "FoxP3: PD1" ~ "CD4-", 
    Phenotype== "PD1" ~ "CD4-"))  

#CD8 
CRU00291743_001_merge_PhenoptrFormat<- CRU00291743_001_merge_PhenoptrFormat %>% 
  mutate("Phenotype CD8" = case_when(
    Phenotype== "CD3" ~ "CD8-", 
    Phenotype== "CD3: CD4" ~ "CD8-", 
    Phenotype== "CD3: CD4: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "CD8+",
    Phenotype== "CD3: CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD4: PD1" ~ "CD8-", 
    Phenotype== "CD3: CD8" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "CD8+", 
    Phenotype== "CD3: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD3: FoxP3"~ "CD8-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "CD8-",
    Phenotype== "CD3: PD1" ~ "CD8-", 
    Phenotype== "CD4" ~ "CD8-", 
    Phenotype== "CD4: CD8: PD1" ~ "CD8+", 
    Phenotype== "CD4: FoxP3" ~ "CD8-", 
    Phenotype== "CD4: FoxP3: PD1"~ "CD8-", 
    Phenotype== "CD4: PD1"~ "CD8-", 
    Phenotype== "CD68" ~ "CD8-", 
    Phenotype== "CD8" ~ "CD8+", 
    Phenotype== "CD8: FoxP3" ~ "CD8+", 
    Phenotype== "CD8: PD1" ~ "CD8+", 
    Phenotype== "FoxP3" ~ "CD8-", 
    Phenotype== "FoxP3: PD1" ~ "CD8-", 
    Phenotype== "PD1" ~ "CD8-"))  

#PD1 
CRU00291743_001_merge_PhenoptrFormat<- CRU00291743_001_merge_PhenoptrFormat %>% 
  mutate("Phenotype PD1" = case_when(
    Phenotype== "CD3" ~ "PD1-", 
    Phenotype== "CD3: CD4" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "PD1+",
    Phenotype== "CD3: CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD4: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "PD1+", 
    Phenotype== "CD3: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD3: FoxP3"~ "PD1-", 
    Phenotype== "CD3: FoxP3: PD1" ~ "PD1+",
    Phenotype== "CD3: PD1" ~ "PD1+", 
    Phenotype== "CD4" ~ "PD1-", 
    Phenotype== "CD4: CD8: PD1" ~ "PD1+", 
    Phenotype== "CD4: FoxP3" ~ "PD1-", 
    Phenotype== "CD4: FoxP3: PD1"~ "PD1+", 
    Phenotype== "CD4: PD1"~ "PD1+", 
    Phenotype== "CD68" ~ "PD1-", 
    Phenotype== "CD8" ~ "PD1-", 
    Phenotype== "CD8: FoxP3" ~ "PD1-", 
    Phenotype== "CD8: PD1" ~ "PD1+", 
    Phenotype== "FoxP3" ~ "PD1-", 
    Phenotype== "FoxP3: PD1" ~ "PD1+", 
    Phenotype== "PD1" ~ "PD1+"))  

#FOXP3
CRU00291743_001_merge_PhenoptrFormat<- CRU00291743_001_merge_PhenoptrFormat %>% 
  mutate("Phenotype FoxP3" = case_when(
    Phenotype== "CD3" ~ "FoxP3-", 
    Phenotype== "CD3: CD4" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD4: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: CD8: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: CD4: CD8: PD1" ~ "FoxP3-",
    Phenotype== "CD3: CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD4: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: CD8" ~ "FoxP3-", 
    Phenotype== "CD3: CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "CD3: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD3: FoxP3"~ "FoxP3+", 
    Phenotype== "CD3: FoxP3: PD1" ~ "FoxP3+",
    Phenotype== "CD3: PD1" ~ "FoxP3-", 
    Phenotype== "CD4" ~ "FoxP3-", 
    Phenotype== "CD4: CD8: PD1" ~ "FoxP3-", 
    Phenotype== "CD4: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD4: FoxP3: PD1"~ "FoxP3+", 
    Phenotype== "CD4: PD1"~ "FoxP3-", 
    Phenotype== "CD68" ~ "FoxP3-", 
    Phenotype== "CD8" ~ "FoxP3-", 
    Phenotype== "CD8: FoxP3" ~ "FoxP3+", 
    Phenotype== "CD8: PD1" ~ "FoxP3-", 
    Phenotype== "FoxP3" ~ "FoxP3+", 
    Phenotype== "FoxP3: PD1" ~ "FoxP3+", 
    Phenotype== "PD1" ~ "FoxP3-"))

View(CRU00291743_001_merge_PhenoptrFormat)

#Save split phenotype file to working dictory 
write.csv(CRU00291743_001_merge_PhenoptrFormat, file="CRU00291743_001_merge_PhenoptrFormat.csv")

######## Using PhenoptRReports Shiny App inputted PhenoptR format file for each patient to generate nearest neighbour analysis results#########

####Perform A Kruskal-Wallis to determine which Nearest Neighbour Distance for pair of phenotype are significantly altered between lethal and non-lethal PCa cases 

#load R packages
library(readxl)
library(matrixTests)

#Import Median Nearest Neighbour Distance for pair of phenotype generated by phenoptR Reports (Tumour Gland)
MedianNND_tumourgland <- read_excel("Median_Nearest_Neighbours_Distances.xlsx", 
                                    sheet = "Tumour Gland ")
View(MedianNND_tumourgland) #view data

#Import Median Nearest Neighbours Distance for pair of phenotypes generated by phenoptR Reports  (Tumour Stroma)
MedianNND_tumourstroma <- read_excel("Median_Nearest_Neighbours_Distances.xlsx", 
                                     sheet = "Tumour Stroma")
View(MedianNND_tumourstroma) #view data 

#Subset out the Cohort and multiplex image ID columns from NI.study 
subsetCohortandMplexID <- subset(NI.study, select = c("Cohort", "Multiplex.image.ID"))
View(subsetCohortandMplexID) #View the Subset 

#Merge Median Nearest Neighbour distance with Subset to add cohort column to define which samples are lethal/non-lethal PCa 
MedianNND_tumourgland <- merge(MedianNND_tumourgland, subsetCohortandMplexID, by="Multiplex.image.ID")
MedianNND_tumourstroma <- merge(MedianNND_tumourstroma, subsetCohortandMplexID, by="Multiplex.image.ID")

#Perform Kruskal-Wallis: examine median nearest neighbour distance between pairs varies between Cohort Status (Tumour Gland)
KWTEST_TGland <- col_kruskalwallis(MedianNND_tumourgland[, 2:241], MedianNND_tumourgland$Cohort)
#subset out significant p-values 
signpvalueTumourGland <- subset(KWTEST_TGland, KWTEST_TGland$pvalue<0.05) #pvalue <0.05 
signpvalueTumourGland #View the significant results 
signOrderTGland <- signpvalueTumourGland[order(signpvalueTumourGland$pvalue),] #order p-values
signOrderTGland

#Perform Kruskal-Wallis: examine median nearest neighbour distance between pairs varies between Cohort Status (Tumour Stroma)
KWTEST_TStroma <- col_kruskalwallis(MedianNND_tumourstroma[, 2:241], MedianNND_tumourstroma$Cohort)
#subset out significant p-values
signpvalueTumourStroma <-subset(KWTEST_TStroma, KWTEST_TStroma$pvalue<0.05) #pvalue <0.05
signpvalueTumourStroma #View the significant results
signOrderTStroma <- signpvalueTumourStroma[order(signpvalueTumourStroma$pvalue),] #order p-values
signOrderTStroma

#find median distance between immune cells lethal vs non-lethal PCa
#Tumour gland: From Cd3+ to CD3+/CD8+/PD1+`
MedianNND_tumourgland %>% group_by(Cohort) %>% summarise(Mean=mean(`Median Nearest Neighbor Distance: From CD3+ to CD3+/CD8+/PD1+`), Max=max(`Median Nearest Neighbor Distance: From CD3+ to CD3+/CD8+/PD1+`), Min=min(`Median Nearest Neighbor Distance: From CD3+ to CD3+/CD8+/PD1+`), Median=median(`Median Nearest Neighbor Distance: From CD3+ to CD3+/CD8+/PD1+`), Std=sd(`Median Nearest Neighbor Distance: From CD3+ to CD3+/CD8+/PD1+`))
#tumour gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+
MedianNND_tumourgland %>% group_by(Cohort) %>% summarise(Mean=mean(`Median Nearest Neighbor Distance:From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+`), Max=max(`Median Nearest Neighbor Distance:From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+`), Min=min(`Median Nearest Neighbor Distance:From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+`), Median=median(`Median Nearest Neighbor Distance:From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+`), Std=sd(`Median Nearest Neighbor Distance:From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+`))
#Tumour Stroma: from CD3+/CD4+/PD1-/Foxp3 to Cd68+
MedianNND_tumourstroma %>% group_by(Cohort) %>% summarise(Mean=mean(`Median Nearest Neighbor Distance: From CD3+/CD4+/PD1-/FoxP3- to CD68+`), Max=max(`Median Nearest Neighbor Distance: From CD3+/CD4+/PD1-/FoxP3- to CD68+`), Min=min(`Median Nearest Neighbor Distance: From CD3+/CD4+/PD1-/FoxP3- to CD68+`), Median=median(`Median Nearest Neighbor Distance: From CD3+/CD4+/PD1-/FoxP3- to CD68+`), Std=sd(`Median Nearest Neighbor Distance: From CD3+/CD4+/PD1-/FoxP3- to CD68+`))
#Tumour Stroma: from Cd3+ to CD68+
MedianNND_tumourstroma %>% group_by(Cohort) %>% summarise(Mean=mean(`Median Nearest Neighbor Distance: From CD3+ to CD68+`), Max=max(`Median Nearest Neighbor Distance: From CD3+ to CD68+`), Min=min(`Median Nearest Neighbor Distance: From CD3+ to CD68+`), Median=median(`Median Nearest Neighbor Distance: From CD3+ to CD68+`), Std=sd(`Median Nearest Neighbor Distance: From CD3+ to CD68+`))

##Overlay Immune cell phenotype on MIF images## 

#Load R packages 
library("tiff")
library("raster")
library("jpeg")
library("magick")
library(phenoptr)
library(tidyverse)
library(readr)
library(ggplot2)
library(phenoptr)
library(readr)

###CRU00290792-001.ome.tif: 

#import nearest neighbours stroma
nearest_neighbors_All_792_Stroma <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder/nearest_neighbors_Stroma.txt", 
                                               delim = "\t", escape_double = FALSE, 
                                               trim_ws = TRUE)

# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_All_792_Stroma %>% filter(select_rows(nearest_neighbors_All_792_Stroma, "CD3+"))
cd68_cells = nearest_neighbors_All_792_Stroma %>% filter(select_rows(nearest_neighbors_All_792_Stroma, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import the MIF image 
background = jpeg::readJPEG("CRU00290792-001.ome.jpg") %>% as.raster()
xlim = c(0,10900)
ylim = c(0, 9450)
base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into region: 
zoombackground = jpeg::readJPEG("CRU00290792-001.ome.jpg") %>% as.raster()
xlim = c(4820,6900)
ylim = c(10, 1840)
zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_All_792_Stroma %>% filter(select_rows(nearest_neighbors_All_792_Stroma,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

##Tumour Gland:

#import nearest neighbour tumour gland 
nearest_neighbors_All_792_Gland <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder/nearest_neighbors_Gland.txt", 
                                              delim = "\t", escape_double = FALSE, 
                                              trim_ws = TRUE)

#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_All_792_Gland %>% filter(select_rows(nearest_neighbors_All_792_Gland, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_All_792_Gland %>% filter(select_rows(nearest_neighbors_All_792_Gland, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#import MIF image 
background = jpeg::readJPEG("CRU00290792-001.ome.jpg") %>% as.raster()
xlim = c(0,10900)
ylim = c(0, 9450)
base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom in region of interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_All_792_Gland %>% filter(select_rows(nearest_neighbors_All_792_Gland, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_All_792_Gland %>% filter(select_rows(nearest_neighbors_All_792_Gland, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')


##CRU00290821_001_part1_leftScan2:

#import MIF tif and convert into pjeg format 
CRU00290821_001_part1_leftScan2.ome <- image_read("CRU00290821-001_part1_leftScan2.ome.tif")
image_write(CRU00290821_001_part1_leftScan2.ome , path = "CRU00290821_001_part1_leftScan2.ome.jpg", format = "jpeg", quality = 75)

#import nearest neighbours stroma
nearest_neighbors_Stroma_CRU00290821_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (2)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)

# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00290821_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290821_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00290821_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290821_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF images
background = jpeg::readJPEG("CRU00290821_001_part1_leftScan2.ome.jpg") %>% as.raster()
xlim = c(0,17900)
ylim = c(0, 20190)

base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into tile of interest 
zoombackground = jpeg::readJPEG("CRU00290821_001_part1_leftScan2.ome.jpg") %>% as.raster()
xlim = c(200,4900)
ylim = c(9500, 20000)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00290821_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290821_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

##Tumour Gland## 

#import nearest neighbours tumour gland
nearest_neighbors_Gland_CRU00290821_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (2)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)

#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00290821_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290821_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290821_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290821_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom in region of interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290821_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290821_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00290821_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290821_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))

#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

###CRU00290825_001###


#import nearest neighbours stroma
nearest_neighbors_Stroma_CRU00290825_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (3)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)
View(nearest_neighbors_Stroma_CRU00290825_001)


# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00290825_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290825_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00290825_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290825_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif and convert into pjeg format 
CRU00290825_001_image <- image_read("CRU00290825-001.ome.tif")
image_write(CRU00290825_001_image, path = "CRU00290825_001_Scan_ome.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00290825_001_Scan_ome.jpg") %>% as.raster()
xlim = c(0,15900)
ylim = c(0,21400)

base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom tile of interest MIF images 
zoombackground = jpeg::readJPEG("CRU00290825_001_Scan_ome.jpg") %>% as.raster()
xlim = c(7480,11000)
ylim = c(7480, 11800)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00290825_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290825_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))

#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')


#Tumour Gland# 

nearest_neighbors_Gland_CRU00290825_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (3)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)


#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00290825_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290825_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290825_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290825_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#plot immune cell into mif images 
base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom into a region of  interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+
#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290825_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290825_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00290825_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290825_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')



###CRU00290828_001####


#import nearest stroma data 
nearest_neighbors_Stroma_CRU00290828_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (4)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)

# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00290828_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290828_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00290828_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290828_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))


#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif and convert into pjeg format 
CRU00290828_001_image <- image_read("CRU00290828-001.ome.tif")
image_write(CRU00290828_001_image, path = "CRU00290828_001.ome.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00290828_001.ome.jpg") %>% as.raster()
xlim = c(0,11800)
ylim = c(0,13908)

base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into a region of interest/tile 
zoombackground = jpeg::readJPEG("CRU00290828_001.ome.jpg") %>% as.raster()
xlim = c(9500,11800)
ylim = c(5290, 13908)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00290828_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290828_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')


#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

##Tumour Gland## 

#import nearest neighbour tumour gland 
nearest_neighbors_Gland_CRU00290828_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (4)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)


#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00290828_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290828_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290828_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290828_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#overlay immune cell co-ordinates on MIF images
base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom into a region of  interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290828_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290828_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00290828_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290828_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

####CRU00290845_001#####

#import nearest neighbour stroma 
nearest_neighbors_Stroma_CRU00290845_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (5)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)
# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00290845_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290845_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00290845_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290845_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif and convert into pjeg format 
CRU00290845_001_image <- image_read("CRU00290845-001_Part2.ome.tif")
image_write(CRU00290845_001_image, path = "CRU00290845_001.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00290845_001.jpg") %>% as.raster()
xlim = c(0,8400)
ylim = c(0,13900)

base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into a region of interest/tile 
zoombackground = jpeg::readJPEG("CRU00290845_001.jpg") %>% as.raster()
xlim = c(0,4000)
ylim = c(0, 7000)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00290845_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290845_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')


#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

##Tumour Gland## 

#import nearest neighbour tumour gland 
nearest_neighbors_Gland_CRU00290845_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (5)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)


#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00290845_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290845_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290845_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290845_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#import MIF images
background = jpeg::readJPEG("CRU00290845_001.jpg") %>% as.raster()
xlim = c(0,11800)
ylim = c(0,13908)

base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom into a region of  interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290845_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290845_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00290845_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290845_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

########CRU00290870_001#####

#import the nearest neighbour stroma 
nearest_neighbors_Stroma_CRU00290870_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (6)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)


# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00290870_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290870_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00290870_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290870_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif and convert into pjeg format 
CRU00290870_001_image <- image_read("CRU00290870-001.ome.tif")
image_write(CRU00290870_001_image, path = "CRU00290870_001.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00290870_001.jpg") %>% as.raster()
xlim = c(0,15000)
ylim = c(0,16808)

base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into a region of interest/tile 
zoombackground = jpeg::readJPEG("CRU00290870_001.jpg") %>% as.raster()
xlim = c(3450,5500)
ylim = c(12000, 16708)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00290870_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00290870_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')


#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

#Tumour Gland:

#import nearest neighbour tumour gland 
nearest_neighbors_Gland_CRU00290870_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (6)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)

#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00290870_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290870_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290870_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00290870_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom into a region of  interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00290870_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290870_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00290870_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00290870_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')


########CRU00291689_001#####


nearest_neighbors_Stroma_CRU00291689_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (7)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)

# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00291689_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291689_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00291689_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291689_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif and convert into pjeg format 
CRU00291689_001_image <- image_read("CRU00291689-001_Scan1.ome.tif")
image_write(CRU00291689_001_image, path = "CRU00291689_001.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00291689_001.jpg") %>% as.raster()
xlim = c(0,11500)
ylim = c(0,15050)

base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into a region of interest/tile 
zoombackground = jpeg::readJPEG("CRU00291689_001.jpg") %>% as.raster()
xlim = c(0,7800)
ylim = c(8550, 15050)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00291689_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291689_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')


#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')


##Tumour Gland## 

#import nearest neighbour tumour gland 
nearest_neighbors_Gland_CRU00291689_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (7)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)

#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00291689_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291689_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291689_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00291689_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#plot immune cells over MIF image 
base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom into a region of  interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291689_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291689_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00291689_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291689_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')


#####CRU00291725_001####


#import tumour stroma nearest neighbour data 
nearest_neighbors_Stroma_CRU00291725_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (8)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)
View(nearest_neighbors_Stroma_CRU00291725_001)

# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00291725_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291725_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00291725_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291725_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif and convert into pjeg format 
CRU00291725_00_image <- image_read("CRU00291725-001_Scan1.ome.tif")
image_write(CRU00291725_00_image, path = "CRU00291725_100.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00291725_100.jpg") %>% as.raster()
xlim = c(0,8300)
ylim = c(0,13000)

base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into a region of interest/tile 
zoombackground = jpeg::readJPEG("CRU00291725_100.jpg") %>% as.raster()
xlim = c(0,3000)
ylim = c(5300, 13000)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00291725_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291725_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

##Tumour Gland:

#import nearest neighbour tumour gland 
nearest_neighbors_Gland_CRU00291725_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (8)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)


#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00291725_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291725_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291725_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00291725_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#plot immune cell over mif images 
base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom into a region of  interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291725_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291725_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00291725_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291725_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

####CRU00291732_001####

#import nearest neighbour file tumour stroma for CRU00291732_001
nearest_neighbors_Stroma_CRU00291732_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (9)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)
View(nearest_neighbors_Stroma_CRU00291732_001) #view data 

# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00291732_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291732_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00291732_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291732_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif and convert into pjeg format 
CRU00291732_001_image <- image_read("CRU00291732-001_Scan2.ome.tif")
image_write(CRU00291732_001_image, path="CRU00291732_001.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00291732_001.jpg") %>% as.raster()
xlim = c(0,11400)
ylim = c(0,15900)

#plot immune cell and overal co-ordinates on MIF images 
base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into a region of interest/tile 
zoombackground = jpeg::readJPEG("CRU00291732_001.jpg") %>% as.raster()
xlim = c(2500,9000)
ylim = c(0, 6000)

#overlay immune cell co-ordinates on MIF image 
zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00291732_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291732_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')


#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

##Tumour Gland:

#import nearest neighbour tumour gland 
nearest_neighbors_Gland_CRU00291732_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (9)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)
#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00291732_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00291732_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291732_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00291732_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#overlay immune co-ordinates on MIF image
base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom in region of interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291732_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291732_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00291732_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291732_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))


#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

########CRU00291743_001####

#import nearest neighbours from stroma
nearest_neighbors_Stroma_CRU00291743_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (10)/nearest_neighbors_Stroma.txt", 
                                                       delim = "\t", escape_double = FALSE, 
                                                       trim_ws = TRUE)
View(nearest_neighbors_Stroma_CRU00291743_001) #view data 

# Filter to just CD3+ and CD68+ cells
cd3_cells = nearest_neighbors_Stroma_CRU00291743_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291743_001, "CD3+"))
cd68_cells = nearest_neighbors_Stroma_CRU00291743_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291743_001, "CD68+"))

# For each CD3+ cell, join with the data for the nearest CD68+ cell
cd3_to_cd68 = cd3_cells %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                      suffix=c('', '.CD68'))

#set working directory mif (mif images)
setwd("C:/Users/cathy/OneDrive/Desktop/Research Project/MIF Images")

#import MIF tif abd covert into pjeg 
CRU00291743_001_image <- image_read("CRU00291743-001_Scan1.ome.tif")
image_write(CRU00291743_001_image, path="CRU00291743_001.jpg", format = "jpeg", quality = 75)

#import MIF images
background = jpeg::readJPEG("CRU00291743_001.jpg") %>% as.raster()
xlim = c(0,11000)
ylim = c(0,15000)

#overlay immune cell co-orinates on MIF images 
base_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+ cell  
base_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                  aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                  color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 

#zoom into a region of interest/tile 
zoombackground = jpeg::readJPEG("CRU00291743_001.jpg") %>% as.raster()
xlim = c(5900,11000)
ylim = c(3600, 10700)

zoombase_plot_cd3_cd68 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD68+'='cyan2'))
zoombase_plot_cd3_cd68 + geom_segment(data=cd3_to_cd68,
                                      aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                      color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=cd3_cells, aes(color='CD3+'), size=1) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+') 


#Filter to just CD3+/CD4+/PD1-/FoxP3- and CD68+
selector <- c("CD3+", "CD4+", "PD1-", "FoxP3-")
Cd3cd4pd1foxp3 = nearest_neighbors_Stroma_CRU00291743_001 %>% filter(select_rows(nearest_neighbors_Stroma_CRU00291743_001,selector))

#for each CD3+/CD4+/PD1-/FoxP3-, join with the data for the nearest CD68+ cell
Cd3cd4pd1foxp3_to_CD68 = Cd3cd4pd1foxp3 %>% left_join(cd68_cells, by=c('Cell ID CD68+'='Cell ID'), 
                                                      suffix=c('', '.CD68'))
#import the MIF image 
base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))

#overlay nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                        aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                        color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.5) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

#Zoom into region of interest 
zoom_base_plot_cd3cd4pd1foxp3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD4+PD1-FoxP3-'='yellow', 'CD68+'='cyan2'))
#overaly the nearest cd68+ to each cd3+Cd4+PD1-Foxp3- cell  
zoom_base_plot_cd3cd4pd1foxp3 + geom_segment(data=Cd3cd4pd1foxp3_to_CD68,
                                             aes(xend=`Cell X Position.CD68`, yend=`Cell Y Position.CD68`),
                                             color='white') +
  geom_point(data=cd68_cells, aes(color='CD68+'),size=1.25) +
  geom_point(data=Cd3cd4pd1foxp3, aes(color='CD3+CD4+PD1-FoxP3-'), size=0.50) + 
  labs(title='Tumour Stroma: Nearest CD68+ to each CD3+CD4+PD1-FoxP3-')

##Tumour Gland:

#import nearest neighbour tumour gland 
nearest_neighbors_Gland_CRU00291743_001 <- read_delim("C:/Users/cathy/OneDrive/Desktop/Research Project/PhenoptR Analysis Report/New folder (10)/nearest_neighbors_Gland.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      trim_ws = TRUE)
View(nearest_neighbors_Gland_CRU00291743_001)


#Filter to just CD3+ and CD3+CD8+PD1+ cells
cd3_cells = nearest_neighbors_Gland_CRU00291743_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00291743_001, "CD3+"))
#define selection of immune cells to filter 
selectorgland <- c("CD3+", "CD8+", "PD1+")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291743_001 %>% filter(select_rows(nearest_neighbors_Gland_CRU00291743_001, selectorgland))

#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+ cell
Cd3cd8pd1_cells_to_CD3 = cd3cd8pd1_cells  %>% left_join(cd3_cells, by=c('Cell ID CD3+'='Cell ID'), 
                                                        suffix=c('', '.CD3'))

#overlay immune cell co-ordinates on MIF images 
base_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#zoom into a region of  interest 
zoom_plot_Cd3cd8pd1_cells_to_CD3 = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+'='red', 'CD3+CD8+PD1+'='pink'))
zoom_plot_Cd3cd8pd1_cells_to_CD3 + geom_segment(data=Cd3cd8pd1_cells_to_CD3,
                                                aes(xend=`Cell X Position.CD3`, yend=`Cell Y Position.CD3`),
                                                color='white') +
  geom_point(data=cd3_cells, aes(color='CD3+'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+ to each CD3+CD8+PD1+')

#Tumour Gland: From CD3+/CD8+/PD1- to CD3+/CD8+/PD1+

#Filter to just CD3+CD8+PD1- and CD3+CD8+PD1+ cells
#define selection of immune cells to filter 
selectorglandcd3cd8pd1 <- c("CD3+", "CD8+", "PD1+")
selectorglandcd3cd8pdminus <- c("CD3+", "CD8+", "PD1-")
cd3cd8pd1_cells = nearest_neighbors_Gland_CRU00291743_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291743_001, selectorglandcd3cd8pd1))
cd3cd8pd1minus_cells = nearest_neighbors_Gland_CRU00291743_001%>% filter(select_rows(nearest_neighbors_Gland_CRU00291743_001, selectorglandcd3cd8pdminus))


#for each CD3+/CD8+/PD1, join with the data for the nearest CD3+CD8+PD1- cell
Cd3cd8pd1_cells_to_CD3CD8PD1minus = cd3cd8pd1_cells  %>% left_join(cd3cd8pd1minus_cells, by=c('Cell ID CD3+/CD8+/PD1-'='Cell ID'), 
                                                                   suffix=c('', '.CD3CD8PD1-'))

#Plot CD3+CD8+PD1+ nearest cells to CD3+CD8+PD1-
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(background,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
base_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                           aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                           color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')

#Zoom into  point of interest 
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus = ggplot(mapping=aes(`Cell X Position`, `Cell Y Position`)) %>% 
  phenoptr:::add_scales_and_background(zoombackground,xlim, ylim,  scale_color='white') +
  labs(x='Cell X Position', y='Cell Y Position') +   scale_color_manual('Phenotype', values=c('CD3+CD8+PD1-'='orange', 'CD3+CD8+PD1+'='pink'))
zoombase_plot_Cd3cd8pd1_cells_to_cd3cd8pd1minus + geom_segment(data=Cd3cd8pd1_cells_to_CD3CD8PD1minus,
                                                               aes(xend=`Cell X Position.CD3CD8PD1-`, yend=`Cell Y Position.CD3CD8PD1-`),
                                                               color='white') +
  geom_point(data=cd3cd8pd1minus_cells, aes(color='CD3+CD8+PD1-'),size=1) +
  geom_point(data=cd3cd8pd1_cells, aes(color='CD3+CD8+PD1+'), size=0.5) + 
  labs(title='Tumour Gland: Nearest CD3+CD8+PD1- to each CD3+CD8+PD1+')





