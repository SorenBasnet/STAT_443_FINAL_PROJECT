###
# Author : Soren Basnet 
# Program File : pat.R 
# Purpose : Program file is used to create a patient file with information regarding pathological and clinical information. No genetic information will be included. 
###


# Import required library
library(dplyr)
library(gtsummary)
library(kableExtra)

dataset_path <- "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/interim/"
dataset_path_raw <- "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/raw/cll_broad_2022"

# Read in data files
data_clinical_patient <- read.table(file.path(dataset_path,"data_clinical_patient_march_7.txt"),header=TRUE, sep=" ")
data_clinical_sample <- read.table(file.path(dataset_path, "data_clinical_sample_march_7.txt"), header=TRUE, sep=" ")

#Left join dataframe - clinical sample and clinical patient file
patient_file <- left_join(data_clinical_sample, data_clinical_patient, by = "PATIENT_ID")

#Add rai stage to the data_patient_cna 
data_timeline_treatment  <- read.table(file.path(dataset_path_raw,"data_timeline_treatment.txt"),header=TRUE, sep="\t") 
data_timeline_diagnosis  <- read.table(file.path(dataset_path_raw,"data_timeline_diagnosis.txt"),header=TRUE, sep="\t")             

# Make total rai stage file
total_rai <- left_join(data_timeline_diagnosis, data_timeline_treatment, by=c("PATIENT_ID"))
total_rai <- total_rai %>% select(c(PATIENT_ID, RAI_AT_DIAGNOSIS, RAI_AT_SAMPLING))

#Left join data_patient_cna with rai state
data_patient_cna <- left_join(patient_file, total_rai, by=c("PATIENT_ID"))

#Recode for death within 5 years of df - data_patient_cna 
data_patient_cna$surv_5yrs <- pmin(data_patient_cna$OS_MONTHS, 60)
data_patient_cna$surv_5yrs <- as.numeric(data_patient_cna$surv_5yrs)
data_patient_cna$death_5yr <- ifelse(data_patient_cna$OS_STATUS == "DECEASED" 
				     & data_patient_cna$OS_MONTHS <= 60, 1, 0)
data_patient_cna$death_5yr <- as.numeric(data_patient_cna$death_5yr)

#Save the datafile now 
write.table(data_patient_cna, "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/importance_score/pat/patient_file.txt", sep=" ", row.names = FALSE, quote = FALSE)
