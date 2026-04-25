
# original is found in src/prediction_death_5_yrs
# Later version is from data/importance_score/pat_cna/01_pat_cna.R

library(dplyr)

dataset_path <- "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/raw/cll_broad_2022"

data_clinical_patient <- read.table(file.path(dataset_path,"data_clinical_patient.txt"),header=TRUE, sep="\t")
data_clinical_sample <- read.table(file.path(dataset_path, "data_clinical_sample.txt"), header=TRUE, sep="\t")


#Lets see what patient ID are getting repeated 
#table(data_clinical_sample$PATIENT_ID)[table(data_clinical_sample$PATIENT_ID) > 1]

dup_ids <- data_clinical_sample$PATIENT_ID[duplicated(data_clinical_sample$PATIENT_ID)]
data_dups <- data_clinical_sample[data_clinical_sample$PATIENT_ID %in% dup_ids, ]

#We will only keep the first occurence 
data_unique <- data_clinical_sample[!duplicated(data_clinical_sample$PATIENT_ID), ]


#Merge the 2 patient files
patient_file <- left_join(data_unique, data_clinical_patient, by = "PATIENT_ID")

data_armlevel_cna   <- read.table(file.path(dataset_path,"data_armlevel_cna.txt"),header=TRUE, sep="\t") 

#Change the column names to something suitable for merge 
colnames(data_armlevel_cna) <- gsub("\\.", "-", colnames(data_armlevel_cna))

#Add colnames - 11q_status, 17p_status, 17q_status, etc to the patient file 
cna_mat <- data_armlevel_cna[, -(1:3)]
cna_t <- as.data.frame(t(cna_mat))
colnames(cna_t) <- data_armlevel_cna$ENTITY_STABLE_ID
cna_t$SAMPLE_ID <- rownames(cna_t)
rownames(cna_t) <- NULL

#Merge the patient file with cna_t file 
patient_file_cna <- merge(patient_file, cna_t, by = "SAMPLE_ID", all.x = TRUE)
patient_file_cna$PATIENT_ID <- gsub("-", "_", patient_file_cna$PATIENT_ID)

#Rai Stage is another important variable required for good predicting
data_timeline_treatment  <- read.table(file.path(dataset_path,"data_timeline_treatment.txt"),header=TRUE, sep="\t") 

################## ################## ################## ################## ##################

dataset_path_raw <- "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/raw/cll_broad_2022"

#Add rai stage to the data_patient_cna 
data_timeline_treatment  <- read.table(file.path(dataset_path_raw,"data_timeline_treatment.txt"),header=TRUE, sep="\t") 
data_timeline_diagnosis  <- read.table(file.path(dataset_path_raw,"data_timeline_diagnosis.txt"),header=TRUE, sep="\t")             

# Make total rai stage file
total_rai <- left_join(data_timeline_diagnosis, data_timeline_treatment, by=c("PATIENT_ID"))
total_rai <- total_rai %>% select(c(PATIENT_ID, RAI_AT_DIAGNOSIS, RAI_AT_SAMPLING))
total_rai$PATIENT_ID <- gsub("-", "_", total_rai$PATIENT_ID)

colnames(patient_file_cna)
patient_file_cna_rai <- left_join(patient_file_cna, total_rai, by=c("PATIENT_ID"))
colnames(patient_file_cna_rai)
summary(patient_file_cna_rai$RAI_AT_SAMPLING)
################################################################################

#Left join the patient_file_cna and data_timeline_treatment
#patient_file_cna_rai <- merge(patient_file_cna, data_timeline_treatment, by="SAMPLE_ID", all.x = TRUE)

#Take some columns out
#patient_file_cna_rai$PATIENT_ID <- patient_file_cna_rai$PATIENT_ID.x
#patient_file_cna_rai <- patient_file_cna_rai %>% select(-PATIENT_ID.x, -PATIENT_ID.y, -CENTER, -EVENT_TYPE, -START_DATE, -STOP_DATE)

#Remove any spaces or unwanted syntax
#patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub(",", "__", .)))
#patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub(" ", "__", .)))
#patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub("-", "_", .)))
#patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub("/", "_", .)))

################################################################################

#Remove any spaces or unwanted syntax
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub(",", "__", .)))
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub(" ", "__", .)))
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub("-", "_", .)))
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub("/", "_", .)))


#Make the OS_Survival for 5 years of less into 1 or 0 
patient_file_cna_rai$surv_5yrs <- pmin(patient_file_cna_rai$OS_MONTHS, 60)
patient_file_cna_rai$surv_5yrs <- as.numeric(patient_file_cna_rai$surv_5yrs)
patient_file_cna_rai$death_5yr <- ifelse(patient_file_cna_rai$OS_STATUS == "DECEASED" & patient_file_cna_rai$OS_MONTHS <= 60, 1, 0)
patient_file_cna_rai$death_5yr <- as.numeric(patient_file_cna_rai$death_5yr)


patient_file_cna_rai[is.na(patient_file_cna_rai)] <- NA
patient_file_cna_rai[patient_file_cna_rai == ""] <- NA
patient_file_cna_rai$AGE_SAMPLING <- as.numeric(patient_file_cna_rai$AGE_SAMPLING)
colnames(patient_file_cna_rai)

######################################## 
# Add mutations now 

mutations <- read.table("/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/genetic_contributors/data_patient_mutation_march_8.txt", header=TRUE)
mutations <- mutations %>% select(1, 42:last_col())

patient_file_cna_rai_mutations <- left_join(patient_file_cna_rai, mutations, by=c("PATIENT_ID"))

#Write the patient_file_cna_rai to prediction_death_5_years dataset 
write.table(patient_file_cna_rai_mutations, "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/importance_score/pat_cna_mutations/patient_cna_rai_mutation.txt", sep=" ", row.names = FALSE, quote = FALSE)
colnames(patient_file_cna_rai_mutations)
