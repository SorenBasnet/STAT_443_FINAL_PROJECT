
# Note for self : Original is found in src/prediction_death_5_yrs

library(dplyr)

# File path
dataset_path <- "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/raw/cll_broad_2022"

# dataset
data_clinical_patient <- read.table(file.path(dataset_path,"data_clinical_patient.txt"),header=TRUE, sep="\t")
data_clinical_sample <- read.table(file.path(dataset_path, "data_clinical_sample.txt"), header=TRUE, sep="\t")

# See what patient ID are getting repeated 
# .table(data_clinical_sample$PATIENT_ID)[table(data_clinical_sample$PATIENT_ID) > 1]

dup_ids <- data_clinical_sample$PATIENT_ID[duplicated(data_clinical_sample$PATIENT_ID)]
data_dups <- data_clinical_sample[data_clinical_sample$PATIENT_ID %in% dup_ids, ]

# We will only keep the first occurance 
data_unique <- data_clinical_sample[!duplicated(data_clinical_sample$PATIENT_ID), ]

# Merge the 2 patient files
patient_file <- left_join(data_unique, data_clinical_patient, by = "PATIENT_ID")

data_armlevel_cna   <- read.table(file.path(dataset_path,"data_armlevel_cna.txt"),header=TRUE, sep="\t") 

# Change the column names to something suitable for merge 
colnames(data_armlevel_cna) <- gsub("\\.", "-", colnames(data_armlevel_cna))

# Add colnames - 11q_status, 17p_status, 17q_status, etc to the patient file 
cna_mat <- data_armlevel_cna[, -(1:3)]
cna_t <- as.data.frame(t(cna_mat))
colnames(cna_t) <- data_armlevel_cna$ENTITY_STABLE_ID
cna_t$SAMPLE_ID <- rownames(cna_t)
rownames(cna_t) <- NULL

# Merge the patient file with cna_t file 
patient_file_cna <- merge(patient_file, cna_t, by = "SAMPLE_ID", all.x = TRUE)

# Rai Stage is another important variable required for good predicting
data_timeline_treatment  <- read.table(file.path(dataset_path,"data_timeline_treatment.txt"),header=TRUE, sep="\t") 

# Rai stage at Daignosis
data_timeline_diagnosis  <- read.table(file.path(dataset_path,"data_timeline_diagnosis.txt"),header=TRUE, sep="\t")                 

# Left join the data_timeline_treatment and data_time_diagnosis 
total_rai <- merge(data_timeline_diagnosis, data_timeline_treatment, by="PATIENT_ID", all.x = TRUE)
total_rai$SAMPLE_ID <- total_rai$SAMPLE_ID.x
total_rai <- total_rai %>% select(-PATIENT_ID, -START_DATE.x, -START_DATE.y, 
                                  -STOP_DATE.x, -STOP_DATE.y, -EVENT_TYPE.x, -EVENT_TYPE.y, 
                                  -AGE_AT_DIAGNOSIS, -SAMPLE_ID.y, -SAMPLE_ID.x)


# Left join the patient_file_cna and data_timeline_treatment
patient_file_cna_rai <- merge(patient_file_cna, total_rai, by="SAMPLE_ID", all.x = TRUE)

# Take some columns out
patient_file_cna_rai <- patient_file_cna_rai %>% select(-CENTER)

# Remove any spaces or unwanted syntax
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub(",", "__", .)))
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub(" ", "__", .)))
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub("-", "_", .)))
patient_file_cna_rai <- patient_file_cna_rai %>% mutate(across(where(is.character), ~ gsub("/", "_", .)))

# Make the OS_Survival for 5 years of less into 1 or 0 
patient_file_cna_rai$surv_5yrs <- pmin(patient_file_cna_rai$OS_MONTHS, 60)
patient_file_cna_rai$surv_5yrs <- as.numeric(patient_file_cna_rai$surv_5yrs)
patient_file_cna_rai$death_5yr <- ifelse(patient_file_cna_rai$OS_STATUS == "DECEASED" & patient_file_cna_rai$OS_MONTHS <= 60, 1, 0)
patient_file_cna_rai$death_5yr <- as.numeric(patient_file_cna_rai$death_5yr)
patient_file_cna_rai[is.na(patient_file_cna_rai)] <- NA
patient_file_cna_rai[patient_file_cna_rai == ""] <- NA
patient_file_cna_rai$AGE_SAMPLING <- as.numeric(patient_file_cna_rai$AGE_SAMPLING)

# Write the patient_file_cna_rai to prediction_death_5_years dataset 
write.table(patient_file_cna_rai, "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/data_patient_file_cna_rai/patient_file_cna_rai.txt", sep=" ", row.names = FALSE, quote = FALSE)
write.table(patient_file_cna_rai, "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/importance_score/pat_cna/patient_file_cna_rai.txt", sep=" ", row.names = FALSE, quote = FALSE)




