
#Create dsc function
create_dsc_file <- function(file_name, datafile_path) {
  z <- read.table(file.path(datafile_path, file_name),header=TRUE)
  nvars <- ncol(z)
  write(file_name,file=file.path(datafile_path,paste0("dsc_",file_name)))
  write("NA",file=file.path(datafile_path,paste0("dsc_",file_name)),append=TRUE)
  write("2",file=file.path(datafile_path,paste0("dsc_",file_name)),append=TRUE)
  roles <- ifelse(sapply(z, function(x) is.numeric(x) | is.integer(x)), "n", "c")
  write.table(cbind(1:nvars,names(z),roles),
              file=file.path(datafile_path,paste0("dsc_",file_name)), 
              col.names=FALSE,row.names=FALSE,append=TRUE,quote=FALSE)
}


#Dsc file for patient_file for data_patient_cna.txt
create_dsc_file(
  file_name <- "patient_file_cna_rai.txt",
  datafile_path <- "/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/src/simulation"
)
