## Function for predicting 0.50-quantile  of surv_5yrs
## Code produced by GUIDE 46.2 on 4/30/26 at 9:44
guide_predict <- function(){
 catvalues <- c("Treated")
 if(TREATMENT_STATUS_AT_SAMPLING %in% catvalues){
   catvalues <- c("DFCI")
   if(COHORT %in% catvalues){
     nodeid <- 4
     predict <- 35.4050000000
   } else {
     nodeid <- 5
     predict <- 60.0000000000
   }
 } else {
   if(!is.na(AGE_SAMPLING) & AGE_SAMPLING <= 85.5000000000 ){
     nodeid <- 6
     predict <- 60.0000000000
   } else {
     nodeid <- 7
     predict <- 43.4650000000
   }
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## If desired, replace "patient_file_cna_rai.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("patient_file_cna_rai.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    AGE_SAMPLING <- as.numeric(newdata$AGE_SAMPLING[i])
    COHORT <- as.character(newdata$COHORT[i])
    TREATMENT_STATUS_AT_SAMPLING <- as.character(newdata$TREATMENT_STATUS_AT_SAMPLING[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}
