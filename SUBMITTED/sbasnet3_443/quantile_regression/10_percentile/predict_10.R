## Function for predicting 0.10-quantile  of surv_5yrs
## Code produced by GUIDE 46.2 on 4/30/26 at 9:50
guide_predict <- function(){
 catvalues <- c("Loss")
 if(X17p_status %in% catvalues){
   nodeid <- 2
   predict <- 11.3400000000
 } else {
   catvalues <- c("Untreated")
   if(TREATMENT_STATUS_AT_SAMPLING %in% catvalues){
     if(is.na(IGHV_IDENTITY_PERCENTAGE) | IGHV_IDENTITY_PERCENTAGE <= 97.2000000000 ){
       nodeid <- 12
       predict <- 44.1200000000
     } else {
       nodeid <- 13
       predict <- 29.9500000000
     }
   } else {
     catvalues <- c("n_CLL")
     if(CLL_EPITYPE %in% catvalues){
       nodeid <- 14
       predict <- 0.300000000000
     } else {
       nodeid <- 15
       predict <- 30.6600000000
     }
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
    CLL_EPITYPE <- as.character(newdata$CLL_EPITYPE[i])
    IGHV_IDENTITY_PERCENTAGE <- as.numeric(newdata$IGHV_IDENTITY_PERCENTAGE[i])
    X17p_status <- as.character(newdata$X17p_status[i])
    TREATMENT_STATUS_AT_SAMPLING <- as.character(newdata$TREATMENT_STATUS_AT_SAMPLING[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}
