## Function for assigning node number and regression coefficients of death_5yr
## Code produced by GUIDE 46.2 on 4/30/26 at 20:6
guide_predict <- function(){
 catvalues <- c("Loss")
 if(X17p_status %in% catvalues){
   nodeid <- 2
   coefs <- 1.17674289440
 } else {
   if(!is.na(AGE_SAMPLING) & AGE_SAMPLING <= 74.5000000000 ){
     nodeid <- 6
     coefs <- -0.446096015221
   } else {
     nodeid <- 7
     coefs <- 0.814716677153
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
## coefs contain regression coefficients
node <- NULL
coefs <- NULL
for(i in 1:nrow(newdata)){
    AGE_SAMPLING <- as.numeric(newdata$AGE_SAMPLING[i])
    X17p_status <- as.character(newdata$X17p_status[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    coefs <- c(coefs,tmp[-1])
}
