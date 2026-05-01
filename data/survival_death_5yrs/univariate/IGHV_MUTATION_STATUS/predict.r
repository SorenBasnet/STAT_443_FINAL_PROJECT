## Function for assigning node number and regression coefficients of death_5yr
## Code produced by GUIDE 46.2 on 4/25/26 at 13:3
guide_predict <- function(){
 catvalues <- c("unmutated")
 if(IGHV_MUTATION_STATUS %in% catvalues){
   nodeid <- 2
   coefs <- 0.381061009574
 } else {
   nodeid <- 3
   coefs <- -0.466757417275
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
    IGHV_MUTATION_STATUS <- as.character(newdata$IGHV_MUTATION_STATUS[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    coefs <- c(coefs,tmp[-1])
}
