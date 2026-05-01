## Function for assigning node number and regression coefficients of death_5yr
## Code produced by GUIDE 46.2 on 4/25/26 at 13:34
guide_predict <- function(){
 nodeid <- 1
 coefs <- 0.00000000000
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
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    coefs <- c(coefs,tmp[-1])
}
