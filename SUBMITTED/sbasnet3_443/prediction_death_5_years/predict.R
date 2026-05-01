## Function for predicting class posterior probabilities of death_5yr
## Code produced by GUIDE 46.2 on 4/30/26 at 10:26
guide_predict <- function(){
 if(!is.na(AGE_SAMPLING) & AGE_SAMPLING <= 74.5000000000 ){
   nodeid <- 2
   predclass <- "0"
   posterior <- c( 0.86982E+00, 0.13018E+00)
 } else {
   catvalues <- c("unmutated")
   if(IGHV_MUTATION_STATUS %in% catvalues){
     catvalues <- c("Yes")
     if(ARRAY450K %in% catvalues){
       nodeid <- 12
       predclass <- "1"
       posterior <- c( 0.32432E+00, 0.67568E+00)
     } else {
       nodeid <- 13
       predclass <- "0"
       posterior <- c( 0.73684E+00, 0.26316E+00)
     }
   } else {
     nodeid <- 7
     predclass <- "0"
     posterior <- c( 0.77570E+00, 0.22430E+00)
   }
 }
 return(c(nodeid,predclass,posterior))
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
## pred.class contains predicted class
## prob contains predicted posterior probabilities
node <- NULL
pred.class <- NULL
prob <- NULL
for(i in 1:nrow(newdata)){
    ARRAY450K <- as.character(newdata$ARRAY450K[i])
    AGE_SAMPLING <- as.numeric(newdata$AGE_SAMPLING[i])
    IGHV_MUTATION_STATUS <- as.character(newdata$IGHV_MUTATION_STATUS[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
