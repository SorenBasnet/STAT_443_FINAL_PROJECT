## Function for predicting least-squares mean of INTRDVX
## Code produced by GUIDE 46.2 on 2/19/26 at 10:21
guide_predict <- function(){
 catvalues <- c("5")
 if(REFGEN %in% catvalues){
   nodeid <- 2
   predict <- 980.352916577
 } else {
   if(!is.na(INC_RANK) & INC_RANK <= 0.840186250000 ){
     nodeid <- 6
     predict <- 2822.64453247
   } else {
     catvalues <- c("8")
     if(EARNCOMP %in% catvalues){
       catvalues <- c("1")
       if(RETSURV %in% catvalues){
         nodeid <- 28
         predict <- 27641.2815914
       } else {
         nodeid <- 29
         predict <- 85859.2758334
       }
     } else {
       if(!is.na(FFTAXOWE) & FFTAXOWE <= 27769.5000000 ){
         nodeid <- 30
         predict <- 2646.53671681
       } else {
         if(is.na(AGE2) | AGE2 <= 56.5000000000 ){
           nodeid <- 62
           predict <- 8036.33405691
         } else {
           if(!is.na(BATHRMQ) & BATHRMQ <= 4.50000000000 ){
             catvalues <- c("6","17","21","24","27","29","32","37","42")
             if(STATE %in% catvalues){
               nodeid <- 252
               predict <- 41453.9950552
             } else {
               nodeid <- 253
               predict <- 5032.11410402
             }
           } else {
             nodeid <- 127
             predict <- 124323.809284
           }
         }
       }
     }
   }
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## If desired, replace "ce2021.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("ce2021.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    AGE2 <- as.numeric(newdata$AGE2[i])
    BATHRMQ <- as.numeric(newdata$BATHRMQ[i])
    EARNCOMP <- as.character(newdata$EARNCOMP[i])
    INC_RANK <- as.numeric(newdata$INC_RANK[i])
    STATE <- as.character(newdata$STATE[i])
    RETSURV <- as.character(newdata$RETSURV[i])
    REFGEN <- as.character(newdata$REFGEN[i])
    FFTAXOWE <- as.numeric(newdata$FFTAXOWE[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}
