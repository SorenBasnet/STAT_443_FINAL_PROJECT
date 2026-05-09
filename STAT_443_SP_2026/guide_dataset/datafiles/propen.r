## Function for predicting least-squares mean of INTRDVXnonmiss
## Code produced by GUIDE 46.2 on 3/10/26 at 10:13
guide_predict <- function(){
 catvalues <- c("2","6","10","11","21","24","25","27","31","40","41","47","48","49")
 if(STATE %in% catvalues){
   if(is.na(INCLASS2)){
     nodeid <- 4
     predict <- 0.298977383238
   } else {
     nodeid <- 5
     predict <- 0.795223223052
   }
 } else {
   catvalues <- c("A")
   if(RETS_RVX %in% catvalues){
     if(!is.na(FINDRETX) & FINDRETX <= 391.500000000 ){
       if(!is.na(ERANKH) & ERANKH <= 0.652695150000 ){
         catvalues <- c("13","15","19","22","28","32","45")
         if(STATE %in% catvalues){
           nodeid <- 48
           predict <- 0.170224431218
         } else {
           nodeid <- 49
           predict <- 0.612516475497
         }
       } else {
         nodeid <- 25
         predict <- 0.351542944224
       }
     } else {
       nodeid <- 13
       predict <- 0.678091228947
     }
   } else {
     if(is.na(RETSURVX)){
       nodeid <- 14
       predict <- 0.104771777441
     } else {
       nodeid <- 15
       predict <- 0.646485256859
     }
   }
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## If desired, replace "ce2021miss.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("ce2021miss.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    FINDRETX <- as.numeric(newdata$FINDRETX[i])
    STATE <- as.character(newdata$STATE[i])
    ERANKH <- as.numeric(newdata$ERANKH[i])
    INCLASS2 <- as.numeric(newdata$INCLASS2[i])
    RETSURVX <- as.numeric(newdata$RETSURVX[i])
    RETS_RVX <- as.character(newdata$RETS_RVX[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}
