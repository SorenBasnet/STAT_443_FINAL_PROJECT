## Function for predicting class posterior probabilities of Gender
## Code produced by GUIDE 46.1 on 2/3/26 at 11:31
guide_predict <- function(){
 if(!is.na(Shouldergirth) & Shouldergirth <= 107.400000000 ){
   if(!is.na(Elbowdiam) & Elbowdiam <= 13.4500000000 ){
     if(!is.na(Thighgirth) & Thighgirth <= 50.9000000000 ){
       if(!is.na(Wristdiam) & Wristdiam <= 9.75000000000 ){
         nodeid <- 16
         predclass <- "F"
         posterior <- c( 0.99904E+00, 0.95901E-03)
       } else {
         nodeid <- 17
         predclass <- "M"
         posterior <- c( 0.10095E-02, 0.99899E+00)
       }
     } else {
       nodeid <- 9
       predclass <- "F"
       posterior <- c( 0.99065E+00, 0.93458E-02)
     }
   } else {
     if(!is.na(Weight) & Weight <= 65.0500000000 ){
       nodeid <- 10
       predclass <- "M"
       posterior <- c( 0.10095E-02, 0.99899E+00)
     } else {
       if(!is.na(Anklediam) & Anklediam <= 14.5000000000 ){
         nodeid <- 22
         predclass <- "F"
         posterior <- c( 0.99904E+00, 0.95901E-03)
       } else {
         nodeid <- 23
         predclass <- "M"
         posterior <- c( 0.10095E-02, 0.99899E+00)
       }
     }
   }
 } else {
   if(!is.na(Biacromialdiam) & Biacromialdiam <= 38.8500000000 ){
     if(!is.na(Hipgirth) & Hipgirth <= 98.0000000000 ){
       nodeid <- 12
       predclass <- "M"
       posterior <- c( 0.19048E+00, 0.80952E+00)
     } else {
       if(!is.na(Thighgirth) & !is.na(Forearmgirth) &  -3.18397631715  * Forearmgirth + Thighgirth <= -25.3289333276 ){
         nodeid <- 26
         predclass <- "M"
         posterior <- c( 0.10095E-02, 0.99899E+00)
       } else {
         nodeid <- 27
         predclass <- "F"
         posterior <- c( 0.99904E+00, 0.95901E-03)
       }
     }
   } else {
     if(!is.na(Biacromialdiam) & Biacromialdiam <= 40.0500000000 ){
       if(!is.na(Wristmingirth) & Wristmingirth <= 16.4500000000 ){
         nodeid <- 28
         predclass <- "F"
         posterior <- c( 0.77778E+00, 0.22222E+00)
       } else {
         nodeid <- 29
         predclass <- "M"
         posterior <- c( 0.64516E-01, 0.93548E+00)
       }
     } else {
       nodeid <- 15
       predclass <- "M"
       posterior <- c( 0.55556E-02, 0.99444E+00)
     }
   }
 }
 return(c(nodeid,predclass,posterior))
}
## end of function
##
##
## If desired, replace "bodydata.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("bodydata.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred.class contains predicted class
## prob contains predicted posterior probabilities
node <- NULL
pred.class <- NULL
prob <- NULL
for(i in 1:nrow(newdata)){
    Biacromialdiam <- as.numeric(newdata$Biacromialdiam[i])
    Elbowdiam <- as.numeric(newdata$Elbowdiam[i])
    Wristdiam <- as.numeric(newdata$Wristdiam[i])
    Anklediam <- as.numeric(newdata$Anklediam[i])
    Shouldergirth <- as.numeric(newdata$Shouldergirth[i])
    Hipgirth <- as.numeric(newdata$Hipgirth[i])
    Thighgirth <- as.numeric(newdata$Thighgirth[i])
    Forearmgirth <- as.numeric(newdata$Forearmgirth[i])
    Wristmingirth <- as.numeric(newdata$Wristmingirth[i])
    Weight <- as.numeric(newdata$Weight[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
