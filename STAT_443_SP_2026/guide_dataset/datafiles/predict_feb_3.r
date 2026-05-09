## Function for predicting class posterior probabilities of Gender
## Code produced by GUIDE 46.1 on 2/3/26 at 11:20
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
     nodeid <- 5
     predclass <- "M"
     posterior <- c( 0.36364E+00, 0.63636E+00)
   }
 } else {
   if(!is.na(Biacromialdiam) & Biacromialdiam <= 38.8500000000 ){
     if(!is.na(Hipgirth) & Hipgirth <= 98.0000000000 ){
       if(!is.na(Elbowdiam) & !is.na(Calfmaxgirth) &  -0.147932850459  * Calfmaxgirth + Elbowdiam <= 7.85962409841 ){
         nodeid <- 24
         predclass <- "F"
         posterior <- c( 0.99904E+00, 0.95901E-03)
       } else {
         nodeid <- 25
         predclass <- "M"
         posterior <- c( 0.55556E-01, 0.94444E+00)
       }
     } else {
       if(!is.na(Bitrochantericdiam) & !is.na(Anklediam) &  -1.52526353830  * Anklediam + Bitrochantericdiam <= 10.1872046870 ){
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
    Bitrochantericdiam <- as.numeric(newdata$Bitrochantericdiam[i])
    Elbowdiam <- as.numeric(newdata$Elbowdiam[i])
    Wristdiam <- as.numeric(newdata$Wristdiam[i])
    Anklediam <- as.numeric(newdata$Anklediam[i])
    Shouldergirth <- as.numeric(newdata$Shouldergirth[i])
    Hipgirth <- as.numeric(newdata$Hipgirth[i])
    Thighgirth <- as.numeric(newdata$Thighgirth[i])
    Calfmaxgirth <- as.numeric(newdata$Calfmaxgirth[i])
    Wristmingirth <- as.numeric(newdata$Wristmingirth[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
