## Function for predicting class posterior probabilities of Gender
## Code produced by GUIDE 46.0 on 1/27/26 at 14:43
guide_predict <- function(){
 if(!is.na(Forearmgirth) & Forearmgirth <= 25.6500000000 ){
   if(!is.na(Forearmgirth) & Forearmgirth <= 24.7500000000 ){
     nodeid <- 4
     predclass <- "F"
     posterior <- c( 0.98010E+00, 0.19900E-01)
   } else {
     if(!is.na(Thighgirth) & Thighgirth <= 56.0500000000 ){
       nodeid <- 10
       predclass <- "M"
       posterior <- c( 0.66667E-01, 0.93333E+00)
     } else {
       nodeid <- 11
       predclass <- "F"
       posterior <- c( 0.99904E+00, 0.95901E-03)
     }
   }
 } else {
   if(!is.na(Biacromialdiam) & Biacromialdiam <= 39.1500000000 ){
     if(!is.na(Bitrochantericdiam) & Bitrochantericdiam <= 33.3500000000 ){
       if(!is.na(Calfmaxgirth) & Calfmaxgirth <= 36.3500000000 ){
         nodeid <- 24
         predclass <- "M"
         posterior <- c( 0.58824E-01, 0.94118E+00)
       } else {
         if(!is.na(Shouldergirth) & Shouldergirth <= 112.500000000 ){
           nodeid <- 50
           predclass <- "F"
           posterior <- c( 0.83333E+00, 0.16667E+00)
         } else {
           nodeid <- 51
           predclass <- "M"
           posterior <- c( 0.10095E-02, 0.99899E+00)
         }
       }
     } else {
       nodeid <- 13
       predclass <- "F"
       posterior <- c( 0.94737E+00, 0.52632E-01)
     }
   } else {
     if(!is.na(Weight) & Weight <= 77.5000000000 ){
       if(!is.na(Hipgirth) & !is.na(Weight) &  -0.407879970408  * Weight + Hipgirth <= 72.4004281765 ){
         nodeid <- 28
         predclass <- "M"
         posterior <- c( 0.10095E-02, 0.99899E+00)
       } else {
         nodeid <- 29
         predclass <- "F"
         posterior <- c( 0.99904E+00, 0.95901E-03)
       }
     } else {
       nodeid <- 15
       predclass <- "M"
       posterior <- c( 0.25641E-01, 0.97436E+00)
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
    Shouldergirth <- as.numeric(newdata$Shouldergirth[i])
    Hipgirth <- as.numeric(newdata$Hipgirth[i])
    Thighgirth <- as.numeric(newdata$Thighgirth[i])
    Forearmgirth <- as.numeric(newdata$Forearmgirth[i])
    Calfmaxgirth <- as.numeric(newdata$Calfmaxgirth[i])
    Weight <- as.numeric(newdata$Weight[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
