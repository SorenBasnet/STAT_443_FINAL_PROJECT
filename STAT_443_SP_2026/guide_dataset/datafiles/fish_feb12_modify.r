## Function for predicting class posterior probabilities of species
## Code produced by GUIDE 46.2 on 2/12/26 at 11:31
guide_predict <- function(){
 if(!is.na(heightpc) & heightpc <= 33.9000000000 ){
   if(!is.na(heightpc) & heightpc <= 20.1000000000 ){
     if(!is.na(weight) & weight <= 109.950000000 ){
       nodeid <- 8
       predclass <- "smelt"
       posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.99430E+00, 0.23585E-03)
     } else {
       nodeid <- 9
       predclass <- "pike"
       posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.99442E+00, 0.78616E-03, 0.55031E-03, 0.23585E-03)
     }
   } else {
     if(is.na(weight) | weight <= 452.000000000 ){
       if(!is.na(heightpc) & heightpc <= 25.2500000000 ){
         nodeid <- 20
         predclass <- "perch"
         posterior <- c( 0.13758E-02, 0.43239E-03, 0.95078E+00, 0.66824E-03, 0.45957E-01, 0.55031E-03, 0.23585E-03)
       } else {
         if(!is.na(length2) & !is.na(length3) &  -0.925770636221  * length3 + length2 <= -0.163032887570 ){
           nodeid <- 42
           predclass <- "roach"
           posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.66824E-03, 0.85902E+00, 0.55031E-03, 0.13575E+00)
         } else {
           nodeid <- 43
           predclass <- "perch"
           posterior <- c( 0.13758E-02, 0.43239E-03, 0.99595E+00, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
         }
       }
     } else {
       nodeid <- 11
       predclass <- "perch"
       posterior <- c( 0.13758E-02, 0.43239E-03, 0.86044E+00, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.13575E+00)
     }
   }
 } else {
   if(!is.na(length3) & length3 <= 29.5000000000 ){
     nodeid <- 6
     predclass <- "parkki"
     posterior <- c( 0.13758E-02, 0.99418E+00, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
   } else {
     nodeid <- 7
     predclass <- "bream"
     posterior <- c( 0.99513E+00, 0.43239E-03, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
   }
 }
 return(c(nodeid,predclass,posterior))
}
## end of function
##
##
## If desired, replace "fishdata.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("fishdata.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred.class contains predicted class
## prob contains predicted posterior probabilities
node <- NULL
pred.class <- NULL
prob <- NULL
for(i in 1:nrow(newdata)){
    weight <- as.numeric(newdata$weight[i])
    length2 <- as.numeric(newdata$length2[i])
    length3 <- as.numeric(newdata$length3[i])
    heightpc <- as.numeric(newdata$heightpc[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
