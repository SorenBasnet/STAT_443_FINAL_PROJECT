## Function for predicting class posterior probabilities of species
## Code produced by GUIDE 46.2 on 2/10/26 at 9:59
guide_predict <- function(){
 if(!is.na(height) & height <= 12.6370000000 ){
   if(!is.na(length2) & length2 <= 13.3500000000 ){
     nodeid <- 4
     predclass <- "smelt"
     posterior <- c( 0.13758E-02, 0.43239E-03, 0.78644E-01, 0.66824E-03, 0.78616E-03, 0.91786E+00, 0.23585E-03)
   } else {
     if(!is.na(length3) & length3 <= 47.3000000000 ){
       if(!is.na(length1) & length1 <= 25.1000000000 ){
         if(!is.na(height) & height <= 7.40100000000 ){
           catvalues <- c("unknown")
           if(sex %in% catvalues){
             if(!is.na(length3) & !is.na(weight) &  -6.92925188678  * weight + length3 <= -0.201775847633E-01 ){
               nodeid <- 160
               predclass <- "perch"
               posterior <- c( 0.13758E-02, 0.43239E-03, 0.90561E+00, 0.66824E-03, 0.91127E-01, 0.55031E-03, 0.23585E-03)
             } else {
               nodeid <- 161
               predclass <- "roach"
               posterior <- c( 0.13758E-02, 0.99807E-01, 0.20095E+00, 0.66824E-03, 0.69641E+00, 0.55031E-03, 0.23585E-03)
             }
           } else {
             if(!is.na(weight) & !is.na(length1) &  -53.2194097447  * length1 + weight <= -800.348319814 ){
               if(!is.na(length1) & !is.na(length3) &  -0.960407406223  * length3 + length1 <= -2.72353330562 ){
                 nodeid <- 324
                 predclass <- "roach"
                 posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.66824E-03, 0.99454E+00, 0.55031E-03, 0.23585E-03)
               } else {
                 nodeid <- 325
                 predclass <- "perch"
                 posterior <- c( 0.13758E-02, 0.43239E-03, 0.99595E+00, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
               }
             } else {
               nodeid <- 163
               predclass <- "smelt"
               posterior <- c( 0.13758E-02, 0.33168E+00, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.66305E+00, 0.23585E-03)
             }
           }
         } else {
           if(!is.na(height) & height <= 8.88480000000 ){
             if(!is.na(weight) & weight <= 154.500000000 ){
               nodeid <- 164
               predclass <- "parkki"
               posterior <- c( 0.13758E-02, 0.99418E+00, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
             } else {
               nodeid <- 165
               predclass <- "roach"
               posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.66824E-03, 0.59704E+00, 0.55031E-03, 0.39774E+00)
             }
           } else {
             if(!is.na(length3) & length3 <= 29.5000000000 ){
               nodeid <- 166
               predclass <- "parkki"
               posterior <- c( 0.13758E-02, 0.99418E+00, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
             } else {
               nodeid <- 167
               predclass <- "bream"
               posterior <- c( 0.99513E+00, 0.43239E-03, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
             }
           }
         }
       } else {
         if(!is.na(height) & height <= 6.93830000000 ){
           nodeid <- 42
           predclass <- "pike"
           posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.99442E+00, 0.78616E-03, 0.55031E-03, 0.23585E-03)
         } else {
           if(!is.na(length3) & length3 <= 35.7500000000 ){
             nodeid <- 86
             predclass <- "perch"
             posterior <- c( 0.84188E-01, 0.43239E-03, 0.66470E+00, 0.66824E-03, 0.83599E-01, 0.55031E-03, 0.16586E+00)
           } else {
             if(!is.na(height) & height <= 11.3125000000 ){
               if(!is.na(weight) & !is.na(height) &  -504.219377529  * height + weight <= -4000.68905953 ){
                 nodeid <- 348
                 predclass <- "perch"
                 posterior <- c( 0.13758E-02, 0.43239E-03, 0.99595E+00, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
               } else {
                 nodeid <- 349
                 predclass <- "pike"
                 posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.99442E+00, 0.78616E-03, 0.55031E-03, 0.23585E-03)
               }
             } else {
               if(!is.na(weight) & !is.na(weight) &  -22330.5903193  * weight + weight <= -151916.237784 ){
                 nodeid <- 350
                 predclass <- "perch"
                 posterior <- c( 0.13758E-02, 0.43239E-03, 0.99595E+00, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
               } else {
                 nodeid <- 351
                 predclass <- "whitefish"
                 posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.99399E+00)
               }
             }
           }
         }
       }
     } else {
       nodeid <- 11
       predclass <- "pike"
       posterior <- c( 0.13758E-02, 0.43239E-03, 0.22013E-02, 0.99442E+00, 0.78616E-03, 0.55031E-03, 0.23585E-03)
     }
   }
 } else {
   nodeid <- 3
   predclass <- "bream"
   posterior <- c( 0.96407E+00, 0.43239E-03, 0.33256E-01, 0.66824E-03, 0.78616E-03, 0.55031E-03, 0.23585E-03)
 }
 return(c(nodeid,predclass,posterior))
}
## end of function
##
##
## If desired, replace "fishdata_converted.csv" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.csv("fishdata_converted.csv",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred.class contains predicted class
## prob contains predicted posterior probabilities
node <- NULL
pred.class <- NULL
prob <- NULL
for(i in 1:nrow(newdata)){
    weight <- as.numeric(newdata$weight[i])
    length1 <- as.numeric(newdata$length1[i])
    length2 <- as.numeric(newdata$length2[i])
    length3 <- as.numeric(newdata$length3[i])
    sex <- as.character(newdata$sex[i])
    height <- as.numeric(newdata$height[i])
    weight <- as.numeric(newdata$weight[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
