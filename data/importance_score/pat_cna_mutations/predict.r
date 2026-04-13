## Function for predicting class posterior probabilities of death_5yr
## Code produced by GUIDE 46.2 on 4/6/26 at 21:51
guide_predict <- function(){
 catvalues <- c("Loss")
 if(X17p_status %in% catvalues){
   catvalues <- c("0")
   if(CLL_EPITYPE %in% catvalues){
     catvalues <- c("Unchanged")
     if(tri12_status %in% catvalues){
       nodeid <- 8
       predclass <- "1"
       posterior <- c( 0.22596E-03, 0.99977E+00)
     } else {
       nodeid <- 9
       predclass <- "0"
       posterior <- c( 0.99995E+00, 0.48308E-04)
     }
   } else {
     if(!is.na(RAI_AT_SAMPLING) & RAI_AT_SAMPLING <= 2.50000000000 ){
       if(is.na(IGHV_IDENTITY_PERCENTAGE) | IGHV_IDENTITY_PERCENTAGE <= 99.8850000000 ){
         catvalues <- c("Chemo__+__no__Ab")
         if(PRIOR_TREATMENT_CATEGORY %in% catvalues){
           nodeid <- 40
           predclass <- "1"
           posterior <- c( 0.22596E-03, 0.99977E+00)
         } else {
           nodeid <- 41
           predclass <- "0"
           posterior <- c( 0.91837E+00, 0.81633E-01)
         }
       } else {
         if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 67.0500000000 ){
           catvalues <- c("Chemo__+__Ab")
           if(PRIOR_TREATMENT_CATEGORY %in% catvalues){
             nodeid <- 84
             predclass <- "1"
             posterior <- c( 0.22596E-03, 0.99977E+00)
           } else {
             nodeid <- 85
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48308E-04)
           }
         } else {
           catvalues <- c("Unchanged")
           if(X18p_status %in% catvalues){
             catvalues <- c("Unchanged")
             if(X8q_status %in% catvalues){
               nodeid <- 172
               predclass <- "1"
               posterior <- c( 0.64516E-01, 0.93548E+00)
             } else {
               nodeid <- 173
               predclass <- "0"
               posterior <- c( 0.99995E+00, 0.48308E-04)
             }
           } else {
             nodeid <- 87
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48308E-04)
           }
         }
       }
     } else {
       nodeid <- 11
       predclass <- "0"
       posterior <- c( 0.85345E+00, 0.14655E+00)
     }
   }
 } else {
   if(is.na(AGE) | AGE <= 67.5000000000 ){
     catvalues <- c("n_CLL")
     if(CLL_EPITYPE %in% catvalues){
       catvalues <- c("U_CLL")
       if(TUMOR_MOLECULAR_SUBTYPE %in% catvalues){
         catvalues <- c("NA")
         catvalues <- c(catvalues,NA)
         if(is.na(U1_STATUS) | U1_STATUS %in% catvalues){
           nodeid <- 48
           predclass <- "1"
           posterior <- c( 0.24000E+00, 0.76000E+00)
         } else {
           nodeid <- 49
           predclass <- "0"
           posterior <- c( 0.74174E+00, 0.25826E+00)
         }
       } else {
         catvalues <- c("Chemo__+__no__Ab")
         if(PRIOR_TREATMENT_CATEGORY %in% catvalues){
           nodeid <- 50
           predclass <- "1"
           posterior <- c( 0.22596E-03, 0.99977E+00)
         } else {
           nodeid <- 51
           predclass <- "0"
           posterior <- c( 0.88909E+00, 0.11091E+00)
         }
       }
     } else {
       nodeid <- 13
       predclass <- "0"
       posterior <- c( 0.94284E+00, 0.57160E-01)
     }
   } else {
     catvalues <- c("EC_m1","EC_m2","EC_u1","EC_u2")
     if(EXPRESSION_CLUSTER %in% catvalues){
       if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 53.1000000000 ){
         if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 94.2400000000 ){
           if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 91.7750000000 ){
             nodeid <- 112
             predclass <- "1"
             posterior <- c( 0.22596E-03, 0.99977E+00)
           } else {
             nodeid <- 113
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48308E-04)
           }
         } else {
           nodeid <- 57
           predclass <- "1"
           posterior <- c( 0.22596E-03, 0.99977E+00)
         }
       } else {
         if(!is.na(AGE) & AGE <= 73.5000000000 ){
           if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 68.9500000000 ){
             nodeid <- 116
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48308E-04)
           } else {
             if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 82.7500000000 ){
               catvalues <- c("WT")
               if(U1_STATUS %in% catvalues){
                 nodeid <- 468
                 predclass <- "1"
                 posterior <- c( 0.22596E-03, 0.99977E+00)
               } else {
                 nodeid <- 469
                 predclass <- "0"
                 posterior <- c( 0.99995E+00, 0.48308E-04)
               }
             } else {
               nodeid <- 235
               predclass <- "0"
               posterior <- c( 0.88372E+00, 0.11628E+00)
             }
           }
         } else {
           nodeid <- 59
           predclass <- "1"
           posterior <- c( 0.43548E+00, 0.56452E+00)
         }
       }
     } else {
       if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 1.31666666650 ){
         if(!is.na(RAI_AT_DIAGNOSIS) & RAI_AT_DIAGNOSIS <= 1.50000000000 ){
           catvalues <- c("Chemo__+__no__Ab")
           if(PRIOR_TREATMENT_CATEGORY %in% catvalues){
             nodeid <- 120
             predclass <- "1"
             posterior <- c( 0.22596E-03, 0.99977E+00)
           } else {
             nodeid <- 121
             predclass <- "0"
             posterior <- c( 0.90037E+00, 0.99631E-01)
           }
         } else {
           nodeid <- 61
           predclass <- "1"
           posterior <- c( 0.30000E+00, 0.70000E+00)
         }
       } else {
         nodeid <- 31
         predclass <- "0"
         posterior <- c( 0.54615E+00, 0.45385E+00)
       }
     }
   }
 }
 return(c(nodeid,predclass,posterior))
}
## end of function
##
##
## If desired, replace "patient_cna_rai_mutation.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("patient_cna_rai_mutation.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred.class contains predicted class
## prob contains predicted posterior probabilities
node <- NULL
pred.class <- NULL
prob <- NULL
for(i in 1:nrow(newdata)){
    NORMAL_MEAN_COVERAGE <- as.numeric(newdata$NORMAL_MEAN_COVERAGE[i])
    TUMOR_MEAN_COVERAGE <- as.numeric(newdata$TUMOR_MEAN_COVERAGE[i])
    CLL_EPITYPE <- as.character(newdata$CLL_EPITYPE[i])
    TUMOR_MOLECULAR_SUBTYPE <- as.character(newdata$TUMOR_MOLECULAR_SUBTYPE[i])
    U1_STATUS <- as.character(newdata$U1_STATUS[i])
    TMB_NONSYNONYMOUS <- as.numeric(newdata$TMB_NONSYNONYMOUS[i])
    AGE <- as.numeric(newdata$AGE[i])
    IGHV_IDENTITY_PERCENTAGE <- as.numeric(newdata$IGHV_IDENTITY_PERCENTAGE[i])
    PRIOR_TREATMENT_CATEGORY <- as.character(newdata$PRIOR_TREATMENT_CATEGORY[i])
    EXPRESSION_CLUSTER <- as.character(newdata$EXPRESSION_CLUSTER[i])
    X17p_status <- as.character(newdata$X17p_status[i])
    X18p_status <- as.character(newdata$X18p_status[i])
    X8q_status <- as.character(newdata$X8q_status[i])
    tri12_status <- as.character(newdata$tri12_status[i])
    RAI_AT_DIAGNOSIS <- as.numeric(newdata$RAI_AT_DIAGNOSIS[i])
    RAI_AT_SAMPLING <- as.numeric(newdata$RAI_AT_SAMPLING[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
