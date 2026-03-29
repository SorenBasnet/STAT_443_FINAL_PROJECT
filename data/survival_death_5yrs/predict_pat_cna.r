## Function for predicting class posterior probabilities of death_5yr
## Code produced by GUIDE 46.2 on 3/28/26 at 22:49
guide_predict <- function(){
 catvalues <- c("PI3Kinhibitor","SRCinhibitor")
 if(TREATMENT_AFTER_SAMPLING %in% catvalues){
   nodeid <- 2
   predclass <- "1"
   posterior <- c( 0.22559E-03, 0.99977E+00)
 } else {
   catvalues <- c("Yes")
   if(U_CLL_WES %in% catvalues){
     catvalues <- c("Chemo+Ab","Chemo+noAb")
     if(PRIOR_TREATMENT_CATEGORY %in% catvalues){
       if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 0.683333333500 ){
         nodeid <- 24
         predclass <- "0"
         posterior <- c( 0.99995E+00, 0.48684E-04)
       } else {
         if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 2.00500000000 ){
           catvalues <- c("NA")
           catvalues <- c(catvalues,NA)
           if(is.na(TREATMENT_AFTER_SAMPLING) | TREATMENT_AFTER_SAMPLING %in% catvalues){
             nodeid <- 100
             predclass <- "1"
             posterior <- c( 0.22559E-03, 0.99977E+00)
           } else {
             if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 1.98500000000 ){
               if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 91.1500000000 ){
                 if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 1.97500000000 ){
                   if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 1.96500000000 ){
                     nodeid <- 1616
                     predclass <- "0"
                     posterior <- c( 0.99995E+00, 0.48684E-04)
                   } else {
                     nodeid <- 1617
                     predclass <- "1"
                     posterior <- c( 0.22559E-03, 0.99977E+00)
                   }
                 } else {
                   nodeid <- 809
                   predclass <- "0"
                   posterior <- c( 0.99995E+00, 0.48684E-04)
                 }
               } else {
                 if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 3.05000000000 ){
                   if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 194.400000000 ){
                     nodeid <- 1620
                     predclass <- "1"
                     posterior <- c( 0.22559E-03, 0.99977E+00)
                   } else {
                     nodeid <- 1621
                     predclass <- "0"
                     posterior <- c( 0.99995E+00, 0.48684E-04)
                   }
                 } else {
                   nodeid <- 811
                   predclass <- "0"
                   posterior <- c( 0.99995E+00, 0.48684E-04)
                 }
               }
             } else {
               if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.930000000000 ){
                 if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.825000000000 ){
                   nodeid <- 812
                   predclass <- "0"
                   posterior <- c( 0.99995E+00, 0.48684E-04)
                 } else {
                   nodeid <- 813
                   predclass <- "1"
                   posterior <- c( 0.22559E-03, 0.99977E+00)
                 }
               } else {
                 nodeid <- 407
                 predclass <- "0"
                 posterior <- c( 0.99995E+00, 0.48684E-04)
               }
             }
           }
         } else {
           if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 99.4800000000 ){
             nodeid <- 102
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48684E-04)
           } else {
             nodeid <- 103
             predclass <- "1"
             posterior <- c( 0.22559E-03, 0.99977E+00)
           }
         }
       }
     } else {
       catvalues <- c("U_CLL")
       if(TUMOR_MOLECULAR_SUBTYPE %in% catvalues){
         catvalues <- c("NA")
         catvalues <- c(catvalues,NA)
         if(is.na(U1_STATUS) | U1_STATUS %in% catvalues){
           if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 1.50000000000 ){
             if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 2.01000000000 ){
               if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 73.6000000000 ){
                 nodeid <- 416
                 predclass <- "0"
                 posterior <- c( 0.99995E+00, 0.48684E-04)
               } else {
                 nodeid <- 417
                 predclass <- "1"
                 posterior <- c( 0.38462E+00, 0.61538E+00)
               }
             } else {
               nodeid <- 209
               predclass <- "1"
               posterior <- c( 0.22559E-03, 0.99977E+00)
             }
           } else {
             nodeid <- 105
             predclass <- "1"
             posterior <- c( 0.22559E-03, 0.99977E+00)
           }
         } else {
           if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 2.01500000000 ){
             catvalues <- c("Chemo+noAb","NA")
             catvalues <- c(catvalues,NA)
             if(is.na(TREATMENT_AFTER_SAMPLING) | TREATMENT_AFTER_SAMPLING %in% catvalues){
               catvalues <- c("EC_i","EC_m4","EC_o")
               if(EXPRESSION_CLUSTER %in% catvalues){
                 nodeid <- 424
                 predclass <- "0"
                 posterior <- c( 0.99995E+00, 0.48684E-04)
               } else {
                 if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 1.99500000000 ){
                   catvalues <- c("EC_m3","EC_u1")
                   if(EXPRESSION_CLUSTER %in% catvalues){
                     if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 132.850000000 ){
                       if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 77.5000000000 ){
                         catvalues <- c("MUT")
                         if(U1_STATUS %in% catvalues){
                           nodeid <- 13600
                           predclass <- "1"
                           posterior <- c( 0.22559E-03, 0.99977E+00)
                         } else {
                           nodeid <- 13601
                           predclass <- "0"
                           posterior <- c( 0.99995E+00, 0.48684E-04)
                         }
                       } else {
                         nodeid <- 6801
                         predclass <- "1"
                         posterior <- c( 0.22559E-03, 0.99977E+00)
                       }
                     } else {
                       nodeid <- 3401
                       predclass <- "0"
                       posterior <- c( 0.99995E+00, 0.48684E-04)
                     }
                   } else {
                     if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 98.8700000000 ){
                       nodeid <- 3402
                       predclass <- "1"
                       posterior <- c( 0.22559E-03, 0.99977E+00)
                     } else {
                       if(!is.na(NORMAL_MEAN_COVERAGE) & !is.na(TUMOR_SAMPLE_PURITY) &  -13.1308588604  * TUMOR_SAMPLE_PURITY + NORMAL_MEAN_COVERAGE <= 78.8675101490 ){
                         nodeid <- 6806
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         catvalues <- c("No")
                         if(EC_DISCOVERY %in% catvalues){
                           nodeid <- 13614
                           predclass <- "1"
                           posterior <- c( 0.22559E-03, 0.99977E+00)
                         } else {
                           nodeid <- 13615
                           predclass <- "0"
                           posterior <- c( 0.99995E+00, 0.48684E-04)
                         }
                       }
                     }
                   }
                 } else {
                   catvalues <- c("EC_m1","EC_m2")
                   if(EXPRESSION_CLUSTER %in% catvalues){
                     nodeid <- 1702
                     predclass <- "1"
                     posterior <- c( 0.22559E-03, 0.99977E+00)
                   } else {
                     nodeid <- 1703
                     predclass <- "0"
                     posterior <- c( 0.55435E+00, 0.44565E+00)
                   }
                 }
               }
             } else {
               catvalues <- c("Yes")
               if(IGLV3_21_R110 %in% catvalues){
                 nodeid <- 426
                 predclass <- "1"
                 posterior <- c( 0.22559E-03, 0.99977E+00)
               } else {
                 if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 2.00500000000 ){
                   nodeid <- 854
                   predclass <- "0"
                   posterior <- c( 0.98020E+00, 0.19802E-01)
                 } else {
                   if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 83.4500000000 ){
                     nodeid <- 1710
                     predclass <- "0"
                     posterior <- c( 0.99995E+00, 0.48684E-04)
                   } else {
                     nodeid <- 1711
                     predclass <- "1"
                     posterior <- c( 0.22559E-03, 0.99977E+00)
                   }
                 }
               }
             }
           } else {
             if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 99.4800000000 ){
               nodeid <- 214
               predclass <- "1"
               posterior <- c( 0.22559E-03, 0.99977E+00)
             } else {
               nodeid <- 215
               predclass <- "0"
               posterior <- c( 0.94545E+00, 0.54545E-01)
             }
           }
         }
       } else {
         catvalues <- c("Yes")
         if(EC_DISCOVERY %in% catvalues){
           if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 108.200000000 ){
             if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 1.41666666650 ){
               if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 88.1500000000 ){
                 if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.965000000000 ){
                   catvalues <- c("LEN+Ab")
                   if(TREATMENT_AFTER_SAMPLING %in% catvalues){
                     nodeid <- 1728
                     predclass <- "0"
                     posterior <- c( 0.99995E+00, 0.48684E-04)
                   } else {
                     if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 1.25000000000 ){
                       if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 55.8500000000 ){
                         nodeid <- 6916
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         catvalues <- c("NA")
                         catvalues <- c(catvalues,NA)
                         if(is.na(TREATMENT_AFTER_SAMPLING) | TREATMENT_AFTER_SAMPLING %in% catvalues){
                           nodeid <- 13834
                           predclass <- "0"
                           posterior <- c( 0.99995E+00, 0.48684E-04)
                         } else {
                           if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 0.616666666500 ){
                             nodeid <- 27670
                             predclass <- "1"
                             posterior <- c( 0.22559E-03, 0.99977E+00)
                           } else {
                             if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.955000000000 ){
                               nodeid <- 55342
                               predclass <- "0"
                               posterior <- c( 0.99995E+00, 0.48684E-04)
                             } else {
                               nodeid <- 55343
                               predclass <- "1"
                               posterior <- c( 0.33333E+00, 0.66667E+00)
                             }
                           }
                         }
                       }
                     } else {
                       if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.795000000000 ){
                         nodeid <- 6918
                         predclass <- "1"
                         posterior <- c( 0.22559E-03, 0.99977E+00)
                       } else {
                         catvalues <- c("NA")
                         catvalues <- c(catvalues,NA)
                         if(is.na(TREATMENT_AFTER_SAMPLING) | TREATMENT_AFTER_SAMPLING %in% catvalues){
                           nodeid <- 13838
                           predclass <- "1"
                           posterior <- c( 0.22559E-03, 0.99977E+00)
                         } else {
                           nodeid <- 13839
                           predclass <- "0"
                           posterior <- c( 0.99995E+00, 0.48684E-04)
                         }
                       }
                     }
                   }
                 } else {
                   if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 1.99500000000 ){
                     if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 0.650000000000 ){
                       catvalues <- c("NA")
                       catvalues <- c(catvalues,NA)
                       if(is.na(TREATMENT_AFTER_SAMPLING) | TREATMENT_AFTER_SAMPLING %in% catvalues){
                         nodeid <- 6920
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         nodeid <- 6921
                         predclass <- "1"
                         posterior <- c( 0.22559E-03, 0.99977E+00)
                       }
                     } else {
                       nodeid <- 3461
                       predclass <- "0"
                       posterior <- c( 0.99995E+00, 0.48684E-04)
                     }
                   } else {
                     nodeid <- 1731
                     predclass <- "0"
                     posterior <- c( 0.99995E+00, 0.48684E-04)
                   }
                 }
               } else {
                 if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 70.5000000000 ){
                   nodeid <- 866
                   predclass <- "0"
                   posterior <- c( 0.99995E+00, 0.48684E-04)
                 } else {
                   nodeid <- 867
                   predclass <- "1"
                   posterior <- c( 0.39216E+00, 0.60784E+00)
                 }
               }
             } else {
               catvalues <- c("Chemo+noAb","NA")
               catvalues <- c(catvalues,NA)
               if(is.na(TREATMENT_AFTER_SAMPLING) | TREATMENT_AFTER_SAMPLING %in% catvalues){
                 if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 1.99500000000 ){
                   nodeid <- 868
                   predclass <- "1"
                   posterior <- c( 0.22559E-03, 0.99977E+00)
                 } else {
                   if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 52.9000000000 ){
                     nodeid <- 1738
                     predclass <- "1"
                     posterior <- c( 0.22559E-03, 0.99977E+00)
                   } else {
                     catvalues <- c("EC_u1")
                     if(EXPRESSION_CLUSTER %in% catvalues){
                       nodeid <- 3478
                       predclass <- "0"
                       posterior <- c( 0.99995E+00, 0.48684E-04)
                     } else {
                       nodeid <- 3479
                       predclass <- "1"
                       posterior <- c( 0.22559E-03, 0.99977E+00)
                     }
                   }
                 }
               } else {
                 nodeid <- 435
                 predclass <- "0"
                 posterior <- c( 0.99995E+00, 0.48684E-04)
               }
             }
           } else {
             nodeid <- 109
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48684E-04)
           }
         } else {
           if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 2.00500000000 ){
             catvalues <- c("No")
             if(IGLV3_21_R110 %in% catvalues){
               nodeid <- 220
               predclass <- "0"
               posterior <- c( 0.90286E+00, 0.97143E-01)
             } else {
               if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 99.1400000000 ){
                 nodeid <- 442
                 predclass <- "1"
                 posterior <- c( 0.22559E-03, 0.99977E+00)
               } else {
                 nodeid <- 443
                 predclass <- "0"
                 posterior <- c( 0.92683E+00, 0.73171E-01)
               }
             }
           } else {
             nodeid <- 111
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48684E-04)
           }
         }
       }
     }
   } else {
     if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 1.38333333350 ){
       catvalues <- c("Chemo+Ab")
       if(PRIOR_TREATMENT_CATEGORY %in% catvalues){
         nodeid <- 28
         predclass <- "1"
         posterior <- c( 0.22559E-03, 0.99977E+00)
       } else {
         catvalues <- c("EC_i","EC_m2","EC_u2")
         if(EXPRESSION_CLUSTER %in% catvalues){
           if(is.na(IGHV_IDENTITY_PERCENTAGE) | IGHV_IDENTITY_PERCENTAGE <= 97.7700000000 ){
             if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.945000000000 ){
               nodeid <- 232
               predclass <- "0"
               posterior <- c( 0.99995E+00, 0.48684E-04)
             } else {
               if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 31.6500000000 ){
                 nodeid <- 466
                 predclass <- "1"
                 posterior <- c( 0.37500E+00, 0.62500E+00)
               } else {
                 if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 63.8500000000 ){
                   nodeid <- 934
                   predclass <- "0"
                   posterior <- c( 0.99995E+00, 0.48684E-04)
                 } else {
                   if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 69.4500000000 ){
                     if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 0.700000000000 ){
                       nodeid <- 3740
                       predclass <- "0"
                       posterior <- c( 0.99995E+00, 0.48684E-04)
                     } else {
                       nodeid <- 3741
                       predclass <- "1"
                       posterior <- c( 0.22559E-03, 0.99977E+00)
                     }
                   } else {
                     if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 85.3500000000 ){
                       if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 81.7500000000 ){
                         nodeid <- 7484
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         nodeid <- 7485
                         predclass <- "1"
                         posterior <- c( 0.22559E-03, 0.99977E+00)
                       }
                     } else {
                       nodeid <- 3743
                       predclass <- "0"
                       posterior <- c( 0.99995E+00, 0.48684E-04)
                     }
                   }
                 }
               }
             }
           } else {
             if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 97.9300000000 ){
               nodeid <- 234
               predclass <- "1"
               posterior <- c( 0.22559E-03, 0.99977E+00)
             } else {
               nodeid <- 235
               predclass <- "0"
               posterior <- c( 0.99995E+00, 0.48684E-04)
             }
           }
         } else {
           catvalues <- c("Yes")
           if(IGLV3_21_R110 %in% catvalues){
             if(!is.na(TUMOR_SAMPLE_PLOIDY) & TUMOR_SAMPLE_PLOIDY <= 2.01500000000 ){
               catvalues <- c("NA")
               catvalues <- c(catvalues,NA)
               if(is.na(TUMOR_MOLECULAR_SUBTYPE) | TUMOR_MOLECULAR_SUBTYPE %in% catvalues){
                 if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.995000000000 ){
                   nodeid <- 944
                   predclass <- "0"
                   posterior <- c( 0.99995E+00, 0.48684E-04)
                 } else {
                   nodeid <- 945
                   predclass <- "1"
                   posterior <- c( 0.22559E-03, 0.99977E+00)
                 }
               } else {
                 nodeid <- 473
                 predclass <- "0"
                 posterior <- c( 0.99995E+00, 0.48684E-04)
               }
             } else {
               nodeid <- 237
               predclass <- "1"
               posterior <- c( 0.22559E-03, 0.99977E+00)
             }
           } else {
             if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.995000000000 ){
               if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 0.983333333500 ){
                 if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 0.516666666500 ){
                   nodeid <- 952
                   predclass <- "0"
                   posterior <- c( 0.97810E+00, 0.21898E-01)
                 } else {
                   catvalues <- c("EC_m1","EC_m4","EC_o")
                   if(EXPRESSION_CLUSTER %in% catvalues){
                     if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 38.5000000000 ){
                       nodeid <- 3812
                       predclass <- "0"
                       posterior <- c( 0.50000E+00, 0.50000E+00)
                     } else {
                       if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 79.8000000000 ){
                         nodeid <- 7626
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.975000000000 ){
                           catvalues <- c("M_CLL")
                           if(TUMOR_MOLECULAR_SUBTYPE %in% catvalues){
                             nodeid <- 30508
                             predclass <- "0"
                             posterior <- c( 0.99995E+00, 0.48684E-04)
                           } else {
                             if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.910000000000 ){
                               nodeid <- 61018
                               predclass <- "0"
                               posterior <- c( 0.99995E+00, 0.48684E-04)
                             } else {
                               nodeid <- 61019
                               predclass <- "1"
                               posterior <- c( 0.22559E-03, 0.99977E+00)
                             }
                           }
                         } else {
                           nodeid <- 15255
                           predclass <- "0"
                           posterior <- c( 0.99995E+00, 0.48684E-04)
                         }
                       }
                     }
                   } else {
                     if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 69.7500000000 ){
                       catvalues <- c("U_CLL")
                       if(TUMOR_MOLECULAR_SUBTYPE %in% catvalues){
                         nodeid <- 7628
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 97.9500000000 ){
                           catvalues <- c("EC_m3")
                           if(EXPRESSION_CLUSTER %in% catvalues){
                             nodeid <- 30516
                             predclass <- "1"
                             posterior <- c( 0.41667E+00, 0.58333E+00)
                           } else {
                             nodeid <- 30517
                             predclass <- "0"
                             posterior <- c( 0.87500E+00, 0.12500E+00)
                           }
                         } else {
                           nodeid <- 15259
                           predclass <- "1"
                           posterior <- c( 0.22559E-03, 0.99977E+00)
                         }
                       }
                     } else {
                       if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.985000000000 ){
                         nodeid <- 7630
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         catvalues <- c("No")
                         if(EC_DISCOVERY %in% catvalues){
                           nodeid <- 15262
                           predclass <- "1"
                           posterior <- c( 0.22559E-03, 0.99977E+00)
                         } else {
                           nodeid <- 15263
                           predclass <- "0"
                           posterior <- c( 0.99995E+00, 0.48684E-04)
                         }
                       }
                     }
                   }
                 }
               } else {
                 if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.955000000000 ){
                   if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 59.9500000000 ){
                     if(!is.na(TMB_NONSYNONYMOUS) & TMB_NONSYNONYMOUS <= 1.01666666650 ){
                       nodeid <- 3816
                       predclass <- "1"
                       posterior <- c( 0.22559E-03, 0.99977E+00)
                     } else {
                       nodeid <- 3817
                       predclass <- "0"
                       posterior <- c( 0.99995E+00, 0.48684E-04)
                     }
                   } else {
                     if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 165.300000000 ){
                       if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 97.1500000000 ){
                         nodeid <- 7636
                         predclass <- "0"
                         posterior <- c( 0.99995E+00, 0.48684E-04)
                       } else {
                         if(!is.na(NORMAL_MEAN_COVERAGE) & NORMAL_MEAN_COVERAGE <= 102.950000000 ){
                           nodeid <- 15274
                           predclass <- "1"
                           posterior <- c( 0.22559E-03, 0.99977E+00)
                         } else {
                           nodeid <- 15275
                           predclass <- "0"
                           posterior <- c( 0.99995E+00, 0.48684E-04)
                         }
                       }
                     } else {
                       nodeid <- 3819
                       predclass <- "1"
                       posterior <- c( 0.22559E-03, 0.99977E+00)
                     }
                   }
                 } else {
                   nodeid <- 955
                   predclass <- "0"
                   posterior <- c( 0.94118E+00, 0.58824E-01)
                 }
               }
             } else {
               nodeid <- 239
               predclass <- "0"
               posterior <- c( 0.98297E+00, 0.17032E-01)
             }
           }
         }
       }
     } else {
       if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 52.1500000000 ){
         if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 90.2250000000 ){
           nodeid <- 60
           predclass <- "1"
           posterior <- c( 0.22559E-03, 0.99977E+00)
         } else {
           catvalues <- c("Yes")
           if(IGLV3_21_R110 %in% catvalues){
             nodeid <- 122
             predclass <- "1"
             posterior <- c( 0.22559E-03, 0.99977E+00)
           } else {
             if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 94.0650000000 ){
               catvalues <- c("i_CLL")
               if(CLL_EPITYPE %in% catvalues){
                 nodeid <- 492
                 predclass <- "1"
                 posterior <- c( 0.22559E-03, 0.99977E+00)
               } else {
                 nodeid <- 493
                 predclass <- "0"
                 posterior <- c( 0.90323E+00, 0.96774E-01)
               }
             } else {
               nodeid <- 247
               predclass <- "0"
               posterior <- c( 0.99995E+00, 0.48684E-04)
             }
           }
         }
       } else {
         catvalues <- c("Yes")
         if(EC_EXTENSION %in% catvalues){
           catvalues <- c("Yes")
           if(M_CLL_WES %in% catvalues){
             nodeid <- 124
             predclass <- "1"
             posterior <- c( 0.22559E-03, 0.99977E+00)
           } else {
             nodeid <- 125
             predclass <- "0"
             posterior <- c( 0.99995E+00, 0.48684E-04)
           }
         } else {
           catvalues <- c("n_CLL")
           if(CLL_EPITYPE %in% catvalues){
             catvalues <- c("NA")
             catvalues <- c(catvalues,NA)
             if(is.na(U1_STATUS) | U1_STATUS %in% catvalues){
               nodeid <- 252
               predclass <- "1"
               posterior <- c( 0.28571E+00, 0.71429E+00)
             } else {
               catvalues <- c("Chemo+Ab")
               if(TREATMENT_AFTER_SAMPLING %in% catvalues){
                 nodeid <- 506
                 predclass <- "0"
                 posterior <- c( 0.83333E+00, 0.16667E+00)
               } else {
                 catvalues <- c("EC_u2")
                 if(EXPRESSION_CLUSTER %in% catvalues){
                   nodeid <- 1014
                   predclass <- "1"
                   posterior <- c( 0.25000E+00, 0.75000E+00)
                 } else {
                   nodeid <- 1015
                   predclass <- "0"
                   posterior <- c( 0.80000E+00, 0.20000E+00)
                 }
               }
             }
           } else {
             catvalues <- c("No")
             if(IGLV3_21_R110 %in% catvalues){
               catvalues <- c("NA")
               catvalues <- c(catvalues,NA)
               if(is.na(U1_STATUS) | U1_STATUS %in% catvalues){
                 if(!is.na(TUMOR_MEAN_COVERAGE) & TUMOR_MEAN_COVERAGE <= 73.2000000000 ){
                   nodeid <- 1016
                   predclass <- "0"
                   posterior <- c( 0.99995E+00, 0.48684E-04)
                 } else {
                   if(!is.na(TUMOR_SAMPLE_PURITY) & TUMOR_SAMPLE_PURITY <= 0.995000000000 ){
                     catvalues <- c("Female")
                     if(SEX %in% catvalues){
                       nodeid <- 4068
                       predclass <- "1"
                       posterior <- c( 0.22559E-03, 0.99977E+00)
                     } else {
                       nodeid <- 4069
                       predclass <- "0"
                       posterior <- c( 0.99995E+00, 0.48684E-04)
                     }
                   } else {
                     nodeid <- 2035
                     predclass <- "0"
                     posterior <- c( 0.94737E+00, 0.52632E-01)
                   }
                 }
               } else {
                 nodeid <- 509
                 predclass <- "0"
                 posterior <- c( 0.98582E+00, 0.14184E-01)
               }
             } else {
               if(!is.na(IGHV_IDENTITY_PERCENTAGE) & IGHV_IDENTITY_PERCENTAGE <= 93.5750000000 ){
                 nodeid <- 510
                 predclass <- "0"
                 posterior <- c( 0.99995E+00, 0.48684E-04)
               } else {
                 catvalues <- c("No")
                 if(EC_DISCOVERY %in% catvalues){
                   nodeid <- 1022
                   predclass <- "1"
                   posterior <- c( 0.22559E-03, 0.99977E+00)
                 } else {
                   nodeid <- 1023
                   predclass <- "0"
                   posterior <- c( 0.87500E+00, 0.12500E+00)
                 }
               }
             }
           }
         }
       }
     }
   }
 }
 return(c(nodeid,predclass,posterior))
}
## end of function
##
##
## If desired, replace "data_patient_cna.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("data_patient_cna.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred.class contains predicted class
## prob contains predicted posterior probabilities
node <- NULL
pred.class <- NULL
prob <- NULL
for(i in 1:nrow(newdata)){
    NORMAL_MEAN_COVERAGE <- as.numeric(newdata$NORMAL_MEAN_COVERAGE[i])
    TUMOR_MEAN_COVERAGE <- as.numeric(newdata$TUMOR_MEAN_COVERAGE[i])
    TUMOR_SAMPLE_PURITY <- as.numeric(newdata$TUMOR_SAMPLE_PURITY[i])
    TUMOR_SAMPLE_PLOIDY <- as.numeric(newdata$TUMOR_SAMPLE_PLOIDY[i])
    CLL_EPITYPE <- as.character(newdata$CLL_EPITYPE[i])
    TUMOR_MOLECULAR_SUBTYPE <- as.character(newdata$TUMOR_MOLECULAR_SUBTYPE[i])
    M_CLL_WES <- as.character(newdata$M_CLL_WES[i])
    U_CLL_WES <- as.character(newdata$U_CLL_WES[i])
    EC_DISCOVERY <- as.character(newdata$EC_DISCOVERY[i])
    EC_EXTENSION <- as.character(newdata$EC_EXTENSION[i])
    IGLV3_21_R110 <- as.character(newdata$IGLV3_21_R110[i])
    U1_STATUS <- as.character(newdata$U1_STATUS[i])
    TMB_NONSYNONYMOUS <- as.numeric(newdata$TMB_NONSYNONYMOUS[i])
    SEX <- as.character(newdata$SEX[i])
    IGHV_IDENTITY_PERCENTAGE <- as.numeric(newdata$IGHV_IDENTITY_PERCENTAGE[i])
    PRIOR_TREATMENT_CATEGORY <- as.character(newdata$PRIOR_TREATMENT_CATEGORY[i])
    TREATMENT_AFTER_SAMPLING <- as.character(newdata$TREATMENT_AFTER_SAMPLING[i])
    EXPRESSION_CLUSTER <- as.character(newdata$EXPRESSION_CLUSTER[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
    prob <- rbind(prob,as.numeric(tmp[-c(1,2)]))
}
