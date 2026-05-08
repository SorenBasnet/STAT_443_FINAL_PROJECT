## Function for predicting class posterior probabilities of Gender
## Code produced by GUIDE 46.2 on 2/12/26 at 9:26
find_crim <- function(x,xbar,crimdim,qdafit,coefs,gm,gv,gcor,priors,majorc){
   ## gv and gcor cannot have NA values
   x <- ifelse(is.na(x),xbar,x) # impute missing with means
   crim <- rep(0,crimdim)
   for(j in 1:crimdim){
       crim[j] <- sum(x*coefs[,j])
   }
   nc <- nrow(gm)  # number of classes
   mindis <- Inf
   pred <- majorc
   if(crimdim == 1){  # use only crim1
       for(j in 1:nc){
           if(priors[j] > 0 & gv[j,1] > 0 & !is.na(gm[j,1])){
               dis <- (crim[1]-gm[j,1])^2/gv[j,1]-2*log(priors[j])
               if(dis < mindis){
                   mindis <- dis
                   pred <- j
               }
           }
       }
   } else if(crimdim > 1){
       if(qdafit){ # qda fits with 2 crimcoords
           for(j in 1:nc){
               if(priors[j] > 0){
                   if(gv[j,1] > 0 & gv[j,2] > 0 & !is.na(gm[j,1]) & !is.na(gm[j,2])){
                       sdx <- sqrt(gv[j,1])
                       sdy <- sqrt(gv[j,2])
                       stdx <- (crim[1]-gm[j,1])/sdx
                       stdy <- (crim[2]-gm[j,2])/sdy
                       tmp <- 1-gcor[j]*gcor[j]
                       dis <- stdx*stdx+stdy*stdy-2*gcor[j]*stdx*stdy
                       dis <- dis/tmp+2*log(sdx*sdy*sqrt(tmp))-2*log(priors[j])
                       if(dis < mindis){
                           mindis <- dis
                           pred <- j
                       }
                   }
               }
           }
       } else {  # use lda with equal variances and no correlations
           for(j in 1:nc){
               if(priors[j] > 0){
                   dis <- -2*log(priors[j])
                   for(k in 1:crimdim){
                       if(gv[j,k] > 0 & !is.na(gm[j,k])){
                           dis <- dis+(crim[k]-gm[j,k])^2/gv[j,k]
                       }
                   }
                   if(dis < mindis){
                       mindis <- dis
                       pred <- j
                   }
               }
           }
       }
   }
   return(c(pred,crim))
}
guide_predict <- function(){
 classnames <- c("F","M")
 nodeid <- 1
 majorc <- 1
 xv <- c(Biacromialdiam,Bitrochantericdiam,Chestdepth,Chestdiam,Elbowdiam,Wristdiam,Kneediam, Anklediam, Shouldergirth, Chestgirth, Waistgirth, Navelgirth, Hipgirth, Bicepgirth, Forearmgirth, Kneegirth, Calfmaxgirth, Anklemingirth, Wristmingirth, Age, Weight, Height)
 xbar <- c(38.8114398422,31.9804733728,19.2260355030,27.9737672584,
     13.3852071006,10.5426035503,18.8106508876,13.8633136095,
     108.195069034,93.3335305720,76.9794871795,85.6538461538,
     96.6808678501,31.1696252465,25.9429980276,36.2029585799,
     36.0783037475,22.1573964497,16.0974358974,30.1814595661,
     69.1475345168,171.143786982)
 xv <- ifelse(is.na(xv),xbar,xv)
 coefs1 <- c(-0.231187567668E-02,0.787972705781E-03,-0.101778089653E-02,
     0.542619096231E-03,-0.377488274666E-02,0.496223727515E-03,
     -0.122854270141E-02,-0.401747274267E-02,-0.584687552419E-03,
     0.360608052726E-03,-0.292986393130E-02,0.125617734108E-02,
     0.142506490809E-02,-0.164049599391E-02,-0.470091011889E-02,
     0.915183347469E-03,0.769059931116E-03,-0.391215150091E-03,
     0.282727945793E-03,0.243894300376E-04,0.144043893667E-02,
     -0.107427460460E-02)
 crimcoord1 <- sum(xv*coefs1)
 if(crimcoord1 <= -0.389180887552){
   nodeid <- 2
   majorc <- 2
   xv <- c(Elbowdiam,Wristdiam,Anklediam,Hipgirth,Thighgirth,Height)
   xbar <- c(14.4137254902,11.2231372549,14.6898039216,97.9694117647,
       56.6568627451,177.405098039)
   xv <- ifelse(is.na(xv),xbar,xv)
   coefs1 <- c(0.219548971325E-01,0.149006074465E-01,0.274763702341E-01,
       -0.558231906154E-02,-0.389556157103E-02,0.269440245031E-02)
   crimcoord1 <- sum(xv*coefs1)
   if(crimcoord1 <= 0.467708837811){
     nodeid <- 4
     majorc <- 1
     xv <- c(Anklediam,Chestgirth)
     xbar <- c(13.4571428571,101.685714286)
     xv <- ifelse(is.na(xv),xbar,xv)
     coefs1 <- c(0.153242839175,0.257218298002E-01)
     crimcoord1 <- sum(xv*coefs1)
     if(crimcoord1 <= 4.69267700340){
       nodeid <- 8
       majorc <- 1
       crimdim <- 0
       predclass <- classnames[majorc]
     } else {
       nodeid <- 9
       majorc <- 2
       crimdim <- 1
       gm <- matrix(0,nrow=2,ncol=crimdim)
       gm[,1] <- c(5.79604906587,4.78787581648)
       gv <- matrix(0,nrow=2,ncol=crimdim)
       gv[,1] <- rep(0.257577198675E-01,2)
       gcor <- rep(0,2)
       priors <- c(0.142857142857,0.857142857143)
       xv <- c(Kneediam,Weight)
       xbar <- c(20.2714285714,91.1285714286)
       coefs <- matrix(0,nrow=2,ncol=crimdim)
       coefs[,1] <- c(0.114202170729,0.287161246877E-01)
       tmp <- find_crim(xv,xbar,crimdim,FALSE,coefs,gm,gv,gcor,priors,majorc)
       predclass <- classnames[tmp[1]]
       crimcoords <- tmp[-1]
     }
   } else {
     nodeid <- 5
     majorc <- 2
     crimdim <- 0
     predclass <- classnames[majorc]
   }
 } else {
   nodeid <- 3
   majorc <- 1
   crimdim <- 0
   predclass <- classnames[majorc]
 }
 return(c(nodeid,predclass))
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
    Chestdepth <- as.numeric(newdata$Chestdepth[i])
    Chestdiam <- as.numeric(newdata$Chestdiam[i])
    Elbowdiam <- as.numeric(newdata$Elbowdiam[i])
    Wristdiam <- as.numeric(newdata$Wristdiam[i])
    Kneediam <- as.numeric(newdata$Kneediam[i])
    Anklediam <- as.numeric(newdata$Anklediam[i])
    Shouldergirth <- as.numeric(newdata$Shouldergirth[i])
    Chestgirth <- as.numeric(newdata$Chestgirth[i])
    Waistgirth <- as.numeric(newdata$Waistgirth[i])
    Navelgirth <- as.numeric(newdata$Navelgirth[i])
    Hipgirth <- as.numeric(newdata$Hipgirth[i])
    Thighgirth <- as.numeric(newdata$Thighgirth[i])
    Bicepgirth <- as.numeric(newdata$Bicepgirth[i])
    Forearmgirth <- as.numeric(newdata$Forearmgirth[i])
    Kneegirth <- as.numeric(newdata$Kneegirth[i])
    Calfmaxgirth <- as.numeric(newdata$Calfmaxgirth[i])
    Anklemingirth <- as.numeric(newdata$Anklemingirth[i])
    Wristmingirth <- as.numeric(newdata$Wristmingirth[i])
    Age <- as.numeric(newdata$Age[i])
    Weight <- as.numeric(newdata$Weight[i])
    Height <- as.numeric(newdata$Height[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred.class <- rbind(pred.class,tmp[2])
}
