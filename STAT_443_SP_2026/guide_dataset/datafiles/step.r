## Function for predicting least-squares mean of BodyfatB
## Code produced by GUIDE 46.2 on 3/17/26 at 12:10
guide_predict <- function(){
 lincomb <- 0
 x <- ifelse(is.na(Weight), 178.924404762, Weight)
 lincomb <- lincomb +  -0.105583232818 * x
 x <- ifelse(is.na(Abdomen), 92.5559523810, Abdomen)
 lincomb <- lincomb +  0.901908258513 * x
 x <- ifelse(is.na(Wrist), 18.2297619048, Wrist)
 lincomb <- lincomb +  -1.14569677450 * x
 if(!is.na(lincomb) & lincomb <= 43.0139551773 ){
   nodeid <- 2
   predict <- -16.6598870210
   x <- ifelse(is.na(Neck), 36.7130081301, Neck)
   predict <- predict +  -0.892365701431 * x
   x <- ifelse(is.na(Abdomen), 84.4317073171, Abdomen)
   predict <- predict +  0.742200065626 * x
   predict <- min(max(predict, 0.00000000000),45.1000000000)
   if(is.na(predict)) predict <-  13.2439024390
 } else {
   nodeid <- 3
   predict <- -10.1038076384
   x <- ifelse(is.na(Weight), 194.657364341, Weight)
   predict <- predict +  -0.111737547146 * x
   x <- ifelse(is.na(Abdomen), 100.302325581, Abdomen)
   predict <- predict +  0.799560087630 * x
   x <- ifelse(is.na(Wrist), 18.5441860465, Wrist)
   predict <- predict +  -1.29286748165 * x
   predict <- min(max(predict, 0.00000000000),45.1000000000)
   if(is.na(predict)) predict <-  24.3682170543
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## If desired, replace "fat.dat" with name of file containing new data
## and change the "names(newdata)" line if necessary
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("fat.dat",header=FALSE,colClasses="character")
names(newdata) <- c("Case","BodyfatB","BodyfatS","Density","Age","Weight","Height",
"Adiposity","FatFreewt","Neck","Chest","Abdomen","Hip","Thigh","Knee",
"Ankle","Biceps","Forearm","Wrist")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    Weight <- as.numeric(newdata$Weight[i])
    Neck <- as.numeric(newdata$Neck[i])
    Abdomen <- as.numeric(newdata$Abdomen[i])
    Wrist <- as.numeric(newdata$Wrist[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}
