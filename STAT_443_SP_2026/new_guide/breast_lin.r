## Function for predicting least-squares mean of Class
## Code produced by GUIDE 46.2 on 4/30/26 at 11:32
guide_predict <- function(){
 lincomb <- 0
 x <- ifelse(is.na(Cl.thickness), 4.44216691069, Cl.thickness)
 lincomb <- lincomb +  0.314243078284E-01 * x
 x <- ifelse(is.na(Cell.size), 3.15080527086, Cell.size)
 lincomb <- lincomb +  0.276388882458E-01 * x
 x <- ifelse(is.na(Cell.shape), 3.21522693997, Cell.shape)
 lincomb <- lincomb +  0.169273803986E-01 * x
 x <- ifelse(is.na(Bare.nuclei), 3.54465592972, Bare.nuclei)
 lincomb <- lincomb +  0.476315327719E-01 * x
 x <- ifelse(is.na(Bl.cromatin), 3.44509516837, Bl.cromatin)
 lincomb <- lincomb +  0.211857272903E-01 * x
 x <- ifelse(is.na(Normal.nucleoli), 2.86969253294, Normal.nucleoli)
 lincomb <- lincomb +  0.204631372475E-01 * x
 if(!is.na(lincomb) & lincomb <= 0.325059429548 ){
   nodeid <- 2
   predict <- 0.00000000000
   if(is.na(predict)) predict <-  0.00000000000
 } else {
   lincomb <- 0
   x <- ifelse(is.na(Cl.thickness), 6.34293948127, Cl.thickness)
   lincomb <- lincomb +  0.387048718768E-01 * x
   x <- ifelse(is.na(Cell.size), 5.15561959654, Cell.size)
   lincomb <- lincomb +  0.255764465800E-01 * x
   x <- ifelse(is.na(Cell.shape), 5.18731988473, Cell.shape)
   lincomb <- lincomb +  0.151898785034E-01 * x
   x <- ifelse(is.na(Bare.nuclei), 5.95389048991, Bare.nuclei)
   lincomb <- lincomb +  0.455180858118E-01 * x
   x <- ifelse(is.na(Bl.cromatin), 4.98270893372, Bl.cromatin)
   lincomb <- lincomb +  0.215236969188E-01 * x
   x <- ifelse(is.na(Normal.nucleoli), 4.62247838617, Normal.nucleoli)
   lincomb <- lincomb +  0.196234884557E-01 * x
   if(!is.na(lincomb) & lincomb <= 0.448098899615 ){
     nodeid <- 6
     predict <- 0.00000000000
     if(is.na(predict)) predict <-  0.00000000000
   } else {
     lincomb <- 0
     x <- ifelse(is.na(Cl.thickness), 6.94776119403, Cl.thickness)
     lincomb <- lincomb +  0.256758309591E-01 * x
     x <- ifelse(is.na(Cell.shape), 6.19776119403, Cell.shape)
     lincomb <- lincomb +  0.258645810707E-01 * x
     x <- ifelse(is.na(Bare.nuclei), 7.22388059701, Bare.nuclei)
     lincomb <- lincomb +  0.244496930588E-01 * x
     x <- ifelse(is.na(Bl.cromatin), 5.71268656716, Bl.cromatin)
     lincomb <- lincomb +  0.207792210240E-01 * x
     if(!is.na(lincomb) & lincomb <= 0.428039355856 ){
       nodeid <- 14
       predict <- 0.273175100761E-01
       x <- ifelse(is.na(Cell.size), 3.20000000000, Cell.size)
       predict <- predict +  0.125391849530 * x
       predict <- min(max(predict, 0.00000000000),1.00000000000)
       if(is.na(predict)) predict <-  0.428571428571
     } else {
       nodeid <- 15
       predict <- 0.961373390558
       if(is.na(predict)) predict <-  0.961373390558
     }
   }
 }
 return(c(nodeid,predict))
}
## end of function
##
##
## If desired, replace "breast.txt" with name of file containing new data
## New file must have at least the same variables with same names
## (but not necessarily the same order) as in the training data file
## Missing value code is converted to NA if not already NA
newdata <- read.table("breast.txt",header=TRUE,colClasses="character")
## node contains terminal node ID of each case
## pred contains predicted value of each case
node <- NULL
pred <- NULL
for(i in 1:nrow(newdata)){
    Cl.thickness <- as.numeric(newdata$Cl.thickness[i])
    Cell.size <- as.numeric(newdata$Cell.size[i])
    Cell.shape <- as.numeric(newdata$Cell.shape[i])
    Bare.nuclei <- as.numeric(newdata$Bare.nuclei[i])
    Bl.cromatin <- as.numeric(newdata$Bl.cromatin[i])
    Normal.nucleoli <- as.numeric(newdata$Normal.nucleoli[i])
    tmp <- guide_predict()
    node <- c(node,as.numeric(tmp[1]))
    pred <- c(pred,tmp[2])
}
