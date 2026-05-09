library(rpart)
library(ranger)
library(randomForest)
library(party)

z <- read.table("realdata.txt",header=TRUE)
y <- z$y
n <- nrow(z)
nsim <- 10

ct.err <- 0
ct.err2 <- 0
rp.err <- 0
rp.err2 <- 0
rf.err <- 0
rf.err2 <- 0
ran.err <- 0
ran.err2 <- 0
gcon.err <- 0
gcon.err2 <- 0
gstep.err <- 0
gstep.err2 <- 0
for(i in 1:nsim){
    ran <- rbinom(n,1,0.5)
    train <- ran == 1
    test <- ran == 0
### ctree
    ct <- ctree(y ~ .,data=z[train,])
    ct.pred <- predict(ct,newdata=z[test,])
    error <- mean((y[test]-ct.pred)^2)
    ct.err <- ct.err+error
    ct.err2 <- ct.err2+error*error
### rpart
    rp <- rpart(y ~ .,data=z[train,], method="anova")
    rp.pred <- predict(rp,newdata=z[test,])
    error <- mean((y[test]-rp.pred)^2)
    rp.err <- rp.err+error
    rp.err2 <- rp.err2+error*error
### randomforest
    rf <- randomForest(y ~ .,data=z[train,], method="anova")
    rf.pred <- predict(rf,newdata=z[test,])
    error <- mean((y[test]-rf.pred)^2)
    rf.err <- rf.err+error
    rf.err2 <- rf.err2+error*error
### ranger
    ran <- ranger(y ~ .,data=z[train,])
    ran.pred <- predict(ran,data=z[test,])$predictions
    error <- mean((y[test]-ran.pred)^2)
    ran.err <- ran.err+error
    ran.err2 <- ran.err2+error*error
### guide constant tree
    z0 <- z
    z0$y[train] <- NA
    write.table(z0,"simdata.txt",row.names=FALSE,col.names=TRUE)
    shell("..\\guide < simcons.in > simcons.log")
    z1 <- read.table("simcons.fit",header=TRUE)
    gcon.pred <- z1$predicted[test]
    error <- mean((y[test]-gcon.pred)^2)
    gcon.err <- gcon.err+error
    gcon.err2 <- gcon.err2+error*error
### guide stepwise linear tree
    shell("..\\guide < simstep.in > simstep.log")
    z1 <- read.table("simstep.fit",header=TRUE)
    gstep.pred <- z1$predicted[test]
    error <- mean((y[test]-gstep.pred)^2)
    gstep.err <- gstep.err+error
    gstep.err2 <- gstep.err2+error*error
###
    if(i == nsim | i==5*floor(i/5)){
        print(paste("Number of iterations completed =",i),quote=FALSE)
        ct.mean <- ct.err/i
        ct.se <- sqrt((ct.err2-i*ct.mean^2)/(i*(i-1)))
        print(paste("ctree MSE and SE =",ct.mean,ct.se),quote=FALSE)
        rp.mean <- rp.err/i
        rp.se <- sqrt((rp.err2-i*rp.mean^2)/(i*(i-1)))
        print(paste("rpart MSE and SE =",rp.mean,rp.se),quote=FALSE)
        rf.mean <- rf.err/i
        rf.se <- sqrt((rf.err2-i*rf.mean^2)/(i*(i-1)))
        print(paste("randomForest MSE and SE =",rf.mean,rf.se),quote=FALSE)
        ran.mean <- ran.err/i
        ran.se <- sqrt((ran.err2-i*ran.mean^2)/(i*(i-1)))
        print(paste("ranger MSE and SE =",ran.mean,ran.se),quote=FALSE)
        gcon.mean <- gcon.err/i
        gcon.se <- sqrt((gcon.err2-i*gcon.mean^2)/(i*(i-1)))
        print(paste("Guide constant MSE and SE =",gcon.mean,gcon.se),quote=FALSE)
        gstep.mean <- gstep.err/i
        gstep.se <- sqrt((gstep.err2-i*gstep.mean^2)/(i*(i-1)))
        print(paste("Guide stepwise MSE and SE =",gstep.mean,gstep.se),quote=FALSE)
    }
}
