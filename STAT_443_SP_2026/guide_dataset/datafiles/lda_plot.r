library(MASS)
descendents <- function(node){
    desc <- NULL
    for(m in tnodes){
        j <- m
        while(j > 1){
            j <- floor(j/2)
            if(j == node) desc <- c(desc,m)
        }
    }
    return(desc)
}
pdf(file='ldaplots.pdf',width=8,height=6)
## postscript(file='ldaplots.ps',horizontal=TRUE)
par(pty='s',mfrow=c(2,4),cex=0.7,cex.main=0.9,cex.lab=0.8,cex.axis=0.8)
eqplot <- FALSE
splits <- c( -0.389181, 0.467709, 4.69268)
varnames <- scan(file='lda_data.txt',what='character',nlines=1)
nvar <- length(varnames)
z <- as.data.frame(matrix(scan(file='lda_data.txt',skip=1),ncol=nvar,byrow=TRUE))
colnames(z) <- varnames
y.id <- 1 # class variable column
z[,y.id] <- as.factor(z[,y.id])
nonmissing <- !is.na(z[,y.id])
z <- z[nonmissing,]
class.codes <- unclass(z[,y.id])
uniq.codes <- unique(sort(class.codes))
pred <- read.table(file='lda_fit.txt',header=TRUE)
pred <- pred[nonmissing,]
tnodes <- unique(sort(pred$node))  ## terminal nodes
coefs <- read.table(file='lda_coefs.txt',header=TRUE)
inodes <- unique(sort(coefs$node))
inodes <- setdiff(inodes,tnodes)   ## intermediate nodes
allnodes <- sort(c(inodes,tnodes))
k <- 0
for(node in allnodes){
    goodplot <- TRUE
    if(node %in% inodes){ ## intermediate node
        terminal <- FALSE
        d <- descendents(node)
        if(length(d) > 0){
            gp <- pred$node %in% d
            k <- k+1
        } else {
            goodplot <- FALSE
        }
    } else { ## terminal node
        terminal <- TRUE
        gp <- pred$node == node
        tmp <- unique(sort(z[gp,y.id]))
        if(length(tmp) <= 1) goodplot <- FALSE
    }
    if(goodplot){
        vgp <- coefs$node == node
        xvars <- coefs$variable[vgp]
        xmeans <- as.numeric(coefs$mean[vgp])
        subdatx <- z[gp,]
        subdatx <- subdatx[,which(names(subdatx) %in% xvars)]
        subdaty <- z[gp,y.id]
        if(length(xvars) == 1){
            xmat <- as.numeric(subdatx)
        } else {
            for(j in 1:ncol(subdatx)){ ## impute missing values
                tmp <- subdatx[,j]
                tmp[is.na(tmp)] <- xmeans[j]
                subdatx[,j] <- tmp
            }
            xmat <- as.matrix(subdatx)
        }
        ncol <- ncol(coefs)
        if(ncol == 4){
            crimdim <- 1
        } else {
            crimdim <- ifelse(is.na(sum(coefs[vgp,5])),1,2)
        }
        if(crimdim == 1){
            if(length(xvars) == 1){
                crimc <- coefs[vgp,4]
                crimx <- xmat*crimc
            } else {
                crimc <- as.matrix(coefs[vgp,4])
                crimx <- xmat %*% crimc
            }
            stripchart(crimx ~ subdaty,method='jitter',
                pch=19,col = 1+unique(sort(class.codes[gp])),
                ylab = 'Class',xlab='LDA coordinate')
            if(terminal){
                title(paste('Node',node,' (terminal)'))
            } else {
                abline(v=splits[k],lty=2)
                title(paste('Node',node,' (intermediate)'))
            }
        } else {
            crimc <- as.matrix(coefs[vgp,4:5])
            crimx <- xmat %*% crimc
            if(eqplot){ ## find variances
                v1 <- 0
                v2 <- 0
                n <- 0
                for(i in uniq.codes){
                    cgp <- subdaty == i
                    nc <- sum(cgp)
                    if(nc > 1){
                        n <- n+nc
                        v1 <- v1+(nc-1)*var(crimx[cgp,1])
                        v2 <- v2+(nc-1)*var(crimx[cgp,2])
                    }
                }
                v1 <- v1/n
                v2 <- v2/n
                crimx[,1] <- crimx[,1]/sqrt(v1)
                crimx[,2] <- crimx[,2]/sqrt(v2)
                eqscplot(crimx[,1],crimx[,2],type='n',
                    xlab='scaled CRIM1',ylab='scaled CRIM2')
            } else {
                plot(crimx[,1],crimx[,2],type='n',xlab='CRIM1',ylab='CRIM2')
            }
            text(crimx[,1],crimx[,2],labels=subdaty,col=1+class.codes[gp])
            if(terminal){
                for(i in uniq.codes){ # add class means
                    cgp <- subdaty == i
                    m1 <- mean(crimx[cgp,1])
                    m2 <- mean(crimx[cgp,2])
                    text(m1,m2,labels=as.character(i),cex=1.1)
                }
                if(length(allnodes) == 1){ # only 1 node
                    if(eqplot){
                        abline(v=splits/sqrt(v1),lty=2)
                    } else {
                        abline(v=splits,lty=2)
                    }
                }
                title(paste('Node',node,' (terminal)'))
            } else {
                if(eqplot){
                    abline(v=splits[k]/sqrt(v1),lty=2)
                } else {
                    abline(v=splits[k],lty=2)
                }
                title(paste('Node',node,' (intermediate)'))
            }
        }
    }
}
plot(c(-1,1),c(-1,1),type='n',xaxt='n',yaxt='n',xlab='',ylab='')
leg.txt <- unique(sort(pred$observed))
leg.pch <- as.character(uniq.codes)
leg.col <- unique(sort(class.codes))+1
legend('center',legend=leg.txt,col=leg.col,pch=leg.pch,
ncol=1,bty='n',cex=1.5)
title('Legend')
dev.off()
