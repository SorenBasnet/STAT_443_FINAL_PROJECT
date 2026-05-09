rm(list=ls())

# Soren Basnet 
# sbasnet3@wisc.edu

require("FITSio")

cB58 = readFrameFromFITS("cB58_Lyman_break.fit")

cB58_standardized <- (cB58$FLUX - mean(cB58$FLUX)) / sd(cB58$FLUX)

n = length(cB58_standardized)

files = list.files("data")

# Initialize results which will later turn into hw2.csv
results = data.frame(distance=numeric(),spectrumID=character(),shift=integer(),stringsAsFactors=FALSE)

for (file in files) {

    noisy <- readFrameFromFITS(paste0("data/", file))
    stn_noisy <- (noisy$flux - mean(noisy$flux)) / sd(noisy$flux )
    l <- length(stn_noisy)

    possible_shifts <- 0:(l - n)

    all_distance <- numeric(length(possible_shifts))
    
    # Method used :
    #I tried distance formula, l2 minimization and landed 
    # with extraction of residuals using lm() function as 
    # the best method. 


    for (i in 1:length(possible_shifts)) {

        shift <- possible_shifts[i]
        segment <- stn_noisy[(shift + 1):(shift + n)]

        m1 <- lm(cB58_standardized ~ segment)

        all_distance[i] <- sum((m1$residuals)^2)
   }


    best_index <- which.min(all_distance)


result = data.frame( distance = all_distance[best_index],spectrumID = file, shift = possible_shifts[best_index], stringsAsFactors = FALSE )

    results <- rbind(results, result)
}


results = results[order(results$distance),]

write.table(results,file="hw2.csv",sep=",",row.names=FALSE,col.names=FALSE,quote=FALSE)
