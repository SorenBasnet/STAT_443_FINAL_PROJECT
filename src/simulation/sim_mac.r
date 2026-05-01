library(rpart)
library(partykit)
library(ranger)

results <- data.frame(
  model = c("ctree", "cforest", "rpart", "ranger", "gcon", "gforest"),
  err = 0, 
  err2 = 0,
  stringsAsFactors = FALSE
)

for(i in 1:nsim){
  set.seed(i)
  ran <- rbinom(n, 1, 0.5)
  train <- ran == 1
  test <- ran == 0
  
  train_data <- z_clean[train, ]
  test_data <- z_clean[test, ]
  y_test <- test_data$y
  
  ### 1. ctree
  ct <- try(ctree(y ~ ., data = train_data), silent = TRUE)
  if(!inherits(ct, "try-error")) {
    ct.pred <- predict(ct, newdata = test_data)
    error <- mean(y_test != ct.pred)
    results[1, "err"] <- results[1, "err"] + error
    results[1, "err2"] <- results[1, "err2"] + error^2
  }
  
  rp <- try(rpart(y ~ ., data = train_data, method = "class"), silent = TRUE)
  if(!inherits(rp, "try-error")) {
    rp.pred <- predict(rp, newdata = test_data, type = "class")
    error <- mean(y_test != rp.pred)
    results[3, "err"] <- results[3, "err"] + error
    results[3, "err2"] <- results[3, "err2"] + error^2
  }
  
  ran_mod <- try(ranger(y ~ ., data = train_data, respect.unordered.factors = "order"), silent = TRUE)
  if(!inherits(ran_mod, "try-error")) {
    ran.pred <- predict(ran_mod, data = test_data)$predictions
    error <- mean(y_test != ran.pred)
    results[4, "err"] <- results[4, "err"] + error
    results[4, "err2"] <- results[4, "err2"] + error^2
  }
  
  ### 4. GUIDE implementation
  # GUIDE requires column 57 (death_5yr) to be identified as 'd'
  z_guide <- z_clean
  z_guide$y[test] <- NA
  write.table(z_guide, "simdata.txt", row.names=FALSE, col.names=TRUE, quote=FALSE)
  
  # Ensure input.txt exists in the working directory
  if(file.exists("input.txt")) {
    system("./guide < input.txt > simcons.log")
    if(file.exists("fit.txt")) {
      z1 <- read.table("fit.txt", header=TRUE)
      gcon.pred <- z1$predicted[test]
      error <- mean(as.character(y_test) != as.character(gcon.pred))
      results[5, "err"] <- results[5, "err"] + error
      results[5, "err2"] <- results[5, "err2"] + error^2
    }
  }
  
  if(i %% 5 == 0 || i == nsim){
    cat("\n--- Progress Report: Iteration", i, "---\n")
    for(j in 1:nrow(results)){
      if(results$err[j] > 0) {
        m_mean <- results$err[j] / i
        m_se <- if(i > 1) sqrt(max(0, (results$err2[j] - i * m_mean^2) / (i * (i - 1)))) else 0
        
        cat(sprintf("%-12s | Error: %.4f | SE: %.4f\n", 
                    results$model[j], m_mean, m_se))
      }
    }
  }
}

#Iteration: 45 
#ctree           Error: 0.2066 (SE: 0.0000)
#cforest         Error: 0.1995 (SE: 0.0000)
#rpart           Error: 0.2273 (SE: 0.0000)
#ranger          Error: 0.1934 (SE: 0.0000)
#gcon            Error: 0.2258 (SE: 0.0000)
#gforest         Error: 0.2303 (SE: 0.0000)


#Iteration: 50 
#ctree           Error: 0.2029 (SE: 0.0000)
#cforest         Error: 0.1957 (SE: 0.0000)
#rpart           Error: 0.2231 (SE: 0.0000)
#ranger          Error: 0.1901 (SE: 0.0000)
#gcon            Error: 0.2212 (SE: 0.0000)
#gforest         Error: 0.2259 (SE: 0.0000)




# ============= 
