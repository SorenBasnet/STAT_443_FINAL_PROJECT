library(pROC)

# after fitting model m1 
roc_obj <- roc(data$outcome, predict(m1, type="response"))
auc(roc_obj)