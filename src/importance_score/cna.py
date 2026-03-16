
#####

import shap 
import xgboost 
import pandas as pd
import numpy as np

######

df = pd.read_csv("/Users/sorenbasnet/Documents/Github/STAT_443_FINAL_PROJECT/data/prediction_death_5_years/data_patient_file_cna_rai.txt", sep=r'\s+',index_col=0)
pd.head(df)


X = df.drop(columns=[""])
y = df[""]

# Read the data
X, y = "" 

#####

# Random Forest Classifier 
rf = RandomForestClassifier(n_estimators=100, max_features='sqrt', random_state=42)

#######


# model 
model = xgboost.XGBClassifier()
model.fit(X_train, y_train) 

# SHAP explainer 
explainer = shap.TreeExplainer(model)
# explainer = shap.Explainer(model)

# compute shap values
shap_values = explainer.shap_values(X_test) 

# summary plot 
shap.summary_plot(shap_values, X_test)


