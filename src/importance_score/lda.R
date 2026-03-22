library(MASS)

# Fit the model : grouping ~ predictors 
model_lda <- lda(Species ~ ., data=train_data)

# Print scaling coeddicients and group means 
print(model_lda)


# Predict on new data
predictions <- predict(model_lda, test_data)

# results: predictions$class (labels), predictions$posterior (probabilities)


Package,Best For...,Key Feature
caret,"The ""Old Faithful""",Extremely well-documented; great for quick cross-validation via train().
tidymodels,Modern Tidyverse,Uses the parsnip engine; ideal if you prefer pipes (%>%) and clean data frames.
klaR,Visualization,"Includes partimat(), which creates ""partition plots"" to see how LDA splits your classes."