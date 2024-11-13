setwd("C:/Users/UMAR/Desktop/Assigment 3 - DS/Assignment  3 - FDA")
weekly.data <- read.csv("C:/Users/UMAR/Desktop/Assigment 3 - DS/Assignment  3 - FDA/Weekly.csv")
auto.data <- read.csv("C:/Users/UMAR/Desktop/Assigment 3 - DS/Assignment  3 - FDA/Auto.csv")
str(weekly.data)
summary(weekly.data)

hist(weekly.data$Today, 
     main = "Histogram of Weekly Returns", 
     xlab = "Weekly Returns", 
     col = "purple")
#b.)
weekly.data$Direction <- ifelse(weekly.data$Direction == "Up", 1, 0)
summary(weekly.data$Direction)
logistic_model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                      data = weekly.data, family = binomial)
summary(logistic_model)
# The logistic regression model was fitted with "Direction" as the response variable 
# and the lag variables (Lag1, Lag2, Lag3, Lag4, Lag5) and Volume as predictors.
# Upon reviewing the model summary, it is evident that Lag2 is statistically significant, 
# with a p-value of 0.0296, which is less than the standard significance threshold of 0.05. 
# This suggests that Lag2 has a meaningful effect on predicting the direction of the market.
# However, the other predictors—Lag1, Lag3, Lag4, Lag5, and Volume—have p-values greater than 0.05, 
# indicating that they do not have statistically significant effects on the response variable in this model. 
# Consequently, these variables fail to provide strong evidence against the null hypothesis, 
# which posits that they have no effect on the market direction.
# In summary, while Lag2 appears to be a significant predictor, 
# the other lag variables and Volume do not show a significant relationship with market direction based on this logistic regression model.
#c.)
pred_prob <- predict(logistic_model, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
conf_matrix <- table(Predicted = pred_class, Actual = weekly.data$Direction)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Overall accuracy: ", accuracy))

# c.) Compute the confusion matrix and overall fraction of correct predictions for the logistic regression model

# Get the predicted probabilities from the logistic regression model
pred_prob <- predict(logistic_model, type = "response")

# Convert probabilities to class predictions (1 for "Up", 0 for "Down") using a threshold of 0.5
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Create the confusion matrix by comparing predicted values to actual values
conf_matrix <- table(Predicted = pred_class, Actual = weekly.data$Direction)

# Display the confusion matrix
print(conf_matrix)

# Calculate the overall accuracy by dividing the sum of the diagonal elements (correct predictions) by the total number of observations
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Print the overall accuracy
print(paste("Overall accuracy: ", accuracy))

# Explanation of confusion matrix:
# The confusion matrix shows:
# - True Negatives (TN) = 54, which indicates how often the model correctly predicted "Down".
# - False Positives (FP) = 48, meaning the model incorrectly predicted "Up" when the actual result was "Down".
# - False Negatives (FN) = 430, where the model predicted "Down", but the actual result was "Up".
# - True Positives (TP) = 557, showing how often the model correctly predicted "Up".
#
# The overall accuracy of 56.1% means the model correctly predicted the direction 56.1% of the time.
# However, there is a higher count of False Negatives (430), meaning the model missed predicting upward trends ("Up").
# This suggests that while the model captures downward market movements well, it might be less effective at predicting upward trends.
# To improve this model, adjustments or additional features could be considered.


#d.)
# Split data into training and testing sets
train_data <- subset(weekly.data, Year <= 2008)
test_data <- subset(weekly.data, Year > 2008)

# Fit logistic regression model using only Lag2 as predictor
logistic_model_lag2 <- glm(Direction ~ Lag2, data = train_data, family = binomial)

# View summary of the model
summary(logistic_model_lag2)

# Predict probabilities for the test set
pred_prob_lag2 <- predict(logistic_model_lag2, newdata = test_data, type = "response")

# Convert probabilities to class labels using a threshold of 0.5
pred_class_lag2 <- ifelse(pred_prob_lag2 > 0.5, 1, 0)

# Create the confusion matrix for the predictions
conf_matrix_lag2 <- table(Predicted = pred_class_lag2, Actual = test_data$Direction)

# Display the confusion matrix
print(conf_matrix_lag2)

# Calculate the accuracy of the model
accuracy_lag2 <- sum(diag(conf_matrix_lag2)) / sum(conf_matrix_lag2)

# Print the overall accuracy
print(paste("Overall accuracy (Logistic Regression with Lag2): ", accuracy_lag2))


# Report summary
# In part (d), a logistic regression model was fitted using Lag2 as the sole predictor, 
# with training data from 1990 to 2008. The model revealed that Lag2 was statistically 
# significant with a p-value of 0.04298, indicating its relevance for predicting market direction.
# The model was then tested on data from 2009 to 2010, with an overall accuracy of 62.5%. 
# The confusion matrix showed that 56 out of 90 instances where the actual market direction 
# was "Up" were correctly classified, while 34 were incorrectly classified as "Down." 
# For the 44 "Down" cases, 9 were correctly predicted, and 5 were misclassified as "Up."
# While the model performed reasonably well, the significant number of misclassifications, 
# particularly false negatives, highlights that it struggles with predicting upward market 
# movements. This suggests the need for further model refinement or additional predictors to improve performance.

# e.)
# Repeat d) using LDA (Linear Discriminant Analysis)

# Load the required library for LDA
library(MASS)

# Fit LDA model using only Lag2 as the predictor
lda_model <- lda(Direction ~ Lag2, data = train_data)

# View the LDA model
print(lda_model)

# Predict the class probabilities for the test set
lda_pred_prob <- predict(lda_model, newdata = test_data)$posterior[, 2]

# Convert probabilities to class labels using a threshold of 0.5
lda_pred_class <- ifelse(lda_pred_prob > 0.5, 1, 0)

# Create the confusion matrix for the LDA predictions
lda_conf_matrix <- table(Predicted = lda_pred_class, Actual = test_data$Direction)

# Display the confusion matrix for LDA
print(lda_conf_matrix)

# Calculate the accuracy of the LDA model
lda_accuracy <- sum(diag(lda_conf_matrix)) / sum(lda_conf_matrix)

# Print the overall accuracy for LDA
print(paste("Overall accuracy (LDA with Lag2): ", lda_accuracy))



# In this part of the analysis, we applied Linear Discriminant Analysis (LDA) using Lag2 as the sole predictor. 
# The model was trained on data from 1990 to 2008 and tested on data from 2009 to 2010.
# The LDA model achieved an accuracy of 62.5%, with a confusion matrix indicating 9 True Negatives, 5 False Positives, 
# 34 False Negatives, and 56 True Positives.

# The result is comparable to the logistic regression model, suggesting that Lag2 alone provides limited predictive power.
# Further improvement can be achieved by incorporating additional predictors or exploring alternative methods.



#f.)
# Fit QDA model using Lag2 as the predictor
qda_model <- qda(Direction ~ Lag2, data = train_data)

# Predict the class probabilities for the test set
qda_pred_prob <- predict(qda_model, newdata = test_data)$posterior[, 2]

# Convert probabilities to class labels using a threshold of 0.5
qda_pred_class <- ifelse(qda_pred_prob > 0.5, 1, 0)

# Create the confusion matrix for the QDA predictions
qda_conf_matrix <- table(Predicted = qda_pred_class, Actual = test_data$Direction)

# Display the confusion matrix for QDA
print(qda_conf_matrix)

# Calculate the accuracy of the QDA model
qda_accuracy <- sum(diag(qda_conf_matrix)) / sum(qda_conf_matrix)

# Print the overall accuracy for QDA
print(paste("Overall accuracy (QDA with Lag2): ", qda_accuracy))


# In part (f), Quadratic Discriminant Analysis (QDA) was applied with Lag2 as the sole predictor.
# The model, trained on data from 1990 to 2008 and tested on data from 2009 to 2010, achieved an accuracy of 41.35%.
# The confusion matrix revealed 43 True Positives and 61 False Positives, with no True Negatives,
# indicating that the model struggled to correctly classify "Down" movements.
# This performance suggests that QDA, with just Lag2, is less effective than logistic regression and LDA for this dataset.
# Further improvements may be achieved by incorporating additional predictors or exploring alternative methods.


#g.)
# Load the required library for KNN
library(class)

# Use KNN with K = 1, training on Lag2
knn_pred_class <- knn(train = train_data[, "Lag2", drop = FALSE], 
                      test = test_data[, "Lag2", drop = FALSE], 
                      cl = train_data$Direction, k = 1)

# Create the confusion matrix for the KNN predictions
knn_conf_matrix <- table(Predicted = knn_pred_class, Actual = test_data$Direction)

# Display the confusion matrix for KNN
print(knn_conf_matrix)

# Calculate the accuracy of the KNN model
knn_accuracy <- sum(diag(knn_conf_matrix)) / sum(knn_conf_matrix)

# Print the overall accuracy for KNN
print(paste("Overall accuracy (KNN with Lag2): ", knn_accuracy))


# The K-Nearest Neighbors (KNN) algorithm with K = 1 was applied, using Lag2 as the sole predictor.
# The model was trained on data from 1990 to 2008 and tested on data from 2009 to 2010.
# The KNN model achieved an accuracy of 50.96%, with the confusion matrix showing:
# 21 True Negatives, 29 False Positives, 22 False Negatives, and 32 True Positives.
# This performance is similar to that of the QDA model, suggesting that KNN with Lag2 alone has limited predictive power.
# Future improvements could involve adjusting the value of K or adding more predictors to enhance model accuracy.



#h.)
# Comparative analysis of different models (Logistic Regression, LDA, QDA, and KNN) using Lag2 as the predictor:
# Logistic Regression (using Lag2) achieved an accuracy of 62.5%, with the confusion matrix showing:
# 9 True Negatives, 5 False Positives, 34 False Negatives, and 56 True Positives.
# Linear Discriminant Analysis (LDA) with Lag2 also achieved an accuracy of 62.5%, with the same confusion matrix 
# as the logistic regression model, indicating similar performance between the two methods.
# Quadratic Discriminant Analysis (QDA) with Lag2, however, performed poorly with an accuracy of 41.35%.
# The confusion matrix for QDA showed 43 True Negatives, 61 False Positives, indicating weaker predictive power compared to logistic regression and LDA.
# K-Nearest Neighbors (KNN) with K = 1 and Lag2 as the predictor achieved an accuracy of 50.96%.
# The confusion matrix showed 21 True Negatives, 29 False Positives, 22 False Negatives, and 32 True Positives, 
# which is similar to the performance of QDA, indicating limited predictive power with KNN.

# Conclusion:
# - Logistic Regression and LDA performed similarly, with 62.5% accuracy, making them the most reliable models in this analysis.
# - QDA and KNN with Lag2 as the predictor showed lower accuracy (41.35% for QDA and 50.96% for KNN).
# - The results suggest that a single predictor, Lag2, is not highly predictive for market direction. 
#   Incorporating more predictors or using advanced techniques may improve model performance.


#1.)
# Create squared terms and interaction terms for experimentation
train_data$Lag2_sq <- train_data$Lag2^2
test_data$Lag2_sq <- test_data$Lag2^2
train_data$Lag1_Lag2_interaction <- train_data$Lag1 * train_data$Lag2
test_data$Lag1_Lag2_interaction <- test_data$Lag1 * test_data$Lag2
# Creating different subsets of features for experimentation
train_data_lag2_sq <- train_data[, c("Lag2", "Lag2_sq", "Direction")]
test_data_lag2_sq <- test_data[, c("Lag2", "Lag2_sq", "Direction")]

train_data_lag123 <- train_data[, c("Lag1", "Lag2", "Lag3", "Direction")]
test_data_lag123 <- test_data[, c("Lag1", "Lag2", "Lag3", "Direction")]

train_data_all <- train_data[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Direction")]
test_data_all <- test_data[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Direction")]

train_data_interaction <- train_data[, c("Lag1", "Lag2", "Lag1_Lag2_interaction", "Direction")]
test_data_interaction <- test_data[, c("Lag1", "Lag2", "Lag1_Lag2_interaction", "Direction")]





# KNN function for experimentation
knn_accuracy_experiment <- function(train_data, test_data) {
  accuracy_results <- numeric(4)
  
  for (k in c(1, 3, 5, 7)) {
    knn_pred_class <- knn(train = train_data[, -ncol(train_data)], 
                          test = test_data[, -ncol(test_data)], 
                          cl = train_data$Direction, k = k)
    
    conf_matrix <- table(Predicted = knn_pred_class, Actual = test_data$Direction)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    accuracy_results[k/2] <- accuracy
    cat(paste("Accuracy for KNN with K =", k, ":", accuracy, "\n"))
  }
  return(accuracy_results)
}


# Logistic Regression with different combinations
logistic_model_lag2_sq <- glm(Direction ~ Lag2 + Lag2_sq, data = train_data_lag2_sq, family = binomial)
logistic_pred_prob_lag2_sq <- predict(logistic_model_lag2_sq, newdata = test_data_lag2_sq, type = "response")
logistic_pred_class_lag2_sq <- ifelse(logistic_pred_prob_lag2_sq > 0.5, 1, 0)
conf_matrix_logistic <- table(Predicted = logistic_pred_class_lag2_sq, Actual = test_data_lag2_sq$Direction)
logistic_accuracy <- sum(diag(conf_matrix_logistic)) / sum(conf_matrix_logistic)

# Similar models for LDA, QDA, KNN
lda_model_lag2_sq <- lda(Direction ~ Lag2 + Lag2_sq, data = train_data_lag2_sq)
lda_pred_prob_lag2_sq <- predict(lda_model_lag2_sq, newdata = test_data_lag2_sq)$posterior[, 2]
lda_pred_class_lag2_sq <- ifelse(lda_pred_prob_lag2_sq > 0.5, 1, 0)
conf_matrix_lda <- table(Predicted = lda_pred_class_lag2_sq, Actual = test_data_lag2_sq$Direction)
lda_accuracy <- sum(diag(conf_matrix_lda)) / sum(conf_matrix_lda)

qda_model_lag2_sq <- qda(Direction ~ Lag2 + Lag2_sq, data = train_data_lag2_sq)
qda_pred_prob_lag2_sq <- predict(qda_model_lag2_sq, newdata = test_data_lag2_sq)$posterior[, 2]
qda_pred_class_lag2_sq <- ifelse(qda_pred_prob_lag2_sq > 0.5, 1, 0)
conf_matrix_qda <- table(Predicted = qda_pred_class_lag2_sq, Actual = test_data_lag2_sq$Direction)
qda_accuracy <- sum(diag(conf_matrix_qda)) / sum(conf_matrix_qda)

# Evaluate KNN with multiple K values
knn_accuracy_lag2_sq <- knn_accuracy_experiment(train_data_lag2_sq, test_data_lag2_sq)
# Summarize results for different models and combinations
results <- data.frame(
  Combination = c("Lag2 + Lag2^2", "Lag1 + Lag2 + Lag3", "Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume", "Lag1 + Lag2 + Interaction"),
  Logistic_Accuracy = c(logistic_accuracy, NA, NA, NA),
  LDA_Accuracy = c(lda_accuracy, NA, NA, NA),
  QDA_Accuracy = c(qda_accuracy, NA, NA, NA),
  KNN_Accuracy_1 = c(knn_accuracy_lag2_sq[1], NA, NA, NA),
  KNN_Accuracy_3 = c(knn_accuracy_lag2_sq[2], NA, NA, NA),
  KNN_Accuracy_5 = c(knn_accuracy_lag2_sq[3], NA, NA, NA),
  KNN_Accuracy_7 = c(knn_accuracy_lag2_sq[4], NA, NA, NA)
)

# Display the results for comparison
print(results)


# Assuming 'Lag2 + Lag2^2' is the best combination
logistic_model_best <- glm(Direction ~ Lag2 + Lag2_sq, data = train_data_lag2_sq, family = binomial)

# Generate predicted probabilities on the test data
logistic_pred_prob_best <- predict(logistic_model_best, newdata = test_data_lag2_sq, type = "response")

# Convert predicted probabilities to class predictions
logistic_pred_class_best <- ifelse(logistic_pred_prob_best > 0.5, "Up", "Down")

# Create confusion matrix for Logistic Regression
conf_matrix_best_model <- table(Predicted = logistic_pred_class_best, Actual = test_data_lag2_sq$Direction)

# Print the confusion matrix
print(conf_matrix_best_model)
# Calculate accuracy
accuracy_best_model <- sum(diag(conf_matrix_best_model)) / sum(conf_matrix_best_model)
print(paste("Accuracy of the best model: ", accuracy_best_model))
# True Positives, False Positives, True Negatives, False Negatives
TP <- conf_matrix_best_model[2, 2]
TN <- conf_matrix_best_model[1, 1]
FP <- conf_matrix_best_model[1, 2]
FN <- conf_matrix_best_model[2, 1]

# Accuracy
accuracy_best_model <- (TP + TN) / sum(conf_matrix_best_model)

# Precision (Up) and Recall (Up)
precision_up <- TP / (TP + FP)
recall_up <- TP / (TP + FN)

# F1 Score (Up)
f1_up <- 2 * (precision_up * recall_up) / (precision_up + recall_up)

# Print the results
print(paste("Accuracy: ", accuracy_best_model))
print(paste("Precision (Up): ", precision_up))
print(paste("Recall (Up): ", recall_up))
print(paste("F1 Score (Up): ", f1_up))





# Load required libraries
library(class)    # For KNN
library(MASS)     # For LDA and QDA
library(caret)    # For Cross-Validation

# Step 1: Load your data
# Assuming 'train_data' and 'test_data' are already loaded

# Step 2: Create additional features like squared terms and interaction terms

train_data$Lag2_sq <- train_data$Lag2^2
test_data$Lag2_sq <- test_data$Lag2^2
train_data$Lag1_Lag2_interaction <- train_data$Lag1 * train_data$Lag2
test_data$Lag1_Lag2_interaction <- test_data$Lag1 * test_data$Lag2

# Step 3: Create data subsets with different feature combinations

train_data_all_features <- train_data[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Lag1_Lag2_interaction", "Direction")]
test_data_all_features <- test_data[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume", "Lag1_Lag2_interaction", "Direction")]

# Step 4: Train a Logistic Regression model with all features
logistic_model_all_features <- glm(Direction ~ ., data = train_data_all_features, family = binomial)

# Predict using the Logistic model
logistic_pred_prob_all_features <- predict(logistic_model_all_features, newdata = test_data_all_features, type = "response")
logistic_pred_class_all_features <- ifelse(logistic_pred_prob_all_features > 0.5, 1, 0)
conf_matrix_logistic_all <- table(Predicted = logistic_pred_class_all_features, Actual = test_data_all_features$Direction)

# Logistic model accuracy
logistic_accuracy_all_features <- sum(diag(conf_matrix_logistic_all)) / sum(conf_matrix_logistic_all)
cat("Accuracy for Logistic Regression with all features: ", logistic_accuracy_all_features, "\n")

# Step 5: Hyperparameter Tuning for KNN

knn_tuning <- function(train_data, test_data) {
  best_k <- NULL
  best_accuracy <- 0
  
  for (k in seq(1, 20, by = 2)) {
    knn_pred_class <- knn(train = train_data[, -ncol(train_data)], 
                          test = test_data[, -ncol(test_data)], 
                          cl = train_data$Direction, k = k)
    
    conf_matrix <- table(Predicted = knn_pred_class, Actual = test_data$Direction)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    cat(paste("Accuracy for KNN with k =", k, ":", accuracy, "\n"))
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_k <- k
    }
  }
  
  cat(paste("Best K for KNN: ", best_k, " with accuracy: ", best_accuracy, "\n"))
  return(best_k)
}

# Find the best k for KNN
best_k <- knn_tuning(train_data_all_features, test_data_all_features)

# Step 6: Cross-Validation for Logistic Regression

cv_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train Logistic Regression using cross-validation
logistic_cv_model <- train(Direction ~ ., data = train_data_all_features, 
                           method = "glm", 
                           family = binomial, 
                           trControl = cv_control)

# Display Logistic CV results
print(logistic_cv_model)

# Step 7: Cross-Validation for KNN
knn_cv_model <- train(Direction ~ ., data = train_data_all_features, 
                      method = "knn", 
                      trControl = cv_control, 
                      tuneLength = 10)  # Try different values of k

# Display KNN CV results
print(knn_cv_model)

# Step 8: Evaluate KNN model and Logistic Regression model on the test data

# Logistic Regression prediction
logistic_pred_class_cv <- predict(logistic_cv_model, newdata = test_data_all_features)
conf_matrix_logistic_cv <- table(Predicted = logistic_pred_class_cv, Actual = test_data_all_features$Direction)
logistic_accuracy_cv <- sum(diag(conf_matrix_logistic_cv)) / sum(conf_matrix_logistic_cv)
cat("Accuracy from cross-validation for Logistic Regression: ", logistic_accuracy_cv, "\n")

# KNN prediction
knn_pred_class_cv <- predict(knn_cv_model, newdata = test_data_all_features)
conf_matrix_knn_cv <- table(Predicted = knn_pred_class_cv, Actual = test_data_all_features$Direction)
knn_accuracy_cv <- sum(diag(conf_matrix_knn_cv)) / sum(conf_matrix_knn_cv)
cat("Accuracy from cross-validation for KNN: ", knn_accuracy_cv, "\n")

# Step 9: Compare Results

results <- data.frame(
  Model = c("Logistic Regression (All Features)", "KNN (Cross-Validation)"),
  Accuracy = c(logistic_accuracy_cv, knn_accuracy_cv)
)

print(results)

# Step 10: Final Evaluation on the Best Model
# Assuming Logistic Regression is the best based on comparison

# Logistic Regression final model
logistic_model_best <- glm(Direction ~ ., data = train_data_all_features, family = binomial)
logistic_pred_prob_best <- predict(logistic_model_best, newdata = test_data_all_features, type = "response")
logistic_pred_class_best <- ifelse(logistic_pred_prob_best > 0.5, "Up", "Down")

# Confusion matrix for Logistic Regression
conf_matrix_best_model <- table(Predicted = logistic_pred_class_best, Actual = test_data_all_features$Direction)
print(conf_matrix_best_model)

# Accuracy
accuracy_best_model <- sum(diag(conf_matrix_best_model)) / sum(conf_matrix_best_model)
print(paste("Accuracy of the best model: ", accuracy_best_model))

# True Positives, False Positives, True Negatives, False Negatives
TP <- conf_matrix_best_model[2, 2]
TN <- conf_matrix_best_model[1, 1]
FP <- conf_matrix_best_model[1, 2]
FN <- conf_matrix_best_model[2, 1]

# Precision, Recall, and F1 Score for "Up" (positive class)
precision_up <- TP / (TP + FP)
recall_up <- TP / (TP + FN)
f1_up <- 2 * (precision_up * recall_up) / (precision_up + recall_up)

# Print Precision, Recall, and F1 Score
print(paste("Precision (Up): ", precision_up))
print(paste("Recall (Up): ", recall_up))
print(paste("F1 Score (Up): ", f1_up))
# Summary Report for Model Comparison

# 1. Introduction
# This analysis compares the performance of two classification models: K-Nearest Neighbors (KNN) and Logistic Regression. 
# We aim to evaluate the accuracy and generalization ability of these models using cross-validation.

# 2. Models Tested
# - **K-Nearest Neighbors (KNN)**: A non-parametric method that classifies data points based on the majority label of the k nearest neighbors.
# - **Logistic Regression**: A parametric model that predicts the probability of a binary outcome by fitting a logistic function.

# 3. Evaluation Metrics
# - **Accuracy**: The proportion of correctly classified instances out of all instances.
# - **Cross-Validation**: A method to evaluate the performance of models by splitting the data into multiple subsets (folds) and testing the model on each subset.

# 4. Results

# KNN Performance
# - **Best Performance**: KNN with k = 19 demonstrated the highest classification accuracy among all tested configurations.
# - **Cross-validation**: Results from cross-validation were consistent, showing a slight advantage in accuracy for KNN over Logistic Regression.

# Logistic Regression Performance
# - **Accuracy**: Logistic Regression was slightly less accurate than KNN, but still showed solid classification performance.
# - **Interpretability**: Logistic Regression offers advantages in interpretability, as it produces coefficients that can be directly analyzed to understand the relationship between predictors and the outcome.

# 5. Conclusions:
# - **KNN with k = 19** is the best performing model in terms of classification accuracy. 
# - **Logistic Regression**, while slightly less accurate, is still a viable model, particularly for cases where interpretability is more important or if a simpler model is preferred.
# - **Cross-validation results** for both models are similar, with KNN showing a small advantage in terms of classification accuracy.

# 6. Recommendations
# - If your primary goal is achieving the highest classification accuracy, KNN with k = 19 should be preferred.
# - If model interpretability and simplicity are more important, Logistic Regression is a solid alternative.






#PROBLEM 2
setwd("C:/Users/UMAR/Desktop/Assigment 3 - DS/Assignment  3 - FDA")
auto.data <- read.csv("C:/Users/UMAR/Desktop/Assigment 3 - DS/Assignment  3 - FDA/auto.csv")

#a
# Calculate the median of mpg
median_mpg <- median(auto.data$mpg)

# Create the binary variable mpg01
auto.data$mpg01 <- ifelse(auto.data$mpg > median_mpg, 1, 0)

# Check the first few rows to verify
head(auto.data)


#b.)
# Boxplots to explore associations between mpg01 and other features
boxplot(horsepower ~ mpg01, data = auto.data, main = "Horsepower vs mpg01", 
        xlab = "mpg01", ylab = "Horsepower", col = c("lightblue", "lightgreen"))
boxplot(weight ~ mpg01, data = auto.data, main = "Weight vs mpg01", 
        xlab = "mpg01", ylab = "Weight", col = c("lightblue", "lightgreen"))
boxplot(acceleration ~ mpg01, data = auto.data, main = "Acceleration vs mpg01", 
        xlab = "mpg01", ylab = "Acceleration", col = c("lightblue", "lightgreen"))


# Scatterplots to explore relationships between continuous variables and mpg01
plot(auto.data$horsepower, auto.data$mpg01, main = "Horsepower vs mpg01", 
     xlab = "Horsepower", ylab = "mpg01", col = ifelse(auto.data$mpg01 == 1, "blue", "red"))
plot(auto.data$weight, auto.data$mpg01, main = "Weight vs mpg01", 
     xlab = "Weight", ylab = "mpg01", col = ifelse(auto.data$mpg01 == 1, "blue", "red"))
plot(auto.data$acceleration, auto.data$mpg01, main = "Acceleration vs mpg01", 
     xlab = "Acceleration", ylab = "mpg01", col = ifelse(auto.data$mpg01 == 1, "blue", "red"))

#Findings 
# The analysis of the relationship between the binary variable `mpg01` (high or low fuel efficiency) 
#and several continuous features reveals important insights. From the boxplots and scatterplots,
#we observe that cars with higher horsepower are more likely to have lower fuel efficiency (`mpg01 = 0`),
#while cars with lower horsepower tend to have higher fuel efficiency (`mpg01 = 1`). Similarly,
#there is a clear negative relationship between weight and `mpg01`, with lighter cars generally showing 
#higher fuel efficiency. In terms of acceleration, although the relationship is less pronounced, 
#cars with higher acceleration tend to have higher fuel efficiency as well. 
#These findings suggest that **horsepower** and **weight** are the strongest predictors of fuel efficiency,
#with **weight** being the most significant, followed by **horsepower**.
#The scatterplots further reinforce these trends, with cars having high fuel efficiency generally being lighter
#and having lower horsepower,whereas cars with low fuel efficiency tend to be heavier and more powerful.

#c.)
# Set seed for reproducibility
set.seed(42)

# Split the data: 70% for training, 30% for testing
train_index <- sample(1:nrow(auto.data), 0.7 * nrow(auto.data))
train_data <- auto.data[train_index, ]
test_data <- auto.data[-train_index, ]
# Check the dimensions of the training and testing datasets
dim(train_data)  # Number of rows and columns in the training set
dim(test_data)   # Number of rows and columns in the testing set

# View the first few rows of the training set
head(train_data)

# View the first few rows of the testing set
head(test_data)



#d.)
# Load necessary library for LDA
library(MASS)

# Fit LDA model
lda_model <- lda(mpg01 ~ horsepower + weight + acceleration, data = train_data)

# Make predictions on the test set
lda_predictions <- predict(lda_model, test_data)

# Get the predicted class
lda_pred_class <- lda_predictions$class

# Confusion matrix to evaluate performance
conf_matrix_lda <- table(Predicted = lda_pred_class, Actual = test_data$mpg01)
conf_matrix_lda

# Calculate the test error (misclassification rate)
lda_error <- mean(lda_pred_class != test_data$mpg01)
lda_error

# The Linear Discriminant Analysis (LDA) model, when applied to the test set, produced a misclassification rate of 8.47%. 
# The confusion matrix revealed 41 true negatives, 67 true positives, 6 false positives, and 4 false negatives. 
# This indicates that the LDA model performed reasonably well, with only a small proportion of errors in its predictions.

#e.)
# Load necessary library for QDA
library(MASS)

# Fit QDA model
qda_model <- qda(mpg01 ~ horsepower + weight + acceleration, data = train_data)

# Make predictions on the test set
qda_predictions <- predict(qda_model, test_data)

# Get the predicted class
qda_pred_class <- qda_predictions$class

# Confusion matrix to evaluate performance
conf_matrix_qda <- table(Predicted = qda_pred_class, Actual = test_data$mpg01)
conf_matrix_qda

# Calculate the test error (misclassification rate)
qda_error <- mean(qda_pred_class != test_data$mpg01)
qda_error

# The Quadratic Discriminant Analysis (QDA) model resulted in a test error of 9.32%, slightly higher than the LDA model. 
# The confusion matrix showed 40 true negatives, 67 true positives, 7 false positives, and 4 false negatives. 
# While the QDA model performs well, it had marginally more misclassifications compared to LDA, suggesting it is slightly less effective for this particular dataset.

#f.) Logistic Regression
# Fit logistic regression model
logit_model <- glm(mpg01 ~ horsepower + weight + acceleration, data = train_data, family = binomial)

# Make predictions on the test set
logit_predictions <- predict(logit_model, test_data, type = "response")

# Convert predictions to binary (1 if probability > 0.5, else 0)
logit_pred_class <- ifelse(logit_predictions > 0.5, 1, 0)

# Confusion matrix to evaluate performance
conf_matrix_logit <- table(Predicted = logit_pred_class, Actual = test_data$mpg01)
conf_matrix_logit

# Calculate the test error (misclassification rate)
logit_error <- mean(logit_pred_class != test_data$mpg01)
logit_error



# The Logistic Regression model exhibited the best performance with a test error of 7.63%, outperforming both LDA and QDA. 
# The confusion matrix indicated 44 true negatives, 65 true positives, 3 false positives, and 6 false negatives. 
# This suggests that logistic regression was the most accurate model, with the fewest misclassifications on the test set.

#g.)
# Load necessary library for KNN
library(class)

# Standardize the continuous predictor variables
train_data_scaled <- scale(train_data[, c("horsepower", "weight", "acceleration")])
test_data_scaled <- scale(test_data[, c("horsepower", "weight", "acceleration")])

# Set up a range of K values to test
k_values <- c(1, 3, 5, 7, 9, 11)

# Initialize a vector to store errors for each K
knn_errors <- numeric(length(k_values))

# Perform KNN for each value of K
for (i in 1:length(k_values)) {
  # Perform KNN with current K value
  knn_pred_class <- knn(train = train_data_scaled, test = test_data_scaled, 
                        cl = train_data$mpg01, k = k_values[i])
  
  # Calculate the test error (misclassification rate)
  knn_errors[i] <- mean(knn_pred_class != test_data$mpg01)
}

# Display the K values and corresponding errors
data.frame(K = k_values, Error = knn_errors)

# The K-Nearest Neighbors (KNN) model was performed using the most associated predictors, horsepower and weight, 
# from part (b) to predict `mpg01`. The test errors for different values of K were as follows: 
# 19.49% for K = 1, 16.10% for K = 3, 15.25% for K = 5 and K = 7, and 11.86% for K = 9 and K = 11.
# The lowest test errors were obtained for K = 9 and K = 11, both resulting in a test error of 11.86%. 
# This suggests that K = 9 or K = 11 performed best on this dataset.
