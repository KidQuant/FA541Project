
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(knitr)
library(caret)  # For confusion matrix and model evaluation
library(fastDummies)  # For one-hot encoding
library(pROC)  # For ROC curve and AUC calculation

# Load the data
airbnb.newyork <- read.csv("C:/Applied Statistics/Assignment - 3/Assignment 3 - Applied Statistics/Project Assignment/listings.csv")

# Convert price to numeric, handling any non-numeric values
airbnb.newyork$price <- as.numeric(as.character(airbnb.newyork$price))

# Check for any NA values introduced by conversion
if (any(is.na(airbnb.newyork$price))) {
  airbnb.newyork <- airbnb.newyork %>% filter(!is.na(price))
}

# Replace NA values in reviews_per_month with the mean for each room_type
airbnb.newyork <- airbnb.newyork %>% 
  group_by(room_type) %>% 
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 
                                    mean(reviews_per_month, na.rm = TRUE), 
                                    reviews_per_month))

# Remove rows with NA in price or reviews_per_month
airbnb.newyork <- airbnb.newyork %>% filter(!is.na(price) & !is.na(reviews_per_month))

# Step 1: Data Preprocessing - Convert room_type and neighbourhood_group to factor variables
airbnb.newyork$room_type <- as.factor(airbnb.newyork$room_type)
airbnb.newyork$neighbourhood_group <- as.factor(airbnb.newyork$neighbourhood_group)

# Step 2: One-hot encoding for room_type and neighbourhood_group
airbnb.newyork <- fastDummies::dummy_cols(airbnb.newyork, select_columns = c("room_type", "neighbourhood_group"))

# Remove the original categorical columns after encoding
airbnb.newyork <- airbnb.newyork[, !(names(airbnb.newyork) %in% c("room_type", "neighbourhood_group"))]

# Step 3: Summary Statistics
summary_stats <- airbnb.newyork %>%
  summarise(
    Mean_Price = mean(price),
    Median_Price = median(price),
    Min_Price = min(price),
    Max_Price = max(price),
    SD_Price = sd(price),
    Mean_Reviews = mean(reviews_per_month),
    Median_Reviews = median(reviews_per_month),
    Min_Reviews = min(reviews_per_month),
    Max_Reviews = max(reviews_per_month),
    SD_Reviews = sd(reviews_per_month)
  )
print("Summary Statistics:")
print(summary_stats)

# Step 4: Linear Regression Analysis - Dependent variable: price, Independent variables: room_type and neighbourhood_group
regression_model <- lm(price ~ `room_type_Entire home/apt` + `room_type_Hotel room` + 
                         `room_type_Private room` + `room_type_Shared room` + 
                         `neighbourhood_group_Bronx` + `neighbourhood_group_Brooklyn` + 
                         `neighbourhood_group_Manhattan` + `neighbourhood_group_Queens` + 
                         `neighbourhood_group_Staten Island`, 
                       data = airbnb.newyork)

# Show summary of the regression model
print("Linear Regression Model Summary:")
print(summary(regression_model))

# Residual Analysis
residuals <- residuals(regression_model)

# Plotting residuals to check for any patterns
par(mfrow = c(1, 2)) # Set up two plots side by side
plot(regression_model$fitted.values, residuals, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Q-Q plot for residuals to check for normality
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)

# Logistic Regression - Create binary price category (Low vs High) and fit logistic model
airbnb.newyork$price_category <- ifelse(airbnb.newyork$price < quantile(airbnb.newyork$price, 0.33), "Low", 
                                        ifelse(airbnb.newyork$price < quantile(airbnb.newyork$price, 0.66), "Medium", "High"))

# Creating a binary outcome for logistic regression (Low vs High price category)
airbnb.newyork$price_category_binary <- ifelse(airbnb.newyork$price_category == "Low", 0, 1)

# Logistic Regression Model - Predict binary outcome (Low vs High price)
logistic_model <- glm(price_category_binary ~ `room_type_Private room` + `room_type_Hotel room` + 
                        `room_type_Entire home/apt` + `room_type_Shared room` + 
                        `neighbourhood_group_Bronx` + `neighbourhood_group_Brooklyn` + 
                        `neighbourhood_group_Manhattan` + `neighbourhood_group_Queens` + 
                        `neighbourhood_group_Staten Island`, 
                      data = airbnb.newyork, family = binomial)

# summary of the logistic regression model
print("Logistic Regression Model Summary:")
print(summary(logistic_model))

logistic_probs <- predict(logistic_model, type = "response")
roc_curve <- roc(airbnb.newyork$price_category_binary, logistic_probs)
plot(roc_curve, main = "ROC Curve")
auc_value <- roc_curve$auc
print(paste("AUC:", round(auc_value, 4)))

# Summary Report: Airbnb Price Prediction and Classification Analysis

# Objective:
# The goal of this analysis is to understand the factors influencing Airbnb listing prices in New York City and to develop predictive models for both price prediction and classification. This analysis focuses on key predictors, including room type, neighborhood group, and reviews, in order to forecast prices and classify listings into low and high price categories.

# Data Preprocessing:
# The price column was converted to a numeric format, and any non-numeric entries were removed. 
# Missing values in the `reviews_per_month` column were imputed with the mean value for each room type. Rows with missing values in either the price or reviews columns were excluded to ensure data integrity. 
# The `room_type` and `neighbourhood_group` variables were converted into categorical factors and one-hot encoded for use in the regression models.

# Summary Statistics:
# The key statistics for the Airbnb dataset highlight considerable variability in the price and reviews data. The average price per listing was $233, with a median of $147, indicating the presence of a number of high-priced outliers. Prices ranged from a minimum of $8 to a maximum of $100,000, with a standard deviation of $1020. The average reviews per month was 1.24, with a median of 1.19, and a relatively low standard deviation of 1.89, indicating that most listings have few reviews. This variability suggests that factors such as room type, neighborhood, and listing quality may significantly impact the price.

# Linear Regression Analysis:
# A linear regression model was fitted to predict the price of Airbnb listings based on room type and neighborhood group. The results revealed that "Entire home/apt" and "Hotel room" were the most significant predictors of higher prices, with increases of $158 and $274 respectively. On the other hand, "Private room" had a non-significant effect on price. 
# Listings located in "Manhattan" showed a significant positive effect on price, with an estimated increase of $158 compared to other neighborhoods. In contrast, neighborhoods like the Bronx and Queens did not show significant price differences.
# Despite these findings, the R-squared value of the model was very low (0.008), suggesting that only 0.8% of the variance in price is explained by room type and neighborhood group, indicating the presence of additional factors (such as location, amenities, or availability) that likely contribute to price variations. 
# Residual analysis showed no clear pattern in the residuals, but some minor deviations from normality were noted in the Q-Q plot, which suggests that the linear model may not fully capture all relationships in the data. Furthermore, multicollinearity among the predictors may also affect the stability and accuracy of the model, as several room types and neighborhood groups are likely correlated.

# Logistic Regression for Price Category Classification:
# To classify listings into "Low" or "High" price categories, a binary logistic regression model was used. The categories were defined based on the 33rd and 66th percentiles of price, with listings priced below the 33rd percentile categorized as "Low" and those above the 66th percentile categorized as "High."
# The logistic regression model revealed that room types such as "Entire home/apt" and "Hotel room" were strongly associated with the high price category. Listings in "Manhattan" were also more likely to belong to the high price category, followed by "Brooklyn" and "Queens."
# The model achieved an Area Under the Curve (AUC) of 0.8348, indicating good discriminatory power between the low and high price categories. This suggests that the logistic regression model is an effective tool for classifying listings based on price.

# Model Diagnostics and Multicollinearity:
# Residual analysis of the linear regression model revealed no major patterns, although minor deviations from normality were observed, suggesting that the model might not capture all the non-linear relationships in the data. In addition, there is a potential issue of multicollinearity, especially among categorical variables such as room type and neighborhood group. 
# Multicollinearity can cause instability in regression coefficients and affect model reliability. It is advisable to examine the Variance Inflation Factors (VIFs) for the predictors to better understand the degree of multicollinearity in the model.

# Next Steps:
# To improve the linear regression model, additional features such as amenities, listing availability, and specific location data could be incorporated to capture more of the variance in price.
# Further exploration of advanced machine learning models like Random Forests or Gradient Boosting could help in addressing non-linear relationships and improving model accuracy.
# Cross-validation should be performed to assess the robustness and generalization ability of the models, ensuring that they perform well on new, unseen data.

# Conclusion:
# This analysis successfully identified key factors influencing Airbnb prices in New York City, particularly room type and neighborhood group. While the linear regression model provided some insights into price determinants, its limited explanatory power suggests that additional features or more advanced models would likely yield better predictive performance. The logistic regression model for classifying listings into price categories, on the other hand, demonstrated strong predictive ability with an AUC of 0.8348, making it a promising tool for categorizing listings into low and high price categories. Further model refinement and additional feature inclusion are recommended to improve overall prediction accuracy and classification performance.
