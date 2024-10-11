# Install necessary packages if not already installed
if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
  install.packages("fitdistrplus")
}
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(knitr)

# Load the dataset
airbnb.newyork <- read.csv("listings.csv")

# Handle missing values in reviews_per_month
airbnb.newyork <- airbnb.newyork %>%
  group_by(room_type) %>%
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 
                                    mean(reviews_per_month, na.rm = TRUE), 
                                    reviews_per_month))

# Remove rows with missing prices
airbnb.newyork <- airbnb.newyork %>% filter(!is.na(price))

# 1. Exploratory Data Analysis (EDA)

# Summary statistics
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

# Histograms
ggplot(airbnb.newyork, aes(x = price)) +
  geom_histogram(bins = 10, fill = "blue", alpha = 0.5) +
  labs(title = "Price Distribution", x = "Price", y = "Frequency") +
  theme_minimal()
ggsave("Price_Distribution_Histogram.png")

ggplot(airbnb.newyork, aes(x = reviews_per_month)) +
  geom_histogram(bins = 10, fill = "green", alpha = 0.5) +
  labs(title = "Reviews per Month Distribution", x = "Reviews per Month", y = "Frequency") +
  theme_minimal()
ggsave("Reviews_Distribution_Histogram.png")

# Boxplots
ggplot(airbnb.newyork, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Neighborhood", y = "Price", x = "Neighborhood") +
  theme_minimal()
ggsave("Price_by_Neighborhood_Boxplot.png")

ggplot(airbnb.newyork, aes(x = room_type, y = price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Room Type", y = "Price", x = "Room Type") +
  theme_minimal()
ggsave("Price_by_Room_Type_Boxplot.png")

# Scatterplots
ggplot(airbnb.newyork, aes(x = reviews_per_month, y = price, color = neighbourhood_group)) +
  geom_point(alpha = 0.5) +
  labs(title = "Price vs. Reviews per Month by Neighborhood", y = "Price", x = "Reviews per Month") +
  theme_minimal()
ggsave("Price_vs_Reviews_Scatterplot.png")

# 2. Maximum Likelihood Estimation (MLE)
# Fit a normal distribution to prices
fit <- fitdist(airbnb.newyork$price, "norm")
print(summary(fit))

# 3. Confidence Intervals
# Confidence interval for the mean price
price_ci <- t.test(airbnb.newyork$price)$conf.int
print(paste("Confidence Interval for Price: ", paste(round(price_ci, 2), collapse = " to ")))

# Confidence interval for reviews per month
reviews_ci <- t.test(airbnb.newyork$reviews_per_month)$conf.int
print(paste("Confidence Interval for Reviews per Month: ", paste(round(reviews_ci, 2), collapse = " to ")))

# 4. Hypothesis Testing
# ANOVA for price by room type
anova_results <- aov(price ~ room_type, data = airbnb.newyork)
print(summary(anova_results))

# Post-hoc testing (if ANOVA is significant)
post_hoc <- TukeyHSD(anova_results)
print(post_hoc)

# 5. Further Analysis
# Linear regression model
lm_model <- lm(price ~ reviews_per_month + room_type + neighbourhood_group, data = airbnb.newyork)
summary(lm_model)

# Visualize the linear model
ggplot(airbnb.newyork, aes(x = reviews_per_month, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Linear Regression of Price on Reviews per Month", y = "Price", x = "Reviews per Month") +
  theme_minimal()
ggsave("Linear_Regression_Plot.png")

# Report generation
report <- data.frame(
  Statistic = c("Mean Price", "Median Price", "Min Price", "Max Price", "SD Price", 
                "Mean Reviews", "Median Reviews", "Min Reviews", "Max Reviews", "SD Reviews"),
  Value = c(summary_stats$Mean_Price, summary_stats$Median_Price, summary_stats$Min_Price, 
            summary_stats$Max_Price, summary_stats$SD_Price, summary_stats$Mean_Reviews, 
            summary_stats$Median_Reviews, summary_stats$Min_Reviews, summary_stats$Max_Reviews, 
            summary_stats$SD_Reviews)
)

# Save the report
write.csv(report, "EDA_Summary_Statistics.csv", row.names = FALSE)
print("Report saved as EDA_Summary_Statistics.csv")

#Comprehensive Report on Airbnb New York Listings Analysis
#This report provides a detailed analysis of Airbnb listings in New York City, focusing on price dynamics and factors influencing these prices. Using a dataset containing various attributes of listings, we performed exploratory data analysis (EDA), maximum likelihood estimation (MLE), hypothesis testing, and linear regression modeling.

#The exploratory data analysis revealed significant insights into the distribution of prices and reviews per month. Summary statistics indicated that the average price for listings varies notably by room type. For instance, entire homes/ apartments had a mean price of approximately $243, while shared rooms averaged around $91. The distribution of prices showed a right skew, indicating a higher concentration of lower-priced listings, with a notable presence of outliers in the upper price range. The mean reviews per month were generally low, reflecting the high variability in user engagement across listings.

#To visualize these distributions, histograms and boxplots were created. The histograms illustrated the frequency of listings at various price points and reviews per month, while boxplots highlighted the significant differences in price across different neighborhoods and room types. The analysis showed that prices significantly differ based on room type, with entire homes commanding the highest prices.

#Maximum likelihood estimation was employed to fit a normal distribution to the price data, yielding a mean of approximately $188.68 and a standard deviation of $136.70. This fitting process suggested that prices tend to cluster around the mean but also confirmed the presence of significant variability.

#Confidence intervals for the mean price and reviews per month were calculated, providing a range in which the true population parameters are likely to fall. The 95% confidence interval for the mean price ranged from $186.91 to $190.45, indicating a stable estimate with a narrow range, while the interval for reviews per month suggested limited variation.

#To test the hypothesis that room types have different impacts on price, an ANOVA was conducted, yielding statistically significant results (p < 2e-16). This analysis was complemented by post-hoc testing using Tukey's method, which confirmed significant price differences between various room types. For example, the analysis indicated that hotel rooms were, on average, approximately $71 more expensive than entire homes, while shared rooms were significantly cheaper.

#A linear regression model was subsequently constructed to examine the influence of multiple factors, including reviews per month, room type, and neighborhood group on price. The model explained approximately 30% of the variance in price, with significant predictors identified. Notably, each additional review per month increased the average price by about $24.405, while different room types and neighborhoods also showed significant effects on pricing.

#In conclusion, the analysis of Airbnb listings in New York City highlights substantial variations in pricing influenced by room type and neighborhood. The statistical models employed provided insights into how different factors contribute to pricing, thereby assisting hosts and potential guests in understanding the market dynamics. Future research may explore additional variables or non-linear models to further enhance predictions and insights into the Airbnb market
