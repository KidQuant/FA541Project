
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(knitr)

airbnb.newyork <- read.csv("listings.csv")

airbnb.newyork <- airbnb.newyork %>%
  group_by(room_type) %>%
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 
                                    mean(reviews_per_month, na.rm = TRUE), 
                                    reviews_per_month))

airbnb.newyork <- airbnb.newyork %>% filter(!is.na(price))

################################################
# Change Price variable to Numeric
################################################



#################################################
# After Data Cleaning
#################################################

# Convert price to numeric so this line of code will work
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

# Convert price to numeric so this line of code will work
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

# Convert price to numeric so this line of code will work
ggplot(airbnb.newyork, aes(x = neighbourhood_group, y = price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Neighborhood", y = "Price", x = "Neighborhood") +
  theme_minimal()
ggsave("Price_by_Neighborhood_Boxplot.png")

# Convert price to numeric so this line of code will work
ggplot(airbnb.newyork, aes(x = room_type, y = price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Room Type", y = "Price", x = "Room Type") +
  theme_minimal()
ggsave("Price_by_Room_Type_Boxplot.png")

# Convert price to numeric so this line of code will work
ggplot(airbnb.newyork, aes(x = reviews_per_month, y = price, color = neighbourhood_group)) +
  geom_point(alpha = 0.5) +
  labs(title = "Price vs. Reviews per Month by Neighborhood", y = "Price", x = "Reviews per Month") +
  theme_minimal()
ggsave("Price_vs_Reviews_Scatterplot.png")

# Convert price to numeric so this line of code will work
fit <- fitdist(airbnb.newyork$price, "norm")
print(summary(fit))

# Convert price to numeric so this line of code will work
price_ci <- t.test(airbnb.newyork$price)$conf.int
print(paste("Confidence Interval for Price: ", paste(round(price_ci, 2), collapse = " to ")))


reviews_ci <- t.test(airbnb.newyork$reviews_per_month)$conf.int
print(paste("Confidence Interval for Reviews per Month: ", paste(round(reviews_ci, 2), collapse = " to ")))

# Convert price to numeric so this line of code will work
anova_results <- aov(price ~ room_type, data = airbnb.newyork)
print(summary(anova_results))


post_hoc <- TukeyHSD(anova_results)
print(post_hoc)

# Convert price to numeric so this line of code will work
lm_model <- lm(price ~ reviews_per_month + room_type + neighbourhood_group, data = airbnb.newyork)
summary(lm_model)

# Convert price to numeric so this line of code will work
ggplot(airbnb.newyork, aes(x = reviews_per_month, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Linear Regression of Price on Reviews per Month", y = "Price", x = "Reviews per Month") +
  theme_minimal()
ggsave("Linear_Regression_Plot.png")

# Convert price to numeric so this line of code will work
report <- data.frame(
  Statistic = c("Mean Price", "Median Price", "Min Price", "Max Price", "SD Price", 
                "Mean Reviews", "Median Reviews", "Min Reviews", "Max Reviews", "SD Reviews"),
  Value = c(summary_stats$Mean_Price, summary_stats$Median_Price, summary_stats$Min_Price, 
            summary_stats$Max_Price, summary_stats$SD_Price, summary_stats$Mean_Reviews, 
            summary_stats$Median_Reviews, summary_stats$Min_Reviews, summary_stats$Max_Reviews, 
            summary_stats$SD_Reviews)
)


write.csv(report, "EDA_Summary_Statistics.csv", row.names = FALSE)
print("Report saved as EDA_Summary_Statistics.csv")

