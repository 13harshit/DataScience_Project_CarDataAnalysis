# Save all console output
sink("project_output.txt")
cat("Project output logging started...\n")
# ============================================
# Car Data Analysis Project (Beginner Version)
# ============================================

# 1. Load required libraries
# (Install once if not already installed)

# install.packages(c( "ggplot2", "corrplot","dplyr" ))
library(ggplot2)
library(dplyr)
library(corrplot)

cat("Libraries loaded successfully!\n\n")

# 2. Load dataset (mtcars is built-in in R)
data <- mtcars
cat("Dataset loaded successfully!\n")

# Add car names as a column
data$car_name <- rownames(data)
rownames(data) <- NULL

# 3. Basic data exploration
cat("\nDataset dimensions:\n")
print(dim(data))

cat("\nFirst 6 rows of data:\n")
head(data)

cat("\nSummary statistics:\n")
summary(data)

# 4. Data cleaning
# Convert categorical variables into factors
data$cyl  <- factor(data$cyl)
data$am   <- factor(data$am, labels = c("Automatic", "Manual"))
data$gear <- factor(data$gear)

cat("\nData cleaning completed!\n")

# 5. Data Visualization

# 5.1 MPG Distribution
ggplot(data, aes(x = mpg)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Mileage (MPG)",
       x = "Miles Per Gallon",
       y = "Number of Cars") +
  theme_minimal()

# 5.2 MPG vs Horsepower
ggplot(data, aes(x = hp, y = mpg)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Horsepower vs Mileage",
       x = "Horsepower",
       y = "MPG") +
  theme_minimal()

# 5.3 MPG by Transmission Type
ggplot(data, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() +
  labs(title = "Mileage Comparison: Manual vs Automatic",
       x = "Transmission Type",
       y = "MPG") +
  theme_minimal()

# 6. Correlation Analysis
numeric_data <- data %>% select_if(is.numeric)

cor_matrix <- cor(numeric_data)

cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 2))

# Correlation heatmap
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         title = "Correlation Heatmap of Car Features")

# 7. Simple Predictive Model (Linear Regression)

cat("\nBuilding Linear Regression Model...\n")

model <- lm(mpg ~ wt + hp, data = data)
summary(model)

# Add predicted MPG
data$predicted_mpg <- predict(model)

cat("\nActual vs Predicted MPG (first 6 rows):\n")
head(data[, c("car_name", "mpg", "predicted_mpg")])

# 8. Key Insights
cat("\n=== KEY INSIGHTS ===\n")
cat("1. Mileage decreases as weight and horsepower increase.\n")
cat("2. Manual cars generally give better mileage.\n")
cat("3. Weight is the strongest predictor of MPG.\n")

cat("\n=== PROJECT COMPLETED SUCCESSFULLY ===\n")

sink()
