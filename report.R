# 1. Install and load necessary packages
packages <- c("tidyverse", "caret", "randomForest", "pROC", "GGally")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(caret)
library(randomForest)
library(pROC)
library(GGally)

# Set random seed to ensure reproducibility
set.seed(123)

# 2. Data loading and preprocessing
df <- read.csv("https://raw.githubusercontent.com/rwkg23/math/refs/heads/main/heart_failure.csv")

# Check the structure of the data (ensure all variable types are correct)
str(df)

# Check for missing values
if(sum(is.na(df)) > 0) {
  df <- na.omit(df)
}

# Convert certain variables to factors (e.g., sex, smoking, fatal_mi)
df$sex <- as.factor(df$sex)
df$smoking <- as.factor(df$smoking)
df$fatal_mi <- as.factor(df$fatal_mi)

# Standardize numeric variables (except time, which can be included if needed)
numeric_columns <- c("age", "creatinine_phosphokinase", "ejection_fraction", 
                     "platelets", "serum_creatinine", "serum_sodium", "time")
df[numeric_columns] <- scale(df[numeric_columns])

# 3. Data exploration and visualization

# 3.1 View summary statistics of the data
summary(df)

# 3.2 Plot histograms for numeric variables (using age as an example, can extend to others)
p1 <- ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Age Distribution Histogram", x = "Age (Standardized)", y = "Frequency")
print(p1)

# 3.3 Plot boxplot: Compare the distribution of age in different fatal_mi groups
p2 <- ggplot(df, aes(x = fatal_mi, y = age, fill = fatal_mi)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Fatal Myocardial Infarction Groups", x = "Fatal Myocardial Infarction (0=No, 1=Yes)", y = "Age (Standardized)") +
  theme(legend.position = "none")
print(p2)

# 3.4 Plot correlation heatmap for variables
# Select numeric data for correlation calculation
numeric_df <- df %>% 
  select(all_of(numeric_columns))
cor_matrix <- cor(numeric_df)
# Use GGally's ggcorr function to display the correlation
p3 <- ggcorr(numeric_df, label = TRUE, label_size = 3, label_round = 2, hjust = 0.75) +
  labs(title = "Correlation Heatmap for Numeric Variables")
print(p3)

# 3.5 Plot scatterplot matrix to observe relationships between variables (select some variables)
p4 <- ggpairs(df, columns = numeric_columns[1:4],
              mapping = aes(color = fatal_mi),
              title = "Scatterplot Matrix for First 4 Numeric Variables")
print(p4)

# 4. Split the data into training and test sets
trainIndex <- createDataPartition(df$fatal_mi, p = 0.8, list = FALSE)
train_df <- df[trainIndex, ]
test_df <- df[-trainIndex, ]

# 5. Model training (Random Forest)
rf_model <- randomForest(fatal_mi ~ ., data = train_df, ntree = 100)
print(rf_model)

# 6. Model evaluation
# 6.1 Make predictions using the test set
predictions <- predict(rf_model, test_df)

# 6.2 Confusion matrix
conf_matrix <- confusionMatrix(predictions, test_df$fatal_mi)
print(conf_matrix)

# 6.3 Calculate and plot ROC curve (convert factor to numeric: 1 for positive class)
test_labels <- as.numeric(as.character(test_df$fatal_mi))
# Random forest's prediction by default returns the class
rf_prob <- predict(rf_model, test_df, type = "prob")[,2]
roc_curve <- roc(test_labels, rf_prob)
plot(roc_curve, main = "ROC Curve")
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# 6.4 Output key performance metrics for the model
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

# 7. Save the model
saveRDS(rf_model, "random_forest_model.rds")

