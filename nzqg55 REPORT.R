 
data <- read.csv('nzqg55heart_failure.csv')

# Load necessary libraries
library(ggplot2)

# Summary of the data
summary(data)

# Simple visualizations
#  Age distribution
ggplot(data, aes(x = age)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Example: Ejection fraction vs. Serum Creatinine
ggplot(data, aes(x = ejection_fraction, y = serum_creatinine)) +
  geom_point(color = "blue") +
  labs(title = "Ejection Fraction vs. Serum Creatinine", x = "Ejection Fraction", y = "Serum Creatinine")

# Create a dataframe for the summary statistics
summary_data <- data.frame(
  variable = c("Male", "Female"),
  count = c(sum(data$sex == 1), sum(data$sex == 0))
)

# Create a column chart for the 'sex' variable
ggplot(summary_data, aes(x = variable, y = count, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Sex", x = "Sex", y = "Count") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink"))



# Load necessary libraries
library(caret)
library(randomForest)
library(gbm)
library(nnet)



# Define predictors and target variable
predictors <- c("age", "anaemia", "creatinine_phosphokinase", "diabetes", "ejection_fraction", "high_blood_pressure", "platelets", "serum_creatinine", "serum_sodium", "sex", "smoking", "time")
target <- "fatal_mi"

# Create training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data[[target]], p = 0.7, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Train Decision Trees model
decision_tree_model <- train(fatal_mi ~ ., data = train, method = "rpart")

decision_tree_model

# Train Random Forest model
random_forest_model <- randomForest(fatal_mi ~ ., data = train)

random_forest_model

# Train Gradient Boosting Machines (GBM) model
gbm_model <- train(fatal_mi ~ ., data = train, method = "gbm")

gbm_model

# Train Neural Network model
neural_network_model <- train(fatal_mi ~ ., data = train, method = "nnet")

neural_network_model

# Train Logistic Regression model

logistic_regression_model <- train(fatal_mi ~ ., data = train, method = "glm", family = binomial)

logistic_regression_model




# Load necessary libraries
library(gbm)
library(caret)
library(pROC)

# 1. Split Data
set.seed(123) # for reproducibility
train_index <- createDataPartition(data$fatal_mi, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# 2. Train Model
gbm_model <- gbm(fatal_mi ~ ., data = train_data, distribution = "bernoulli")

# 3. Predict
predictions <- predict(gbm_model, newdata = test_data, n.trees = 100, type = "response")

# 4. Evaluate Performance
# Calculate various performance metrics
performance_metrics <- confusionMatrix(table(predictions > 0.5, test_data$fatal_mi))

# Calculate AUC-ROC
roc_curve <- roc(test_data$fatal_mi, predictions)
auc_score <- auc(roc_curve)

# Calibration analysis
calibration_plot <- plot(roc_curve, legacy.axes = TRUE, smooth = TRUE)

# 5. Assess True/False Positive Rates
# Determine the optimal decision threshold based on the trade-off between false negatives and false positives

# Print performance report
print(performance_metrics)
print(auc_score)
calibration_plot

