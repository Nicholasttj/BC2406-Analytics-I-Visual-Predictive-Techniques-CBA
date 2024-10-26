install.packages("ggplot2")
install.packages("data.table")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")

library(ggplot2)
library(data.table)
library(caret)  # For splitting the data and model evaluation
library(rpart)  # For CART
library(rpart.plot)  # For plotting decision trees

# Load the dataset (adjust the file path accordingly)
data <- fread("C:/Users/Nicholas/OneDrive/NTU/2.1/BC2406/AY24 CBA/INF002v4.csv")

# Check the structure of the data
str(data)

# Using Length.of.Stay as the target variable
target_var <- "Length.of.Stay"
predictor_vars <- c("APR.Severity.of.Illness.Code", "Age.Group", "Gender", "Race", "Type.of.Admission")

# Remove any rows with missing values
data <- na.omit(data)

# Split the dataset into a 70-30 train-test split
set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(data[[target_var]], p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# ---- (a) Linear Regression ----
# Specify the formula for the linear regression model
formula_lm <- as.formula(paste(target_var, "~", paste(predictor_vars, collapse = " + ")))

# Train the linear regression model
lm_model <- lm(formula_lm, data = train_data)

# Predict on the test set
lm_predictions <- predict(lm_model, newdata = test_data)

# Calculate RMSE for Linear Regression
lm_rmse <- sqrt(mean((lm_predictions - test_data[[target_var]])^2))


# (b) CART Model

# Train a CART model
cart_model <- rpart(formula_lm, data = train_data, method = "anova")

# Predict on the test set using CART
cart_predictions <- predict(cart_model, newdata = test_data)

# Calculate RMSE for CART
cart_rmse <- sqrt(mean((cart_predictions - test_data[[target_var]])^2))

# Number of terminal nodes in the CART model
num_terminal_nodes <- length(unique(cart_model$where))

# Results Output
results <- data.frame(
  Model = c("Linear Regression", "CART"),
  `Model Complexity` = c(paste(length(predictor_vars), "X variables"), 
                         paste(num_terminal_nodes, "terminal nodes")),
  `Testset RMSE` = c(lm_rmse, cart_rmse)
)

print(results)

# Displaying the CART model
rpart.plot(cart_model, main = "CART Model - Length of Stay Prediction")

