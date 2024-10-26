install.packages("dplyr")
install.packages("readr")
install.packages("mice")

library(dplyr)    # For data manipulation
library(readr)    # For reading CSV files
library(mice)     # For missing data imputation

data <- read_csv("C:/Users/Nicholas/OneDrive/NTU/2.1/BC2406/AY24 CBA/INF002v4.csv")

# Checking for missing values
missing_data <- sapply(data, function(x) sum(is.na(x)))
print(missing_data)

# Step 3: Impute missing values instead of dropping rows
# Using mice package for simple imputation
# Ensure APR.Severity.of.Illness.Code, Age.Group, Gender, Race, Type.of.Admission are not imputed improperly
# You can impute only numerical or categorical columns selectively

# Step 3A: For numeric columns, impute using mean or median if missing
numeric_cols <- names(data)[sapply(data, is.numeric)]
for (col in numeric_cols) {
  data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}

# Step 3B: For categorical columns, impute with the mode (most frequent category)
categorical_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
for (col in categorical_cols) {
  mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
  data[[col]][is.na(data[[col]])] <- mode_val
}

# Step 4: Identify and remove columns with only one unique value (constant columns)
constant_columns <- sapply(data, function(x) length(unique(x)) == 1)
cleaned_data <- data[, !constant_columns]

# Step 5: Perform one-hot encoding for categorical variables
# One-hot encode without creating too many unnecessary variables
encoded_data <- model.matrix(~ . - 1, data = cleaned_data)

# Step 6: Convert the result to a data frame
encoded_data <- as.data.frame(encoded_data)

# Step 7: Question 2 Part 1, Identify Potential Predictor X Variables
predictor_variables <- colnames(encoded_data)
print(predictor_variables)

# Step 8: Question 2 Part 2, Check final dimensions of the dataset
final_observations <- nrow(encoded_data)
final_variables <- ncol(encoded_data)

# Print final dimensions
cat("Final number of observations: ", final_observations, "\n")
cat("Final number of predictor variables: ", final_variables, "\n")