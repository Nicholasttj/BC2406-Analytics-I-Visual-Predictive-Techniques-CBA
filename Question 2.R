install.packages("readr")
install.packages("dplyr")
library(dplyr)    # For data manipulation
library(readr)    # For reading CSV files

# Step 1: Load the dataset
data <- read_csv("C:/Users/nicho/OneDrive/NTU/2.1/BC2406/AY24 CBA/INF002v4.csv")

# Step 2: Check for missing values and clean the data
missing_data <- sapply(data, function(x) sum(is.na(x)))
print(missing_data)

# Step 3: Remove rows with missing values (or handle them with imputation)
cleaned_data <- na.omit(data)

# Step 4: Identify columns with only one unique value (constant columns)
constant_columns <- sapply(cleaned_data, function(x) length(unique(x)) == 1)

# Remove constant columns
cleaned_data <- cleaned_data[, !constant_columns]

# Step 5: Perform one-hot encoding for categorical variables
# Using model.matrix (base R method)
encoded_data <- model.matrix(~ . - 1, data = cleaned_data)

# Step 6: Convert the result to a data frame
encoded_data <- as.data.frame(encoded_data)

# Step 7: Question 2 Part 1, Potential Predictor X Variables
predictor_variables <- colnames(encoded_data)
print(predictor_variables)

# Step 8: Question 2 Part 2, Check final dimensions of the dataset
final_observations <- nrow(encoded_data)
final_variables <- ncol(encoded_data)

# Print final dimensions
cat("Final number of observations: ", final_observations, "\n")
cat("Final number of predictor variables: ", final_variables, "\n")





