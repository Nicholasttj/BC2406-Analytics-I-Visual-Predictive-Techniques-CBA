# Step 1: Load necessary libraries
library(MASS)

# Step 2: Define the target variable and predictor variables, using backticks around predictor names
target_var <- "Length.of.Stay"
all_predictors <- setdiff(names(encoded_data), target_var)

# Step 3: Modify each predictor to include backticks if needed
backticked_predictors <- sapply(all_predictors, function(var) paste0("`", var, "`"))
predictor_formula <- paste(target_var, "~", paste(backticked_predictors, collapse = " + "))

# Step 4: Fit the linear model with all predictors
full_model <- lm(as.formula(predictor_formula), data = encoded_data)

# Step 5: Display the summary, including adjusted R-squared and significant predictors based on p-values
model_summary <- summary(full_model)

# Output adjusted R-squared
cat("Adjusted R-squared:", model_summary$adj.r.squared, "\n")

# Identify significant predictors (p < 0.05)
significant_vars <- model_summary$coefficients[, 4] < 0.05
significant_predictors <- names(which(significant_vars))
cat("Significant predictors (p < 0.05):\n")
print(significant_predictors)

