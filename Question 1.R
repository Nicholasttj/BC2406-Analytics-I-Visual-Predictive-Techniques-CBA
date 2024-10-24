install.packages("ggplot2")
install.packages("data.table")

# Load necessary library
library(ggplot2)  # for data visualization

# Load the CSV file (adjust the file path accordingly)
data <- read.csv("C:/Users/nicho/OneDrive/NTU/2.1/BC2406/AY24 CBA/INF002v4.csv")


# Create a lookup table for Severity Code to Description mapping (0-4 scale)
severity_lookup <- data.frame(
  APR.Severity.of.Illness.Code = c(0, 1, 2, 3, 4),
  APR.Severity.Description = c("Undetermined", "Minor", "Moderate", "Major", "Extreme")
)

# Merge the lookup table with the main data
data <- merge(data, severity_lookup, by = "APR.Severity.of.Illness.Code")

# Convert the 'APR.Severity.Description' to a factor with ordered levels
data$APR.Severity.Description <- factor(data$APR.Severity.Description, 
                                        levels = c("Undetermined", "Minor", "Moderate", "Major", "Extreme"))

# Handle missing values if needed, for example:
data <- na.omit(data)  # This removes rows with NA values

# Now, you can use the `APR.Severity.Description` in your analysis
los_by_severity <- aggregate(Length.of.Stay ~ APR.Severity.Description, data, mean)
print(los_by_severity)

# Boxplot visualization with the newly merged description column
ggplot(data, aes(x = APR.Severity.Description, y = Length.of.Stay, fill = APR.Severity.Description)) +
  geom_boxplot() +
  labs(title = "Length of Stay by Severity of Illness", 
       x = "Severity of Illness", 
       y = "Length of Stay (days)",
       fill = "Severity Description") +
  theme_minimal()

#----------------------------------------------------------------------------#

# Descriptive statistics for Age Group
age_group_summary <- table(data$Age.Group)
print(age_group_summary)

# Visualize the distribution of Age Group using a bar plot
ggplot(data, aes(x = factor(Age.Group))) + 
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(title = "Distribution of Patient Age Group", 
       x = "Age Group", 
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Visualize Age Group distribution by APR Severity of Illness
ggplot(data, aes(x = factor(Age.Group), fill = factor(APR.Severity.of.Illness.Code))) + 
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Age Group Distribution by Severity of Illness", 
       x = "Age Group", 
       y = "Frequency",
       fill = "Severity of Illness Code") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


#----------------------------------------------------------------------------#

## Insight 3: Length of Stay (LOS) across different Age Groups

library(ggplot2)
library(data.table)

# Load the data correctly
data <- fread("INF002v4.csv")

# Check the data structure and ensure the 'Length of Stay' and 'Age.Group' columns exist
head(data)
colnames(data)

# Check for missing values in key columns and remove rows with NAs
data <- na.omit(data[, .(Length.of.Stay, Age.Group)])  # Keep only relevant columns

# Descriptive statistics for Length of Stay by Age Group
los_by_age_group <- aggregate(Length.of.Stay ~ Age.Group, data, mean)
print(los_by_age_group)

# Boxplot visualization for Length of Stay by Age Group
p3 <- ggplot(data, aes(x = factor(Age.Group), y = Length.of.Stay, fill = Age.Group)) +
  geom_boxplot() +
  labs(title = "Length of Stay by Age Group", 
       x = "Age Group", 
       y = "Length of Stay (days)",
       fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Print the plot explicitly
print(p3)

