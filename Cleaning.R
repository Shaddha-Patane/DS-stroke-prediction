# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(DataExplorer)

# Load dataset
dataset <- read.csv("D:\\SEM_4\\DS\\Project\\cpdataset.csv")

# Data Cleaning
## Remove duplicate rows
dataset <- dataset %>% distinct()

## Convert categorical variables to factors
categorical_cols <- c("stroke", "gender", "hypertension", "heart_disease", "ever_married", "work_type", "Residence_type", "smoking_status")
dataset[categorical_cols] <- lapply(dataset[categorical_cols], as.factor)

## Handling missing values
# Check missing values
missing_values <- colSums(is.na(dataset))
print(missing_values)

barplot(missing_values, main="Missing Values in Each Column", col="red", las=2)

## Handle missing values in smoking_status (impute "Unknown")
dataset$smoking_status <- ifelse(is.na(dataset$smoking_status), "Unknown", dataset$smoking_status)
dataset$smoking_status <- as.factor(dataset$smoking_status)

# Impute missing values for numerical columns with median
dataset$bmi[is.na(dataset$bmi)] <- median(dataset$bmi, na.rm = TRUE)

# Remove rows with missing values in categorical columns
dataset <- dataset[complete.cases(dataset), ]

# Basic summary
summary(dataset)

# Check missing values after cleaning
plot_missing(dataset)

# Class distribution
stroke_counts <- table(dataset$stroke)
barplot(stroke_counts, main="Class Distribution of Stroke", col=c("blue", "red"), 
        names.arg=c("No Stroke", "Stroke"), ylab="Count")

# Numeric variable distributions
plot_histogram(dataset)

# Boxplots for numerical features
ggplot(dataset, aes(x=factor(stroke), y=age, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Age Distribution by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=avg_glucose_level, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Glucose Level by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=bmi, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("BMI by Stroke")

# Outlier Handling using Winsorization
winsorize <- function(data, column) {
  lower_bound <- quantile(data[[column]], 0.05, na.rm = TRUE)
  upper_bound <- quantile(data[[column]], 0.95, na.rm = TRUE)
  data[[column]] <- ifelse(data[[column]] < lower_bound, lower_bound, data[[column]])
  data[[column]] <- ifelse(data[[column]] > upper_bound, upper_bound, data[[column]])
  return(data)
}

# Apply Winsorization to numerical columns
dataset <- winsorize(dataset, "age")
dataset <- winsorize(dataset, "avg_glucose_level")
dataset <- winsorize(dataset, "bmi")


dataset <- dataset[complete.cases(dataset), ]

# Remove leading/trailing spaces in character columns
dataset <- dataset %>% mutate(across(where(is.character), str_trim))

# Standardize column names
dataset <- dataset %>% rename_all(~ gsub(" ", "_", .))

# Boxplots for numerical features
ggplot(dataset, aes(x=factor(stroke), y=age, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Age Distribution by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=avg_glucose_level, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Glucose Level by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=bmi, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("BMI by Stroke")

write.csv(dataset, "D:\\SEM_4\\DS\\Project\\cleaned_cpdataset.csv", row.names = FALSE)