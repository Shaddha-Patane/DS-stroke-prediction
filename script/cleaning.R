
library(tidyverse)
library(ggplot2)
library(DataExplorer)

dataset <- read.csv("data/cpdataset.csv")

dataset <- dataset %>% distinct()

categorical_cols <- c("stroke", "gender", "hypertension", "heart_disease", "ever_married", "work_type", "Residence_type", "smoking_status")
dataset[categorical_cols] <- lapply(dataset[categorical_cols], as.factor)

missing_values <- colSums(is.na(dataset))
print(missing_values)

barplot(missing_values, main="Missing Values in Each Column", col="red", las=2)

dataset$smoking_status <- ifelse(is.na(dataset$smoking_status), "Unknown", dataset$smoking_status)
dataset$smoking_status <- as.factor(dataset$smoking_status)

dataset$bmi[is.na(dataset$bmi)] <- median(dataset$bmi, na.rm = TRUE)

dataset <- dataset[complete.cases(dataset), ]

summary(dataset)

plot_missing(dataset)

stroke_counts <- table(dataset$stroke)
barplot(stroke_counts, main="Class Distribution of Stroke", col=c("blue", "red"), 
        names.arg=c("No Stroke", "Stroke"), ylab="Count")

plot_histogram(dataset)

ggplot(dataset, aes(x=factor(stroke), y=age, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Age Distribution by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=avg_glucose_level, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Glucose Level by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=bmi, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("BMI by Stroke")

winsorize <- function(data, column) {
  lower_bound <- quantile(data[[column]], 0.05, na.rm = TRUE)
  upper_bound <- quantile(data[[column]], 0.95, na.rm = TRUE)
  data[[column]] <- ifelse(data[[column]] < lower_bound, lower_bound, data[[column]])
  data[[column]] <- ifelse(data[[column]] > upper_bound, upper_bound, data[[column]])
  return(data)
}

dataset <- winsorize(dataset, "age")
dataset <- winsorize(dataset, "avg_glucose_level")
dataset <- winsorize(dataset, "bmi")


dataset <- dataset[complete.cases(dataset), ]

dataset <- dataset %>% mutate(across(where(is.character), str_trim))

dataset <- dataset %>% rename_all(~ gsub(" ", "_", .))

ggplot(dataset, aes(x=factor(stroke), y=age, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Age Distribution by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=avg_glucose_level, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("Glucose Level by Stroke")

ggplot(dataset, aes(x=factor(stroke), y=bmi, fill=factor(stroke))) +
  geom_boxplot() + ggtitle("BMI by Stroke")

write.csv(dataset, "data/cleaned_cpdataset.csv", row.names = FALSE)
