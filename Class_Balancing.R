library(tidyverse)
library(ggplot2)
library(RANN)
library(doParallel)

# Load data
df <- read.csv("D:\\SEM_4\\DS\\Project\\cleaned_cpdataset.csv")
df$stroke <- as.factor(df$stroke)

# ADASYN Function
ADASYN <- function(data, target_col, k = 5, beta = 1) {
  data[[target_col]] <- as.factor(data[[target_col]])
  
  class_counts <- table(data[[target_col]])
  minority_class <- names(which.min(class_counts))
  majority_class <- names(which.max(class_counts))
  
  minority_data <- data[data[[target_col]] == minority_class, ]
  majority_data <- data[data[[target_col]] == majority_class, ]
  
  numeric_cols <- sapply(minority_data, is.numeric)
  minority_numeric <- minority_data[, numeric_cols]
  
  imbalance_ratio <- nrow(majority_data) / nrow(minority_data)
  G <- as.integer((imbalance_ratio - 1) * nrow(minority_data) * beta)
  
  if (G <= 0) {
    warning("No need to perform ADASYN, dataset is already balanced or beta is too small.")
    return(data)
  }
  
  knn_indices <- nn2(minority_numeric, k = k)$nn.idx
  
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  synthetic_samples <- foreach(i = 1:nrow(minority_data), .combine = rbind, .packages = c("RANN", "dplyr")) %dopar% {
    samples <- list()
    for (n in 1:floor(G / nrow(minority_data))) {
      neighbor_index <- sample(1:k, 1)
      neighbor <- minority_numeric[knn_indices[i, neighbor_index], ]
      original <- minority_numeric[i, ]
      
      diff <- neighbor - original
      gap <- runif(1)
      noise <- rnorm(length(diff), mean = 0, sd = 0.01)
      synth_numeric <- original + gap * diff + noise
      
      non_numeric_data <- minority_data[i, !numeric_cols, drop = FALSE]
      for (col in names(non_numeric_data)) {
        col_values <- minority_data[[col]]
        col_table <- table(col_values)
        
        if (length(col_table) == 1) {
          non_numeric_data[[col]] <- col_values[1]
        } else {
          non_numeric_data[[col]] <- sample(names(col_table), 1, prob = col_table / sum(col_table))
        }
      }
      
      synth_row <- cbind(synth_numeric, non_numeric_data)
      synth_row[[target_col]] <- minority_class
      
      samples[[length(samples) + 1]] <- synth_row
    }
    do.call(rbind, samples)
  }
  
  stopCluster(cl)
  
  synthetic_data <- bind_rows(synthetic_samples)
  balanced_data <- rbind(data, synthetic_data)
  balanced_data[[target_col]] <- as.factor(balanced_data[[target_col]])
  
  return(balanced_data)
}

# Apply ADASYN
balanced_df <- ADASYN(df, "stroke", k = 5, beta = 0.5)

# Save balanced dataset
write.csv(balanced_df, "D:\\SEM_4\\DS\\Project\\balanced_cpdataset.csv", row.names = FALSE)

# Plot BEFORE balancing
ggplot(df, aes(x = factor(stroke))) +
  geom_bar(width = 0.5, fill = "#FF6666") +
  labs(
    title = "Class Distribution Before Modified ADASYN Balancing",
    x = "Stroke Class (0 = No Stroke, 1 = Stroke)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot AFTER balancing
ggplot(balanced_df, aes(x = factor(stroke))) +
  geom_bar(width = 0.5, fill = "#66CC99") +
  labs(
    title = "Class Distribution After Modified ADASYN Balancing",
    x = "Stroke Class (0 = No Stroke, 1 = Stroke)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
