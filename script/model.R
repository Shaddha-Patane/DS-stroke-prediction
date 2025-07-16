
library(ranger)
library(pROC)
library(caret)

balanced_data <- read.csv("data/balanced_cpdataset.csv")

categorical_cols <- c("stroke", "gender", "hypertension", "heart_disease", 
                      "ever_married", "work_type", "Residence_type", "smoking_status")
balanced_data[categorical_cols] <- lapply(balanced_data[categorical_cols], as.factor)

high_cardinality_cols <- names(balanced_data)[sapply(balanced_data, function(x) is.factor(x) && length(levels(x)) > 53)]
if (length(high_cardinality_cols) > 0) {
  balanced_data <- balanced_data[ , !(names(balanced_data) %in% high_cardinality_cols)]
}

target <- "stroke"

set.seed(42)
train_indices <- sample(1:nrow(balanced_data), 0.8 * nrow(balanced_data))
train_data <- balanced_data[train_indices, ]
test_data <- balanced_data[-train_indices, ]

for (col in categorical_cols) {
  if (col %in% colnames(test_data)) {
    test_data[[col]] <- factor(test_data[[col]], levels = levels(train_data[[col]]))
  }
}

test_data <- test_data[, colnames(train_data), drop = FALSE]

modified_rf <- function(train_data, test_data, target) {
  formula <- as.formula(paste(target, "~ ."))

  init_model <- ranger(formula, data = train_data, num.trees = 50, importance = "impurity")
  imp_scores <- importance(init_model)

  selected_features <- names(imp_scores[imp_scores > (0.1 * max(imp_scores))])
  selected_features <- unique(c(selected_features, target))
 
  train_data <- train_data[, selected_features, drop = FALSE]
  test_data  <- test_data[, selected_features, drop = FALSE]

  model <- ranger(formula, data = train_data, num.trees = 150, 
                  mtry = floor(sqrt(ncol(train_data) - 1)), 
                  importance = "impurity", probability = TRUE)
  
  return(list(model = model, selected_features = selected_features, train_data = train_data, test_data = test_data))
}

result <- modified_rf(train_data, test_data, target)

test_data_final <- test_data[, result$selected_features, drop = FALSE]

preds_train_prob <- predict(result$model, result$train_data)$predictions[, "1"]
preds_train_class <- ifelse(preds_train_prob > 0.5, "1", "0")

preds_test_prob  <- predict(result$model, test_data_final)$predictions[, "1"]
preds_test_class <- ifelse(preds_test_prob > 0.5, "1", "0")

roc_train <- roc(response = result$train_data[[target]], predictor = preds_train_prob, levels = c("0", "1"), direction = "<")
roc_test  <- roc(response = result$test_data[[target]], predictor = preds_test_prob, levels = c("0", "1"), direction = "<")
auc_train <- auc(roc_train)
auc_test  <- auc(roc_test)

plot(roc_train, col = "orange", lwd = 2,
     main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Positive Rate", 
     ylab = "True Positive Rate",
     legacy.axes = TRUE)
plot(roc_test, col = "navy", lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")

legend("bottomright", legend = c(
  paste0("Train AUC: ", round(auc_train, 2)),
  paste0("Test AUC: ", round(auc_test, 2))
), col = c("orange", "navy"), lwd = 2)

cm_test <- confusionMatrix(factor(preds_test_class, levels = c("0", "1")), result$test_data[[target]])

TP <- cm_test$table[2, 2]
FP <- cm_test$table[1, 2]
FN <- cm_test$table[2, 1]
accuracy  <- cm_test$overall["Accuracy"]
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nTest Accuracy :", round(accuracy * 100, 2), "%")
cat("\nPrecision     :", round(precision * 100, 2), "%")
cat("\nRecall        :", round(recall * 100, 2), "%\n") 
cat("\nF1-Score      :", round(f1_score * 100, 2), "%\n")
