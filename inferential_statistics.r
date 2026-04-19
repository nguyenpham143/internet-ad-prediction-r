# inferential_statistics.r

rm(list = ls())
options(stringsAsFactors = FALSE)

cat("Current working directory:\n")
print(getwd())

cat("\nFiles in working directory:\n")
print(list.files())

required_packages <- c("pROC")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

default_input <- "preprocessed.csv"

if (file.exists(default_input)) {
  input_file <- normalizePath(default_input)
  cat("\nUsing file found in working directory:\n")
  print(input_file)
} else {
  cat("\npreprocessed.csv not found in working directory.\n")
  cat("Please choose the input file manually...\n")
  input_file <- file.choose()
  input_file <- normalizePath(input_file)
}

data <- read.csv(input_file, stringsAsFactors = FALSE)

cat("\nData imported successfully.\n")
cat("Input file:", input_file, "\n")
cat("Number of observations:", nrow(data), "\n")
cat("Number of variables:", ncol(data), "\n")

# Output folder = same folder as input file
output_dir <- dirname(input_file)
cat("Output directory:", output_dir, "\n")

required_cols <- c("Height", "Width", "Aspect_ratio", "Local", "Is_Ad")
missing_cols <- setdiff(required_cols, names(data))

if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

data$Is_Ad <- as.numeric(data$Is_Ad)
data$Local <- as.numeric(data$Local)
data$Height <- as.numeric(data$Height)
data$Width <- as.numeric(data$Width)
data$Aspect_ratio <- as.numeric(data$Aspect_ratio)

# Remove missing values if any remain
data <- na.omit(data)

cat("\nAfter removing NA values:\n")
cat("Number of observations:", nrow(data), "\n")

cat("\nClass distribution of Is_Ad:\n")
print(table(data$Is_Ad))

model <- glm(
  Is_Ad ~ Height + Width + Aspect_ratio + Local,
  data = data,
  family = binomial(link = "logit")
)

cat("\n===== LOGISTIC REGRESSION SUMMARY =====\n")
print(summary(model))

summary_file <- file.path(output_dir, "logistic_regression_summary.txt")
capture.output(summary(model), file = summary_file)

null_model <- glm(Is_Ad ~ 1, data = data, family = binomial(link = "logit"))
lrt <- anova(null_model, model, test = "Chisq")

cat("\n===== LIKELIHOOD RATIO TEST =====\n")
print(lrt)

lrt_file <- file.path(output_dir, "likelihood_ratio_test.txt")
capture.output(lrt, file = lrt_file)

coef_table <- summary(model)$coefficients
odds_ratio <- exp(coef(model))
conf_int <- exp(confint(model))

results_table <- data.frame(
  Variable = rownames(coef_table),
  Estimate = coef(model),
  Odds_Ratio = odds_ratio,
  CI_Lower = conf_int[, 1],
  CI_Upper = conf_int[, 2],
  Std_Error = coef_table[, 2],
  z_value = coef_table[, 3],
  p_value = coef_table[, 4],
  row.names = NULL
)

cat("\n===== ODDS RATIO TABLE =====\n")
print(results_table)

results_file <- file.path(output_dir, "logistic_regression_results.csv")
write.csv(results_table, results_file, row.names = FALSE)

data$pred_prob <- predict(model, type = "response")
data$pred_class <- ifelse(data$pred_prob > 0.5, 1, 0)

TP <- sum(data$pred_class == 1 & data$Is_Ad == 1)
TN <- sum(data$pred_class == 0 & data$Is_Ad == 0)
FP <- sum(data$pred_class == 1 & data$Is_Ad == 0)
FN <- sum(data$pred_class == 0 & data$Is_Ad == 1)

conf_matrix <- matrix(
  c(TP, FN, FP, TN),
  nrow = 2,
  byrow = TRUE
)

rownames(conf_matrix) <- c("Actual Ad (1)", "Actual Nonad (0)")
colnames(conf_matrix) <- c("Predicted Ad (1)", "Predicted Nonad (0)")

cat("\n===== CONFUSION MATRIX =====\n")
print(conf_matrix)

conf_matrix_file <- file.path(output_dir, "confusion_matrix.txt")
capture.output(conf_matrix, file = conf_matrix_file)

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
f1_score <- ifelse(
  is.na(precision) | is.na(recall) | (precision + recall) == 0,
  NA,
  2 * precision * recall / (precision + recall)
)
balanced_accuracy <- (recall + specificity) / 2

metrics <- data.frame(
  Metric = c(
    "Accuracy",
    "Precision",
    "Recall",
    "Specificity",
    "F1-score",
    "Balanced Accuracy"
  ),
  Value = c(
    accuracy,
    precision,
    recall,
    specificity,
    f1_score,
    balanced_accuracy
  )
)

cat("\n===== EVALUATION METRICS =====\n")
print(metrics)

metrics_file <- file.path(output_dir, "classification_metrics.csv")
write.csv(metrics, metrics_file, row.names = FALSE)

roc_obj <- roc(data$Is_Ad, data$pred_prob, quiet = TRUE)
auc_value <- auc(roc_obj)

cat("\n===== ROC / AUC =====\n")
cat("AUC =", as.numeric(auc_value), "\n")

roc_file <- file.path(output_dir, "roc_curve.png")
png(roc_file, width = 800, height = 600)
plot(
  roc_obj,
  main = paste(
    "ROC Curve for Logistic Regression (AUC =",
    round(as.numeric(auc_value), 4),
    ")"
  )
)
abline(a = 0, b = 1, lty = 2)
dev.off()

pred_file <- file.path(output_dir, "predicted_results.csv")
write.csv(data, pred_file, row.names = FALSE)

cat("\nAnalysis completed successfully.\n")
cat("\nOutput files created in:\n")
cat(output_dir, "\n\n")
cat("- ", basename(summary_file), "\n", sep = "")
cat("- ", basename(lrt_file), "\n", sep = "")
cat("- ", basename(results_file), "\n", sep = "")
cat("- ", basename(conf_matrix_file), "\n", sep = "")
cat("- ", basename(metrics_file), "\n", sep = "")
cat("- ", basename(roc_file), "\n", sep = "")
cat("- ", basename(pred_file), "\n", sep = "")