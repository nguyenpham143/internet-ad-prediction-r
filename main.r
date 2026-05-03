# Main execution file for the group project
# Instruction: Ensure all R scripts and the add.csv file are in the same directory

# Clear the environment (workspace) to prevent conflicts with old variables
rm(list = ls())

# (Optional) Set the working directory to the current folder if RStudio doesn't detect it automatically
# setwd("C:/path_to_your_project_folder")

print("Starting the project pipeline...")

# Step 1: Data Cleaning
print("1. Running data_cleaning.r ...")

decode_script_path <- function(path) {
    # Some Rscript invocations encode spaces as "~+~" inside --file=.
    gsub("~\\+~", " ", path)
}

get_script_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)

    if (length(file_arg) > 0) {
        script_path <- decode_script_path(sub("^--file=", "", file_arg[1]))
        if (file.exists(script_path)) {
            return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
        }
    }

    frame_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(...) NULL)
    if (!is.null(frame_path) && file.exists(frame_path)) {
        return(dirname(normalizePath(frame_path, winslash = "/", mustWork = TRUE)))
    }

    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

coerce_numeric_checked <- function(column, column_name) {
    original_na <- is.na(column)
    numeric_column <- suppressWarnings(as.numeric(column))
    introduced_na <- is.na(numeric_column) & !original_na

    if (any(introduced_na)) {
        bad_values <- unique(column[introduced_na])
        stop(
            "Unexpected non-numeric values found in ",
            column_name,
            ": ",
            paste(bad_values, collapse = ", ")
        )
    }

    numeric_column
}

standardize_character_column <- function(column) {
    if (!is.character(column)) {
        return(column)
    }

    trimmed_column <- trimws(column)
    trimmed_column[trimmed_column == "?"] <- NA
    trimmed_column
}

print_summary_table <- function(data_frame) {
    summary_object <- summary(data_frame)
    print(summary_object)
}

script_dir <- get_script_dir()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
input_override <- if (length(cli_args) >= 1) cli_args[1] else NULL
output_override <- if (length(cli_args) >= 2) cli_args[2] else NULL

if (length(cli_args) > 2) {
    warning("Ignoring extra command-line arguments after the output path.")
}

input_candidates <- c(
    if (!is.null(input_override)) input_override,
    file.path(script_dir, "add.csv"),
    file.path(project_root, "add.csv")
)
input_candidates <- unique(input_candidates)

existing_inputs <- input_candidates[file.exists(input_candidates)]
if (length(existing_inputs) == 0) {
    stop("Cannot find add.csv. Checked: ", paste(input_candidates, collapse = ", "))
}

input_file <- normalizePath(existing_inputs[1], winslash = "/", mustWork = TRUE)
output_file <- if (is.null(output_override)) {
    file.path(script_dir, "preprocessed.csv")
} else {
    output_override
}
output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)

raw_data <- read.csv(
    file = input_file,
    header = TRUE,
    sep = ",",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = "?"
)

expected_column_count <- 1560L
if (ncol(raw_data) != expected_column_count) {
    stop(
        "Unexpected number of columns in raw data: ",
        ncol(raw_data),
        ". Expected ",
        expected_column_count,
        "."
    )
}

selected_column_indices <- c(2L, 3L, 4L, 5L, 1560L)
data <- raw_data[, selected_column_indices]
colnames(data) <- c("Height", "Width", "Aspect_ratio", "Local", "Is_Ad")
data[] <- lapply(data, standardize_character_column)

valid_labels <- c("ad.", "nonad.")
observed_labels <- sort(unique(na.omit(data$Is_Ad)))
if (!all(observed_labels %in% valid_labels)) {
    stop(
        "Unexpected target labels found in Is_Ad: ",
        paste(observed_labels, collapse = ", ")
    )
}

data$Is_Ad <- ifelse(data$Is_Ad == "ad.", 1, 0)

for (col in c("Height", "Width", "Aspect_ratio", "Local")) {
    data[[col]] <- coerce_numeric_checked(data[[col]], col)
}
data$Is_Ad <- as.numeric(data$Is_Ad)

summary_before <- summary(data)
na_count <- sapply(data, function(col) sum(is.na(col)))
na_percentage <- round(na_count / nrow(data) * 100, 2)

height_median <- median(data$Height, na.rm = TRUE)
width_median <- median(data$Width, na.rm = TRUE)
local_mode_proxy <- round(mean(data$Local, na.rm = TRUE))

data$Height[is.na(data$Height)] <- height_median
data$Width[is.na(data$Width)] <- width_median
data$Local[is.na(data$Local)] <- local_mode_proxy
data$Aspect_ratio <- round(data$Width / data$Height, 4)

if (any(data$Height <= 0, na.rm = TRUE)) {
    stop("Height must remain strictly positive after cleaning.")
}

if (anyNA(data)) {
    stop("The cleaned dataset still contains missing values.")
}

write.csv(data, output_file, row.names = FALSE)

cat("Input file:", input_file, "\n")
cat("Output file:", output_file, "\n")
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n\n")

cat("Summary before imputation:\n")
print(summary_before)

cat("\nMissing values before imputation:\n")
print(data.frame(
    Variable = names(na_count),
    Missing_Count = as.integer(na_count),
    Percentage = na_percentage,
    row.names = NULL
))

cat("\nImputation values used:\n")
print(data.frame(
    Variable = c("Height", "Width", "Local"),
    Replacement = c(height_median, width_median, local_mode_proxy),
    row.names = NULL
))

cat("\nSummary after imputation:\n")
print_summary_table(data)

cat("\nRemaining missing values after cleaning:", sum(is.na(data)), "\n")

# Step 2: Descriptive Statistics (charts, mean, median, etc.)
print("2. Running descriptive_statistics.r ...")
library(ggplot2)
library(dplyr)

df <- read.csv("preprocessed.csv", stringsAsFactors = FALSE)

dim(df)
names(df)
str(df)
summary(df)
colSums(is.na(df))

class_freq <- table(df$Is_Ad)
class_percent <- prop.table(class_freq) * 100

class_summary <- data.frame(
  Is_Ad = names(class_freq),
  Frequency = as.vector(class_freq),
  Percentage = round(as.vector(class_percent), 2)
)

print(class_summary)

p_class <- ggplot(df, aes(x = factor(Is_Ad))) +
  geom_bar() +
  labs(
    title = "Distribution of Advertisement Class",
    x = "Is_Ad (0 = Non-Ad, 1 = Ad)",
    y = "Frequency"
  )

print(p_class)

numeric_summary <- data.frame(
  Variable = c("Height", "Width", "Aspect_ratio"),
  Mean = c(mean(df$Height), mean(df$Width), mean(df$Aspect_ratio)),
  Median = c(median(df$Height), median(df$Width), median(df$Aspect_ratio)),
  SD = c(sd(df$Height), sd(df$Width), sd(df$Aspect_ratio)),
  Min = c(min(df$Height), min(df$Width), min(df$Aspect_ratio)),
  Q1 = c(
    as.numeric(quantile(df$Height, 0.25)),
    as.numeric(quantile(df$Width, 0.25)),
    as.numeric(quantile(df$Aspect_ratio, 0.25))
  ),
  Q3 = c(
    as.numeric(quantile(df$Height, 0.75)),
    as.numeric(quantile(df$Width, 0.75)),
    as.numeric(quantile(df$Aspect_ratio, 0.75))
  ),
  Max = c(max(df$Height), max(df$Width), max(df$Aspect_ratio))
)

print(numeric_summary)

p_height_hist <- ggplot(df, aes(x = Height)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Height", x = "Height", y = "Frequency")

p_width_hist <- ggplot(df, aes(x = Width)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Width", x = "Width", y = "Frequency")

p_aspect_hist <- ggplot(df, aes(x = Aspect_ratio)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Aspect Ratio", x = "Aspect Ratio", y = "Frequency")

print(p_height_hist)
print(p_width_hist)
print(p_aspect_hist)

p_height_box <- ggplot(df, aes(y = Height)) +
  geom_boxplot() +
  labs(title = "Boxplot of Height", y = "Height")

p_width_box <- ggplot(df, aes(y = Width)) +
  geom_boxplot() +
  labs(title = "Boxplot of Width", y = "Width")

p_aspect_box <- ggplot(df, aes(y = Aspect_ratio)) +
  geom_boxplot() +
  labs(title = "Boxplot of Aspect Ratio", y = "Aspect Ratio")

print(p_height_box)
print(p_width_box)
print(p_aspect_box)

local_freq <- table(df$Local)
local_percent <- prop.table(local_freq) * 100

local_summary <- data.frame(
  Local = c("0", "1"),
  Frequency = as.vector(local_freq),
  Percentage = round(as.vector(local_percent), 2)
)

print(local_summary)

p_local <- ggplot(df, aes(x = factor(Local))) +
  geom_bar() +
  labs(
    title = "Distribution of Local Variable",
    x = "Local",
    y = "Frequency"
  )

print(p_local)

grouped_summary <- df %>%
  group_by(Is_Ad) %>%
  summarise(
    Mean_Height = mean(Height),
    Mean_Width = mean(Width),
    Mean_Aspect = mean(Aspect_ratio),
    Mean_Local = mean(Local),
    .groups = "drop"
  )

print(grouped_summary)

p_height_class <- ggplot(df, aes(x = factor(Is_Ad), y = Height)) +
  geom_boxplot() +
  labs(title = "Height by Advertisement Class", x = "Is_Ad", y = "Height")

p_width_class <- ggplot(df, aes(x = factor(Is_Ad), y = Width)) +
  geom_boxplot() +
  labs(title = "Width by Advertisement Class", x = "Is_Ad", y = "Width")

p_aspect_class <- ggplot(df, aes(x = factor(Is_Ad), y = Aspect_ratio)) +
  geom_boxplot() +
  labs(title = "Aspect Ratio by Advertisement Class", x = "Is_Ad", y = "Aspect Ratio")

p_local_class <- ggplot(df, aes(x = factor(Is_Ad), fill = factor(Local))) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Local by Advertisement Class",
    x = "Is_Ad",
    y = "Proportion",
    fill = "Local"
  )

print(p_height_class)
print(p_width_class)
print(p_aspect_class)
print(p_local_class)

write.csv(class_summary, "class_summary.csv", row.names = FALSE)
write.csv(numeric_summary, "numeric_summary.csv", row.names = FALSE)
write.csv(local_summary, "local_summary.csv", row.names = FALSE)
write.csv(grouped_summary, "grouped_summary.csv", row.names = FALSE)

ggsave("fig_class_distribution.png", plot = p_class, width = 7, height = 5)
ggsave("fig_height_histogram.png", plot = p_height_hist, width = 7, height = 5)
ggsave("fig_width_histogram.png", plot = p_width_hist, width = 7, height = 5)
ggsave("fig_aspect_ratio_histogram.png", plot = p_aspect_hist, width = 7, height = 5)

ggsave("fig_height_boxplot.png", plot = p_height_box, width = 7, height = 5)
ggsave("fig_width_boxplot.png", plot = p_width_box, width = 7, height = 5)
ggsave("fig_aspect_ratio_boxplot.png", plot = p_aspect_box, width = 7, height = 5)

ggsave("fig_local_distribution.png", plot = p_local, width = 7, height = 5)

ggsave("fig_height_by_class.png", plot = p_height_class, width = 7, height = 5)
ggsave("fig_width_by_class.png", plot = p_width_class, width = 7, height = 5)
ggsave("fig_aspect_ratio_by_class.png", plot = p_aspect_class, width = 7, height = 5)
ggsave("fig_local_by_class.png", plot = p_local_class, width = 7, height = 5)

# Step 3: Inferential Statistics (hypothesis testing, modeling)
print("3. Running inferential_statistics.r ...")
library(caTools)
library(caret)
library(pROC)
library(car)

set.seed(1)

ads_data <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",")

train_index <- sample.split(ads_data$Is_Ad, SplitRatio = 0.8)

train_set <- subset(ads_data, train_index)
test_set  <- subset(ads_data, !train_index)

# Logistic regression models
mdel1 <- glm(Is_Ad ~ Height + Width,
             data = train_set,
             family = binomial)

mdel2 <- glm(Is_Ad ~ Height + Width + Aspect_ratio,
             data = train_set,
             family = binomial)

mdel3 <- glm(Is_Ad ~ Height + Width + Aspect_ratio + Local,
             data = train_set,
             family = binomial)

summary(mdel1)
summary(mdel2)
summary(mdel3)

# Compare nested models
anova(mdel1, mdel2, test = "Chisq")
anova(mdel2, mdel3, test = "Chisq")

# Select the second model
best_model <- mdel2

# Check multicollinearity
library(car)
vif(best_model)

# Check contribution of each variable
drop1(best_model, test = "Chisq")

# 95% confidence intervals
confint(best_model)

# Odds ratios
exp(coef(best_model))

# Predicted probabilities on test set
pred_prob <- predict(best_model, newdata = test_set, type = "response")

library(caret)

predicted <- factor(ifelse(pred_prob > 0.5, 1, 0), levels = c(0, 1))
actual <- factor(test_set$Is_Ad, levels = c(0, 1))

# Confusion matrix
conf_mat <- confusionMatrix(predicted, actual, positive = "1")
print(conf_mat)

# ROC and AUC
roc_curve <- roc(test_set$Is_Ad, pred_prob)

plot(roc_curve, 
     main = "ROC Curve", 
     col = "blue", 
     print.auc = TRUE)

auc(roc_curve)

print("Project execution completed successfully!")

