# Data cleaning pipeline for the Internet Advertisements assignment.
# This script reads the raw add.csv file, keeps the variables used in the
# report, imputes missing values, recalculates Aspect_ratio, and exports
# preprocessed.csv to the same folder as this script.

get_script_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)

    if (length(file_arg) > 0) {
        return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
    }

    normalizePath(getwd())
}

script_dir <- get_script_dir()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)

input_candidates <- c(
    file.path(script_dir, "add.csv"),
    file.path(project_root, "add.csv"),
    file.path(project_root, "251-Probability-and-Statistics-project-main", "add.csv")
)

existing_inputs <- input_candidates[file.exists(input_candidates)]
if (length(existing_inputs) == 0) {
    stop("Cannot find add.csv. Checked: ", paste(input_candidates, collapse = ", "))
}

input_file <- existing_inputs[1]
output_file <- file.path(script_dir, "preprocessed.csv")

raw_data <- read.csv(
    file = input_file,
    header = TRUE,
    sep = ",",
    check.names = FALSE,
    stringsAsFactors = FALSE
)

if (ncol(raw_data) != 1560L) {
    stop("Unexpected number of columns in raw data: ", ncol(raw_data), ". Expected 1560.")
}

data <- raw_data[, -c(1, 6:1559)]
colnames(data) <- c("Height", "Width", "Aspect_ratio", "Local", "Is_Ad")

data <- as.data.frame(lapply(data, function(col) {
    ifelse(grepl("\\?", col), NA, col)
}), stringsAsFactors = FALSE)

data$Is_Ad <- ifelse(data$Is_Ad == "ad.", 1, 0)
data <- as.data.frame(lapply(data, as.numeric))

summary_before <- summary(data)
na_count <- sapply(data, function(col) sum(is.na(col)))
na_percentage <- round(na_count / nrow(data) * 100, 2)

for (col in c("Height", "Width")) {
    data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}

data$Local[is.na(data$Local)] <- round(mean(data$Local, na.rm = TRUE))
data$Aspect_ratio <- round(data$Width / data$Height, 4)

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

cat("\nSummary after imputation:\n")
print(summary(data))

cat("\nRemaining missing values after cleaning:", sum(is.na(data)), "\n")
