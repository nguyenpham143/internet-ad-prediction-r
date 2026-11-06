# Data cleaning pipeline for the Internet Advertisements assignment.
# Usage:
#   Rscript data_cleaning.r [optional_input_csv] [optional_output_csv]

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
    file.path(project_root, "data", "add.csv"),
    file.path(script_dir, "..", "data", "add.csv"),
    file.path(script_dir, "data", "add.csv"),
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
    if (dir.exists(file.path(project_root, "data"))) {
        file.path(project_root, "data", "preprocessed.csv")
    } else {
        file.path(script_dir, "preprocessed.csv")
    }
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
