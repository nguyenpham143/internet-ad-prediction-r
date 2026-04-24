# Main execution file for the group project
# Instruction: Ensure all R scripts and the add.csv file are in the same directory

# Clear the environment (workspace) to prevent conflicts with old variables
rm(list = ls())

# (Optional) Set the working directory to the current folder if RStudio doesn't detect it automatically
# setwd("C:/path_to_your_project_folder")

print("Starting the project pipeline...")

# Step 1: Data Cleaning
print("1. Running data_cleaning.r ...")
source("data_cleaning.r")

# Step 2: Descriptive Statistics (charts, mean, median, etc.)
print("2. Running descriptive_statistics.r ...")
source("descriptive_statistics.r")

# Step 3: Inferential Statistics (hypothesis testing, modeling)
print("3. Running inferential_statistics.r ...")
source("inferential_statistics.r")

print("Project execution completed successfully!")

