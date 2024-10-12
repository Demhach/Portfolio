# Load necessary libraries
library(readxl)
library(stringdist)
library(dplyr)

# Read the correct column names from Q_columns.xlsx
unique_columns <- read_xlsx("../Q_columns.xlsx", col_names = FALSE)

# Convert to character vector (assuming it's a single row in the xlsx file)
unique_columns <- as.character(unique_columns[1, ])

# List all .xlsx files in the directory containing the data
files <- list.files(pattern =  "\\.xlsx$", full.names = TRUE)

# Read all datasets into a list
all_data <- lapply(files, read_xlsx, sheet = 3, col_names = FALSE)

# Initialize an empty list to store cleaned data frames
cleaned_data <- list()

# Iterate over each dataset to standardize column names
for (i in seq_along(all_data)) {
  # Extract the dataset and its second row (i.e., column names)
  df <- all_data[[i]]
  columns <- as.character(df[2, ])  # Extract the second row as the column names
  
  # Perform fuzzy string matching of the current dataset's columns to unique_columns
  matched_columns <- sapply(columns, function(col) {
    # Find the closest match from the unique columns
    distances <- stringdist::stringdist(unique_columns, col, method = "lv")
    best_match <- unique_columns[which.min(distances)]
    return(best_match)  # Return the best matching column name
  })
  
  # Rename the columns of the dataset
  colnames(df) <- matched_columns
  
  # Remove the first two rows (which were initially column headers) from the data
  df <- df[-c(1, 2), ]
  
  # Add any missing columns (from unique_columns) and fill with NA
  missing_columns <- setdiff(unique_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }
  
  # Reorder the columns based on the order in unique_columns
  df <- df[, unique_columns]
  
  # Add the cleaned dataset to the list
  cleaned_data[[i]] <- df
}

# Merge all cleaned data frames into one
merged_data <- bind_rows(cleaned_data)

# View the merged dataset
head(merged_data)


write_xlsx(merged_data,"MMC_WANA_Migrant_Master_Clean_Dataset_Q3_2024.xlsx")


