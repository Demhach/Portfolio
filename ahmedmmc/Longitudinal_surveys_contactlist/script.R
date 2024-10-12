library(readxl)
library(writexl)
library(dplyr)
install.packages("caret")
library(caret)
library(janitor)
library(openxlsx)

df <- read_excel("C:/Users/DRC/Documents/Projects/IM_tasks/data_cleaning/UNHCR_Morocco_ContactList/UNHCR_individual_contact_list/UNHCR_Morocco_contact_list.xlsx", sheet = 2, col_names = FALSE)

titles <- df[1:2,]

colnames(df) <- titles[2,]
colnames(titles) <- titles[2,]

df <- df[-c(1,2),]

df$`_submission_time` <- as.character(excel_numeric_to_date(as.numeric(df$`_submission_time`), include_time = TRUE))


df_priority <- df %>%
  filter(Q31 %in% c("Sudan", "Yemen", "Syria", "Guinea", "Central African Republic (CAR)"))


write_xlsx(df_priority, "priority_callback_list.xlsx")


df_backup <- df %>%
  filter(!(Q31 %in% c("Sudan", "Yemen", "Syria", "Guinea", "Central African Republic (CAR)")))


write_xlsx(df_backup, "backup_callback_list.xlsx")



# Remove specific columns
df_priority <- df_priority %>% select(-c(`_id`,L2, L5, L6, L9, L10, L13, L14, `NA`))
titles <- titles %>% select(-c(`_id`,L2, L5, L6, L9, L10, L13, L14, `NA`))



#randomization process

set.seed(123)

# Create indices for training (or sub-sample 1)
train_indices <- createDataPartition(df_priority$Q31, p = 0.5, list = FALSE)

# Split into two sub-samples based on the indices
callback1 <- df_priority[train_indices, ]
callback2 <- df_priority[-train_indices, ]

write_xlsx(callback1, "callback1.xlsx")
write_xlsx(callback2, "callback2.xlsx")



# Split the callback list by username
username_list <- callback1 %>%
  group_by(username) %>%
  group_split()



# Append the question label and name to each tibble
append_questions <- function(tibble){
  rbind(titles[2,], tibble)
}
clean_username_list <- lapply(username_list, append_questions)


#Rename the callback lists
rename_columns <- function(tibble) {
  colnames(tibble) <- titles[1, ]
  return(tibble)
}

clean_username_list <- lapply(clean_username_list, rename_columns)





library(janitor)  # You can use this package to clean column names if needed
library(openxlsx)

# Define styles
numeric_style <- createStyle(numFmt = "0")

# Loop through the list and export each tibble as a regular data range (no table formatting)
for (i in seq_along(clean_username_list)) {
  
  # Create a new workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  
  # Write the data to the workbook as a normal range (no table formatting)
  writeData(wb, "Sheet1", clean_username_list[[i]], startCol = 1, startRow = 1)
  
  # Apply numeric style to the "Code" column, if it exists
  if ("Code" %in% colnames(clean_username_list[[i]])) {
    addStyle(wb, "Sheet1", numeric_style, cols = which(colnames(clean_username_list[[i]]) == "Code"), 
             rows = 1:(nrow(username_list[[i]]) + 1), gridExpand = TRUE)
  }
  
  # Access the username from the first row of the "username" column
  username <- clean_username_list[[i]][2, "username"]
  
  # Define the filename, using `i` as the index
  file_name <- paste0(username, "_list_1.xlsx")
  
  # Save the workbook
  saveWorkbook(wb, file = file_name, overwrite = TRUE)
  
  # Print progress
  print(paste("Exported:", file_name))
}












