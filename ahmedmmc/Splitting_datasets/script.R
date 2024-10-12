library(readxl)
library(writexl)
library(dplyr)
library(janitor)

df <- read_xlsx("MMC_WANA_Migrant_Master_Clean_Dataset_27-09-2023.xlsx", sheet =1 , col_names = FALSE)
merged <- read_xlsx("merged.xlsx", sheet =1 , col_names = FALSE)

titles <- df[1:2,]
colnames(titles) <- titles[1,]

colnames(df) <- df [1,]

df_1 <- df[-c(1,2),]

colnames(merged) <- merged[2,]
merged <- merged[-c(1,2),]

all_merged <- rbind(df_1,merged)

# Condition to filter rows where 'country' starts with 'tun', 'mar', or 'lib'
condition <- grepl("^(tun|mar|lib)", all_merged$username, ignore.case = TRUE)


# Dataset 1: rows that match the condition
df_na <- all_merged %>%
  filter(condition)%>%
  arrange(today)

excel_numeric_to_date(as.numeric(df_na[[nrow(df_na),4]]))


write_xlsx(df_na, "MMC_clean_master_dataset_NA_2024-01-20.xlsx")

# Dataset 2: rows that do not match the condition
df_wa <- all_merged %>%
  filter(!condition)%>%
  arrange(today)
  
df_wa <- df_wa[1:(nrow(df_wa)-4),]

df_na <- rbind(titles[2,],df_na)

write_xlsx(df_wa, "MMC_clean_master_dataset_WA_2023-12-19.xlsx")
