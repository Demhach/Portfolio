library(dplyr)
library(stringdist)
library(janitor)
library(ggplot2)
library(readxl)

august <- read_xlsx("UNHCR_2024-07-26_2024-08-25 (2).xlsx" , sheet = 3, col_names = FALSE)
september<- read_xlsx("UNHCR_2024-08-26_2024-09-25 (1).xlsx" , sheet =3, col_names = FALSE)


list <- list(september, august)

titles <- august[1:2,]
colnames(titles) <- titles[1,]


# Loop through the list and modify each data frame
for (i in seq_along(list)) {
  list[[i]] <- list[[i]][-c(1,2),]  # Remove rows 1 and 2
  colnames(list[[i]]) <- titles[1,]  # Assign column names from 'titles'
}

september <- list[[1]]
august <- list[[2]]

# List to store results for each enumerator
dissimilarity_data <- list()

# Get all unique enumerators from the username column



df_tun <- rbind(august,september)


unique_enumerators <- unique(df_tun$username)

duplicated_names <- duplicated(colnames(df_tun))

colnames(df_tun)[duplicated_names]
# Loop over each enumerator
for (enumerator in unique_enumerators) {
  
  # Filter for the current enumerator and select relevant columns
  vectors <- df_tun %>% 
    filter(username == enumerator) %>%
    remove_empty(which = "cols") %>%
    as.matrix()
  
  # Skip if there are fewer than 2 rows for comparison
  if (nrow(vectors) < 2) next
  
  # Method: Simple match ratio (calculate dissimilarity)
  count_dissimilar_values <- function(row1, row2) {
    sum(row1 != row2, na.rm = TRUE)  # Compare two rows and count mismatches
  }
  
  # Get the number of rows in the filtered dataset
  n_rows <- nrow(vectors)
  
  # Initialize a matrix to store dissimilarity results
  dissimilarity_matrix <- matrix(NA, nrow = n_rows, ncol = n_rows)
  
  # Loop through all pairs of rows and calculate the number of dissimilar values
  for (i in 1:n_rows) {
    for (j in 1:n_rows) {
      dissimilarity_matrix[i, j] <- count_dissimilar_values(vectors[i, ], vectors[j, ])
    }
  }
  
  # Store dissimilarity counts (flattened into a vector) for plotting later
  dissimilarity_data[[enumerator]] <- as.vector(dissimilarity_matrix)
}

# Combine all dissimilarity data into a single dataframe for plotting
dissimilarity_df <- do.call(rbind, lapply(names(dissimilarity_data), function(enumerator) {
  data.frame(username = enumerator, dissimilarity = dissimilarity_data[[enumerator]])
}))

ggplot(dissimilarity_df, aes(x = username, y = dissimilarity)) +
  geom_boxplot() + # Add jitter points
  labs(title = "Boxplot of Dissimilarity Counts",
       x = "Enumerator (username)",
       y = "Dissimilarity Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Optional if you want flipped coordinates