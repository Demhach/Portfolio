# Install the packages if you haven't already
install.packages("ggplot2")
install.packages("factoextra")
install.packages("dplyr")

# Load the necessary libraries
library(stats)      # Usually pre-installed
library(ggplot2)
library(factoextra)
library(dplyr)

# Load the CSV file
data <- read.csv("Coded_data_round1.csv")

# Preview the data to understand its structure
head(data)
str(data)
# Assume the grouping variable is "Group". Replace with the actual column name
grouping_variable <- data$Group

# Keep only numeric columns
numeric_data <- data[, sapply(data, is.numeric)]
# Perform PCA on the numeric data
pca_result <- prcomp(numeric_data, scale. = TRUE)


library(factoextra)

# Create a PCA plot grouped by the selected grouping variable
fviz_pca_biplot(pca_result,
                geom.ind = "point",  # Display data points
                col.ind = grouping_variable,  # Color points by group
                palette = "jco",  # Select color palette
                addEllipses = TRUE,  # Add ellipses around each group
                legend.title = "Groups")  # Set legend title
