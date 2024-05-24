install.packages("readr")
install.packages("dplyr")
install.packages("janitor")
install.packages("ggplot2")


# Install and load necessary packages
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
source("functions.R")

# Read the data from the file
ETQ_data <- read_delim(ETQ_path, delim = ";", locale = locale(encoding = "UTF-8"))

# Clean the column names
ETQ_data <- ETQ_data %>%
  clean_names()

# Analyze the data using the function
ETQ_data <- analyze_etq_data(ETQ_data)

# View the data with the new overall_score column as the first column
print(head(ETQ_data))

mean(ETQ_data$overall_score)

# Call the function to visualize the overall scores
visualize_overall_scores(ETQ_data)



