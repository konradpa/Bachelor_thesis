install.packages("readr")
install.packages("dplyr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("readxl")


# Install and load necessary packages
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(readxl)
source("functions.R")

# Read the ETQ data from the file
ETQ_data <- read_delim(ETQ_path, delim = ";", locale = locale(encoding = "UTF-8"))

#Read the particpant information sheet
info_data <- read_excel(info_path)

# Clean the ETQ column names
ETQ_data <- ETQ_data %>%
  clean_names()

#add card order to ETQ data
ETQ_data <- ETQ_data %>%
  rename(VPN = id_antwort_id)

ETQ_data <- ETQ_data %>%
  mutate(VPN = as.character(VPN))

participant_data <- info_data %>%
  mutate(VPN = as.character(VPN))

ETQ_data <- ETQ_data %>%
  left_join(info_data %>% select(VPN, "WPT _randomCardOrderMap"), by = "VPN")


# Analyze the data using the function
ETQ_data <- analyze_etq_data(ETQ_data)

# View the data with the new overall_score column as the first column
print(colnames(ETQ_data))

mean(ETQ_data$overall_score)

# Call the function to visualize the overall scores
visualize_overall_scores(ETQ_data)

ETQ_data$id_antwort_id





