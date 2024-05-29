#Cleanup
rm(list = ls())
gc()

# Load Functions
source("functions.R")

#Read the participant information data
info_data <- read_excel(info_path)

# Clean participant information data

info_data <- info_data %>%
  mutate(VPN = as.character(VPN))

# Read the ETQ data 
ETQ_data <- read_delim(ETQ_path, delim = ";", locale = locale(encoding = "UTF-8"))

# Clean the ETQ column names
ETQ_data <- ETQ_data %>%
  clean_names()

ETQ_data <- ETQ_data %>%
  rename(VPN = id_antwort_id)

ETQ_data <- ETQ_data %>%
  mutate(VPN = as.character(VPN))

#add card order to ETQ data

ETQ_data <- ETQ_data %>%
  left_join(info_data %>% select(VPN, "WPT _randomCardOrderMap"), by = "VPN")

# Analyze ETQ data for overall score
ETQ_data <- analyze_etq_data(ETQ_data)

# View the data with the new overall_score column as the first column
print(colnames(ETQ_data))

mean(ETQ_data$overall_score)


# visualize the overall ETQ scores
visualize_overall_scores(ETQ_data)




#Read and clean WPT Data

WPT_file_list <- list.files(WPT_path, pattern = "*.csv", full.names = TRUE)

WPT_data_list <- lapply(WPT_file_list, load_WPT_file)
WPT_data <- bind_rows(WPT_data_list)

WPT_data$correctness <- clean_WPT_data(WPT_data)

# remove trials without correct answwer
WPT_data <- WPT_data %>%
  filter(correctness != -1)

# Calculate overall accuracy
WPT_data <- WPT_data%>%
  mutate(correct = ifelse(correctness == 1, 1, 0))

WPT_accuracy <- WPT_data %>%
  group_by(n) %>%
  summarise(accuracy = mean(correct))

print(WPT_accuracy)



