#Cleanup
rm(list = ls())
gc()

# Load Functions
source("functions.R")

#Read the participant information data
info_data <- read_excel(info_path)

# Clean participant information data

info_data <- info_data %>%
  clean_names()

info_data<- info_data %>% rename(VPN = vpn)

info_data <- info_data %>%
  mutate(vpn = as.character(VPN))

head(info_data)

# Read the ETQ data 
ETQ_data <- read_delim(ETQ_path, delim = ";", locale = locale(encoding = "UTF-8"))

# Clean the ETQ data

ETQ_data <- ETQ_data %>%
  clean_names()

ETQ_data <- ETQ_data %>%
  rename(VPN = id_antwort_id)

ETQ_data <- ETQ_data %>%
  mutate(VPN = as.character(VPN))

ETQ_data$VPN <- sapply(ETQ_data$VPN, format_vpn)

head(ETQ_data)
#add card order to ETQ data

ETQ_data <- ETQ_data %>%
  left_join(info_data %>% select(VPN, "wpt_random_card_order_map"), by = "VPN")

# Analyze ETQ data for overall score
ETQ_data<- analyze_etq_data(ETQ_data)

# add group info to ETQ accuracy data

ETQ_data <- ETQ_data %>%
  left_join(info_data %>% select(VPN, "group"), by = "VPN")

ETQ_data<- ETQ_data %>%
  mutate(simplified_condition = ifelse(grepl("NF", group), "NF", "Sham"))

# visualize the overall ETQ scores
visualize_overall_scores(ETQ_data)

#check for group differnces

mean_accuracy_by_condition_ETQ <- ETQ_data %>%
  group_by(simplified_condition) %>% summarize(mean_overall = mean(overall_score, na.rm = TRUE))


t_test_result_ETQ<- t.test(overall_score ~ simplified_condition, data = ETQ_data)

print(t_test_result_WPT)



#Read and clean WPT Data

WPT_file_list <- list.files(WPT_path, pattern = "*.csv", full.names = TRUE)

WPT_data_list <- lapply(WPT_file_list, load_WPT_file)

WPT_data <- bind_rows(WPT_data_list)

WPT_data <- clean_WPT_data(WPT_data)

WPT_data <- WPT_data %>%
  rename(VPN = n)

WPT_data$VPN <- sapply(WPT_data$VPN, format_vpn)

# remove trials without correct answer or control
WPT_data <- WPT_data %>%
  filter(correctness != -1)

# Calculate overall accuracy

WPT_accuracy <- WPT_data %>%
  group_by(VPN) %>%
  summarise(accuracy = mean(correctness))

print(WPT_accuracy)

mean(WPT_accuracy$accuracy)

# add condition to WPT accuracy 
WPT_accuracy <- WPT_accuracy%>%
  left_join(info_data %>% select(VPN, "group"), by = "VPN")

WPT_accuracy <- WPT_accuracy %>%
  mutate(simplified_condition = ifelse(grepl("NF", group), "NF", "Sham"))


mean_accuracy_by_condition_WPT <- WPT_accuracy %>%
  group_by(simplified_condition) %>% summarize(mean_accuracy = mean(accuracy, na.rm = TRUE))


# test for differences
t_test_result_WPT<- t.test(accuracy ~ simplified_condition, data = WPT_accuracy)

print(t_test_result_WPT)


# Calculate Strategies used 

WPT_data



