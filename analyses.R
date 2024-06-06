#Cleanup
rm(list = ls())
gc()

# Load Functions
source("functions.R")

#Read the participant information data
info_data <- read_excel(info_path)

# Clean participant information data

info_data <- as.data.frame(info_data)

info_data <- info_data %>%
  clean_names()

info_data<- info_data %>% rename(VPN = vpn)


str(info_data)

info_data_clean <- info_data[info_data$group != "NF/stress" & info_data$group != "sham/stress", ]


head(info_data)
head(info_data_clean)

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

# Add group info o ETQ data
ETQ_data <- ETQ_data %>%
  left_join(info_data %>% select(VPN, "group"), by = "VPN")

ETQ_data$group
            
# remove stress trials

ETQ_data_clean <- ETQ_data[ETQ_data$group != "NF/stress" & ETQ_data$group != "sham/stress", ]


# Analyze ETQ data for overall score
ETQ_data_clean<- analyze_etq_data(ETQ_data_clean)


# visualize the overall ETQ scores
visualize_overall_scores(ETQ_data_clean)

#check for group differnces

mean_accuracy_by_condition_ETQ <- ETQ_data_clean %>%
  group_by(group) %>% summarize(mean_overall = mean(overall_score, na.rm = TRUE))


t_test_result_ETQ<- t.test(overall_score ~ group, data = ETQ_data_clean)


#Read and clean WPT Data

WPT_file_list <- list.files(WPT_path, pattern = "*.csv", full.names = TRUE)

WPT_data_list <- lapply(WPT_file_list, load_WPT_file)

WPT_data <- bind_rows(WPT_data_list)

WPT_data <- clean_WPT_data(WPT_data)

WPT_data <- WPT_data %>%
  rename(VPN = n)

WPT_data$VPN <- sapply(WPT_data$VPN, format_vpn)

# remove trials without correct answer or control
WPT_data$trialNumber <- WPT_data %>%
  filter(correctness != -1)

# Remove particopants in stress condition
WPT_data <- WPT_data %>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")

WPT_data <- WPT_data %>%
  left_join(info_data_clean %>% select(VPN, group), by = "VPN") %>%
  filter(!is.na(group))

table(WPT_data$group)

# Calculate overall accuracy

WPT_accuracy_VPN <- WPT_data %>%
  group_by(VPN) %>%
  summarise(accuracy = mean(correctness))


mean(WPT_accuracy_VPN$accuracy)

# add condition to WPT accuracy 
WPT_accuracy <- WPT_accuracy%>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")


mean_accuracy_by_condition_WPT <- WPT_accuracy %>%
  group_by(group) %>% summarize(mean_accuracy = mean(accuracy, na.rm = TRUE))


# test for differences
t_test_result_WPT<- t.test(accuracy ~ group, data = WPT_accuracy)

print(t_test_result_WPT)


# pe block

WPT_accuracy_blocks <- WPT_data %>%
  group_by(blockNumber) %>%
  summarise(accuracy = mean(correctness))

# Visualize the data
ggplot(WPT_accuracy_blocks, aes(x = factor(blockNumber), y = accuracy)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) +
  labs(title = "Mean accuracy across Blocks", x = "Block Number", y = "Accuracy") +
  theme_minimal()


# Remove or handle rows with missing values in stimulusPattern or correctOutcome
WPT_data <- WPT_data %>%
  filter(!is.na(stimulusPattern) & !is.na(correctOutcome)) %>%
  mutate(stimulusPattern = gsub("\\s+", " ", trimws(stimulusPattern))) # Ensure consistent spacing and remove leading/trailing spaces

# Inspect the cleaned data
head(WPT_data$stimulusPattern)

table(WPT_data$stimulus)

# Calculate Strategies used

singleton_patterns <- list(
  c(1, 0, 0, 0), # Only cue 1 is present
  c(0, 1, 0, 0), # Only cue 2 is present
  c(0, 0, 1, 0), # Only cue 3 is present
  c(0, 0, 0, 1)  # Only cue 4 is present
)

source("WPT_strategy_analysis.R")



