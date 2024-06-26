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


## **t test**  Explicit task knowledge
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
WPT_accuracy_VPN <- WPT_accuracy_VPN %>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")


mean_accuracy_by_condition_WPT <- WPT_accuracy_VPN %>%
  group_by(group) %>% summarize(mean_accuracy = mean(accuracy, na.rm = TRUE))

# test for differences - **t-test**  WPT performance with groups
t_test_result_WPT<- t.test(accuracy ~ group, data = WPT_accuracy_VPN)

print(t_test_result_WPT)


## **check for correlation** with ETQ scores

WPT_accuracy_VPN <- WPT_accuracy_VPN %>%
  left_join(ETQ_data_clean %>% select(VPN, "overall_score"), by = "VPN")

correlation_result <- cor.test(WPT_accuracy_VPN$accuracy, WPT_accuracy_VPN$overall_score)

ggplot(WPT_accuracy_VPN, aes(x = overall_score, y = accuracy)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "Scatter Plot of Accuracy vs. Overall Score",
       x = "Overall Score",
       y = "Accuracy") +
  theme_minimal()

# per block

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

# clean WPT data columns

WPT_data_filtered <- WPT_data %>%
  select(VPN, trialNumber, blockNumber, response, stimulusPattern, correctOutcome, correctness, group)


# add WPT order

info_data_clean_subset <- info_data_clean %>%
  select(VPN, wpt_random_card_order_map)

WPT_data_filtered <- WPT_data_filtered %>%
  left_join(info_data_clean_subset, by = "VPN")

table(WPT_data_filtered$wpt_random_card_order_map)

WPT_data_filtered$stimulusPattern

# Calculate Strategies used
source("WPT_strategy_analysis.R")


# Add strategies 
WPT_data_filtered <- WPT_data_filtered %>%
  mutate(strategy_multicue = correctOutcome,
         strategy_singleton = sapply(stimulusPattern, determine_strategy_singleton),
         one_cue_good = sapply(stimulusPattern, determine_one_cue_good),
         one_cue_bad = sapply(stimulusPattern, determine_one_cue_bad))


matching_percentage <- WPT_data_filtered %>%
  summarise(percentage = mean(one_cue_bad == correctOutcome, na.rm = TRUE) * 100)

# Display the result

head(WPT_data_filtered)



# show stretegy mactch
percentage_matches <- calculate_percentage_match(WPT_data_filtered)
print(percentage_matches)

## normalized scores for particpants
strategy_scores_VPN <- WPT_data_filtered %>%
  group_by(VPN) %>%
  do(calculate_normalized_scores(.))

## normalized scores for particpants + blocks
strategy_scores_blocks <- WPT_data_filtered %>%
  group_by(VPN, blockNumber) %>%
  do(calculate_normalized_scores(.))

print(strategy_scores_VPN)
print(strategy_scores_blocks)

str(strategy_scores_VPN)
str(strategy_scores_blocks)

strategy_results <- find_best_strategies(strategy_scores_blocks, strategy_scores_VPN)

# add group info

strategy_results <- strategy_results %>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")

print(strategy_results)

## check for group differnces in straetegy use **chi square test**

# Define the columns to test
strategy_columns <- c("best_strategy_block_1", "best_strategy_block_2", "best_strategy_block_3", "best_strategy_block_4", "best_strategy_overall")

# Function to perform chi-square test and return results
perform_chi_square_test <- function(data, column, group_column) {
  contingency_table <- table(data[[column]], data[[group_column]])
  chisq_test <- chisq.test(contingency_table)
  return(list(column = column, chi_square_test = chisq_test))
}

# Perform chi-square test for each strategy column and store results
chi_square_results <- lapply(strategy_columns, perform_chi_square_test, data = strategy_results, group_column = "group")

# Print the results
for (result in chi_square_results) {
  cat("Results for column:", result$column, "\n")
  print(result$chi_square_test)
  cat("\n")
}

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)

# Print the result
print(chi_square_test)

# Plotting

# Reshape the data to long format for easier plotting
# Assuming your dataframe `final_results` has the correct column names
strategy_results_long <- strategy_results %>%
  pivot_longer(cols = starts_with("best_strategy_block"), names_to = "block", values_to = "strategy") %>%
  mutate(block = factor(block, levels = c("best_strategy_block_1", "best_strategy_block_2", "best_strategy_block_3", "best_strategy_block_4", "best_strategy_overall"),
                        labels = c("Block 1", "Block 2", "Block 3", "Block 4", "Overall")))

# Create bar plots for each block and overall
ggplot(strategy_results_long, aes(x = strategy, fill = group)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ block, scales = "free") +
  labs(title = "Number of Participants Using Each Strategy", x = "Strategy", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Use the overall strategy column for the bar plot
strategy_results_long_overall <- strategy_results_long %>%
  select(VPN, best_strategy_overall, group)

# Create the bar plot for the overall strategy
ggplot(strategy_results_long_overall, aes(x = best_strategy_overall, fill = group)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Participants Using Each Strategy (Overall)", x = "Strategy", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

