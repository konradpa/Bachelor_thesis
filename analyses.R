# Cleanup
rm(list = ls())
gc()

# Load Functions
source("functions.R")

# Read the participant information data
info_data <- read_excel(info_path)

# Clean participant information data
info_data <- info_data %>%
  clean_names() %>%
  rename(VPN = vpn) %>%
  as.data.frame()

# Filter out specific groups
info_data_clean <- info_data %>%
  filter(group != "NF/stress" & group != "sham/stress")

# analyze info data
# age distribution
mean(info_data_clean$age)

# gender distribution
gender_counts <- table(info_data_clean$m_f)

gender_percentages <- prop.table(gender_counts) * 100

gender_percentages

# Read the ETQ data
ETQ_data <- read_delim(ETQ_path, delim = ";", locale = locale(encoding = "UTF-8"))

# Clean the ETQ data
ETQ_data <- ETQ_data %>%
  clean_names() %>%
  rename(VPN = id_antwort_id) %>%
  mutate(VPN = sapply(VPN, format_vpn))

# Add card order and group info to ETQ data
ETQ_data <- ETQ_data %>%
  left_join(info_data %>% select(VPN, wpt_random_card_order_map, group), by = "VPN")

# remove stress trials
ETQ_data_clean <- ETQ_data[ETQ_data$group != "NF/stress" & ETQ_data$group != "sham/stress", ]


# Analyze ETQ data for overall score
ETQ_data_clean<- analyze_etq_data(ETQ_data_clean)


# visualize the overall ETQ scores
visualize_ETQ_scores(ETQ_data_clean)

#check for group differnces

mean_accuracy_by_condition_ETQ <- ETQ_data_clean %>%
  group_by(group) %>% summarize(mean_overall = mean(overall_score, na.rm = TRUE))


## **t test**  Explicit task knowledge
t_test_result_ETQ<- t.test(overall_score ~ group, data = ETQ_data_clean)


# Read and clean WPT Data
WPT_file_list <- list.files(WPT_path, pattern = "*.csv", full.names = TRUE)

WPT_data <- lapply(WPT_file_list, load_WPT_file) %>%
  bind_rows() %>%
  clean_WPT_data() %>%
  rename(VPN = n) %>%
  mutate(VPN = sapply(VPN, format_vpn)) %>%
  filter(correctness != -1) %>%
  left_join(info_data_clean %>% select(VPN, group), by = "VPN") %>%
  filter(!is.na(group))


# Remove or handle rows with missing values in stimulusPattern or correctOutcome
WPT_data <- WPT_data %>%
  filter(!is.na(stimulusPattern) & !is.na(correctOutcome)) %>%
  mutate(stimulusPattern = gsub("\\s+", " ", trimws(stimulusPattern))) # Ensure consistent spacing and remove leading/trailing spaces


# Calculate overall accuracy

WPT_accuracy_VPN <- WPT_data %>%
  group_by(VPN) %>%
  summarise(accuracy = mean(correctness))


mean(WPT_accuracy_VPN$accuracy)


# add condition to WPT accuracy 
WPT_accuracy_VPN <- WPT_accuracy_VPN %>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")

# test for differences - **t-test**  WPT performance with groups
t_test_result_WPT_acc<- t.test(accuracy ~ group, data = WPT_accuracy_VPN)

print(t_test_result_WPT_acc)

# **check for correlation** beween ETQ scores and WPT accuracy
WPT_accuracy_VPN <- WPT_accuracy_VPN %>%
  left_join(ETQ_data_clean %>% select(VPN, "overall_score"), by = "VPN")

ETQ_WPT_acc_correlation_result <- cor.test(WPT_accuracy_VPN$accuracy, WPT_accuracy_VPN$overall_score)

ggplot(WPT_accuracy_VPN, aes(x = overall_score, y = accuracy)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "Scatter Plot of Accuracy vs. Overall Score",
       x = "Overall Score",
       y = "Accuracy") +
  theme_minimal()

# WPT accuracy per blocks

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

# **check for correlation** beween blocks and WPT accuracy
blocks_WPT_acc_correlation_result <- cor.test(WPT_accuracy_blocks$blockNumber, WPT_accuracy_blocks$accuracy)

# Calculate accuracy for each block between groups
WPT_accuracy_VPN_blocks <- WPT_data %>%
  group_by(VPN, blockNumber) %>%
  summarise(accuracy = mean(correctness)) %>%
  pivot_wider(names_from = blockNumber, values_from = accuracy, names_prefix = "block_")

WPT_accuracy_VPN_blocks <- WPT_accuracy_VPN_blocks %>%
  mutate(group = as.factor(group))

WPT_accuracy_VPN_blocks  <- WPT_accuracy_VPN_blocks  %>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")

# Perform t-tests for each block
t_test_result_WPT_acc_blocks <- WPT_accuracy_VPN_blocks %>%
  select(VPN, block_1, block_2, block_3, block_4, group) %>%
  pivot_longer(cols = starts_with("block_"), names_to = "block", values_to = "accuracy") %>%
  group_by(block) %>%
  summarise(
    t_test = list(t.test(accuracy ~ group, data = cur_data()))
  )

# Extract detailed t-test results
t_test_result_WPT_acc_blocks <- t_test_result_WPT_acc_blocks %>%
  mutate(
    t_statistic = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    conf_low = sapply(t_test, function(x) x$conf.int[1]),
    conf_high = sapply(t_test, function(x) x$conf.int[2]),
    mean_group1 = sapply(t_test, function(x) x$estimate[1]),
    mean_group2 = sapply(t_test, function(x) x$estimate[2]),
    group1 = group_levels[1],
    group2 = group_levels[2]
  )

# Print the detailed t-test results
print(t_test_result_WPT_acc_blocks)

# visualize WPT performance between groups in blocks
mean_accuracy_by_block_group <- WPT_accuracy_VPN_blocks %>%
  pivot_longer(cols = starts_with("block_"), names_to = "block", values_to = "accuracy") %>%
  group_by(block, group) %>%
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

ggplot(mean_accuracy_by_block_group, aes(x = block, y = mean_accuracy, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Accuracy by Block and Group",
       x = "Block",
       y = "Mean Accuracy") +
  theme_minimal()

# check for differnces between blocks in each group
long_WPT_data <- WPT_accuracy_VPN_blocks %>%
  pivot_longer(cols = starts_with("block_"), names_to = "block", values_to = "accuracy")

# Perform repeated measures ANOVA for each group separately
anova_results_WPT_acc_blocks <- long_WPT_data %>%
  group_by(group) %>%
  do(tidy(aov(accuracy ~ block + Error(VPN/block), data = .))) %>%
  ungroup()

# Print ANOVA results
anova_results_WPT_acc_blocks %>%
  mutate(summary = map(anova, summary)) %>%
  select(group, summary) %>%
  unnest(cols = summary)

# Perform pairwise comparisons using emmeans
pairwise_results <- long_WPT_data %>%
  group_by(group) %>%
  do(emmeans = emmeans(aov(accuracy ~ block + Error(VPN/block), data = .), pairwise ~ block))

# Print pairwise comparison results
pairwise_results %>%
  mutate(summary = map(emmeans, summary)) %>%
  select(group, summary) %>%
  unnest(cols = summary)


# cleanup WPT data columns

WPT_data_filtered <- WPT_data %>%
  select(VPN, trialNumber, blockNumber, response, stimulusPattern, correctOutcome, correctness, group)

# add WPT order

info_data_clean_subset <- info_data_clean %>%
  select(VPN, wpt_random_card_order_map)

WPT_data_filtered <- WPT_data_filtered %>%
  left_join(info_data_clean_subset, by = "VPN", copy = TRUE)


table(WPT_data_filtered$wpt_random_card_order_map)


# change stimulusPattern according to card order

source("WPT_strategy_analysis.R")

WPT_data_filtered$stimulusPattern <- as.character(WPT_data_filtered$stimulusPattern)
WPT_data_filtered$wpt_random_card_order_map <- as.character(WPT_data_filtered$wpt_random_card_order_map)


WPT_data_filtered <- WPT_data_filtered %>%
  mutate(stimulusPattern = mapply(rearrange_pattern, stimulusPattern, wpt_random_card_order_map))

# Calculate Strategies used - Sterre way

wpt_strats_settings <- list(dir_WPT_strategies = "/Users/test/Desktop/Bachelorarbeit/BA_analyses/wpt_strats_results")
wpt_strats_result <- compute_strategy(WPT_data_filtered, "001", wpt_strats_settings)






# add feedback values
library(R.matlab)

extract_values <- function(file_path) {
  data <- readMat(file_path)
  values <- NA  # Default to NA if vectNFBs.PSC is not present
  
  if (!is.null(data$vectNFBs.PSC)) {
    values <- data$vectNFBs.PSC[1, ]  # Extract all values
  }
  return(values)
}

# Define the file paths
file_paths <- sprintf("Data/NF_values_11.06/NFBs_subj_%03d.mat", 1:33)

# Initialize a list to store the extracted values
extracted_values <- list()

# Loop through the file paths and extract the required values
for (i in 1:33) {
  if (file.exists(file_paths[i])) {
    extracted_values[[i]] <- extract_values(file_paths[i])
  } else {
    extracted_values[[i]] <- NA  # Handle missing files by filling with NA
  }
}

# Determine the maximum length of extracted values
max_length <- max(sapply(extracted_values, function(x) ifelse(is.null(x) || all(is.na(x)), 0, length(x))))

# Pad shorter vectors with NA to ensure all have the same length
padded_values <- lapply(extracted_values, function(x) {
  if (is.null(x) || all(is.na(x))) {
    return(rep(NA, max_length))
  } else {
    return(c(x, rep(NA, max_length - length(x))))
  }
})

# Convert the list to a data frame
extracted_df <- do.call(rbind, padded_values)
colnames(extracted_df) <- paste0("V", 1:ncol(extracted_df))
extracted_df <- as.data.frame(extracted_df)

# Add a column for the VPN number
extracted_df$VPN <- sprintf("VP_%03d", 1:33)

# Calculate the average of the extracted values for each participant
extracted_df$average <- rowMeans(extracted_df[, 1:max_length], na.rm = TRUE)

extracted_df$average_last_5 <- apply(extracted_df[, 1:max_length], 1, function(row) {
  last_5 <- tail(na.omit(row), 5)
  if (length(last_5) < 5) {
    return(NA)
  } else {
    return(mean(last_5, na.rm = TRUE))
  }
})


# Print the filtered data frame
print(extracted_df)

## add to other data

WPT_accuracy_VPN <- WPT_accuracy_VPN %>%
  left_join(extracted_df %>% select(VPN, "average_last_5"), by = "VPN")


## test for correlations

correlation_result <- cor.test(WPT_accuracy_VPN$accuracy, WPT_accuracy_VPN$average_last_5)




