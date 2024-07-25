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

length(info_data_clean$VPN)

# get general subject info
group1_subjects <- info_data_clean$VP[info_data_clean$group == "NF/control"]
group2_subjects <- info_data_clean$VP[info_data_clean$group == "sham/control"]
length(group1_subjects)
length(group2_subjects)


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
# Check the column names after cleaning
ETQ_data <- ETQ_data %>% clean_names()

# Rename and format the VPN column
ETQ_data <- ETQ_data %>%
  rename(VPN = g01q01_what_is_the_vpn_format_xxx) %>%
  mutate(VPN = sapply(VPN, format_vpn))

unique(ETQ_data$VPN)

# Add card order and group info to ETQ data
ETQ_data <- ETQ_data %>%
  left_join(info_data %>% select(VPN, wpt_random_card_order_map, group), by = "VPN")

# remove stress trials
ETQ_data_clean <- ETQ_data %>%
  filter(group != "NF/stress" & group != "sham/stress")


# Analyze ETQ data for overall score
ETQ_data_clean<- analyze_etq_data(ETQ_data_clean)

mean(ETQ_data_clean$overall_score)

length(ETQ_data_clean$VPN)
length(info_data_clean$VPN)

# visualize the overall ETQ scores
visualize_ETQ_scores(ETQ_data_clean)

ETQ_data_clean %>%
  group_by(group) %>%
  summarize(mean = mean(overall_score, na.rm = TRUE), 
            sd = sd(overall_score, na.rm = TRUE))


#check for group differnces

mean_accuracy_by_condition_ETQ <- ETQ_data_clean %>%
  group_by(group) %>% summarize(mean_overall = mean(overall_score, na.rm = TRUE))

# visualize group differences

ggplot(mean_accuracy_by_condition_ETQ, aes(x = group, y = mean_overall, fill = group, color = group)) +
  geom_bar(stat = "identity", position = position_dodge(), size = 0.5) +  # Add outline to bars
  scale_fill_manual(values = c("white", "black"), 
                    labels = c("NF", "Control")) +  # Change the fill colors to white and black
  scale_color_manual(values = c("black", "black"), 
                     labels = c("NF", "Control"), guide = FALSE) + 
  labs(title = "Mean Overall Score by Group",
       x = "Group",
       y = "Mean Overall Score") +
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(color = "black", size = 0.5)))  # Add black outline to legend keys


## **t test**  Explicit task knowledge

# Normality of the data
shapiro.test(ETQ_data_clean$overall_score[ETQ_data_clean$group == "NF/control"])
shapiro.test(ETQ_data_clean$overall_score[ETQ_data_clean$group == "sham/control"])

# Homogeneity of variances
leveneTest(overall_score ~ group, data = ETQ_data_clean)

# t test
t_test_result_ETQ<- t.test(overall_score ~ group, data = ETQ_data_clean)


# Read and clean WPT Data
WPT_file_list <- list.files(WPT_path, pattern = "*.csv", full.names = TRUE)

WPT_data <- lapply(WPT_file_list, load_WPT_file) %>%
  bind_rows() %>%
  clean_WPT_data() %>%
  rename(VPN = n) %>%
  mutate(VPN = sapply(VPN, format_vpn)) %>%
  left_join(info_data_clean %>% select(VPN, group), by = "VPN") %>%
  filter(!is.na(group))

# Calculate overall accuracy

WPT_accuracy_VPN <- WPT_data %>%
  group_by(VPN) %>%
  summarise(accuracy = mean(correctness))

length(WPT_accuracy_VPN$VPN)

mean(WPT_accuracy_VPN$accuracy)


# add condition to WPT accuracy 
WPT_accuracy_VPN <- WPT_accuracy_VPN %>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")

# test for differences - **t-test**  WPT performance with groups
# Normality of the data
shapiro.test(WPT_accuracy_VPN$accuracy[WPT_accuracy_VPN$group == "NF/control"])
shapiro.test(WPT_accuracy_VPN$accuracy[WPT_accuracy_VPN$group == "sham/control"])

# Homogeneity of variances
leveneTest(accuracy ~ group, data = WPT_accuracy_VPN)

# t test
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

str(WPT_accuracy_blocks)

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

WPT_accuracy_VPN_blocks  <- WPT_accuracy_VPN_blocks  %>%
  left_join(info_data_clean %>% select(VPN, "group"), by = "VPN")

str(WPT_accuracy_VPN_blocks)


WPT_accuracy_VPN_blocks <- WPT_accuracy_VPN_blocks %>%
  mutate(group = as.factor(group))


# Perform t-tests for each block
t_test_result_WPT_acc_blocks <- WPT_accuracy_VPN_blocks %>%
  select(VPN, block_1, block_2, block_3, block_4, group) %>%
  pivot_longer(cols = starts_with("block_"), names_to = "block", values_to = "accuracy") %>%
  group_by(block) %>%
  summarise(
    t_test = list(t.test(accuracy ~ group, data = cur_data()))
  ) %>%
  mutate(
    p_value = map_dbl(t_test, ~ .x$p.value),
    t_statistic = map_dbl(t_test, ~ .x$statistic),
    df = map_dbl(t_test, ~ .x$parameter),
    mean_diff = map_dbl(t_test, ~ .x$estimate[1] - .x$estimate[2])
  ) %>%
  select(block, p_value, t_statistic, df, mean_diff)

print(t_test_result_WPT_acc_blocks)

# visualize WPT performance between groups in blocks
mean_accuracy_by_block_group <- WPT_accuracy_VPN_blocks %>%
  pivot_longer(cols = starts_with("block_"), names_to = "block", values_to = "accuracy") %>%
  group_by(block, group) %>%
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

ggplot(mean_accuracy_by_block_group, aes(x = block, y = mean_accuracy, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black", size = 0.5) +  # Add outline to bars
  scale_fill_manual(values = c("white", "black"), 
                    labels = c("NF", "Control")) +  # Change the fill colors to white and black
  scale_color_manual(values = c("black", "black"), 
                     labels = c("NF", "Control"), guide = FALSE) +  
  scale_x_discrete(labels = c("block_1" = "Block 1", "block_2" = "Block 2", "block_3" = "Block 3", "block_4" = "Block 4")) +  # Change the block labels
  labs(x = "Block",
       y = "Mean Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7, face = "bold"),  # Set angle to 0 and center the text horizontally
        strip.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(color = "black", size = 0.5)))  # Correctly place guides function

# check for differnces between blocks in each group
long_WPT_data <- WPT_accuracy_VPN_blocks %>%
  pivot_longer(cols = starts_with("block_"), names_to = "block", values_to = "accuracy")

# analyzte reward feedback

# Initialize a list to store the extracted values
feedback_values <- list()

# Loop through the file paths and extract the required values
for (i in 1:81) {
  if (file.exists(feedback_path[i])) {
    feedback_values[[i]] <- extract_values(feedback_path[i])
  } else {
    feedback_values[[i]] <- NA  # Handle missing files by filling with NA
  }
}

# Determine the maximum length of extracted values
max_length <- max(sapply(feedback_values, function(x) ifelse(is.null(x) || all(is.na(x)), 0, length(x))))

# Pad shorter vectors with NA to ensure all have the same length
padded_feedback_values <- lapply(feedback_values, function(x) {
  if (is.null(x) || all(is.na(x))) {
    return(rep(NA, max_length))
  } else {
    return(c(x, rep(NA, max_length - length(x))))
  }
})

# Convert the list to a data frame
feedback_values_df <- do.call(rbind, padded_feedback_values)
colnames(feedback_values_df) <- paste0("V", 1:ncol(feedback_values_df))
feedback_values_df <- as.data.frame(feedback_values_df)

# Add a column for the VPN number
feedback_values_df$VPN <- sprintf("VP_%03d", 1:81)

# Calculate the average of the extracted values for each participant
feedback_values_df$average <- rowMeans(feedback_values_df[, 1:max_length], na.rm = TRUE)

feedback_values_df$average_last_5 <- apply(feedback_values_df[, 1:max_length], 1, function(row) {
  last_5 <- tail(na.omit(row), 5)
  if (length(last_5) < 5) {
    return(NA)
  } else {
    return(mean(last_5, na.rm = TRUE))
  }
})


# Print the filtered data frame
print(feedback_values_df)

## add to other data

WPT_accuracy_VPN <- WPT_accuracy_VPN %>%
  left_join(feedback_values_df %>% select(VPN, "average_last_5"), by = "VPN")


## test for correlations

correlation_result <- cor.test(WPT_accuracy_VPN$accuracy, WPT_accuracy_VPN$average_last_5)


#t test
WPT_accuracy_VPN$group <- as.factor(WPT_accuracy_VPN$group)
WPT_accuracy_VPN$average_last_5 <- as.numeric(WPT_accuracy_VPN$average_last_5)

t_test_result_reward_group<- t.test(average_last_5 ~ group, data = na.omit(WPT_accuracy_VPN))
str(WPT_accuracy_VPN)



