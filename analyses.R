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

# Filter out stress groups and particpants 009
info_data_clean <- info_data %>%
  filter(group != "NF/stress" & group != "sham/stress") %>%
  filter(VPN != "VP_009")  # Exclude only VP_009


length(info_data_clean$VPN)

# get general subject info

mean(info_data_clean$age)
sd(info_data_clean$age)
min(info_data_clean$age)
max(info_data_clean$age)

gender_counts <- table(info_data_clean$m_f)


NF_subjects <- info_data_clean[info_data_clean$group == "NF/control", ]
sham_subjects <- info_data_clean[info_data_clean$group == "sham/control", ]

mean(NF_subjects$age)
sd(NF_subjects$age)
min(NF_subjects$age)
max(NF_subjects$age)
length(NF_subjects$VPN)
gender_counts_NF <- table(NF_subjects$m_f)


mean(sham_subjects$age)
sd(sham_subjects$age)
min(sham_subjects$age)
max(sham_subjects$age)
length(sham_subjects$VPN)


# analyze info data
# age distributionin
# gender distribution



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
  left_join(info_data %>% 
              select(VPN, wpt_random_card_order_map, group) %>%
              mutate(wpt_random_card_order_map = as.character(wpt_random_card_order_map)), 
            by = "VPN")

ETQ_data$group
ETQ_data$wpt_random_card_order_map

# remove stress trials
ETQ_data_clean <- ETQ_data %>%
  filter(group != "NF/stress" & group != "sham/stress")

ETQ_data_clean$group
ETQ_data_clean$wpt_random_card_order_map

# Analyze ETQ data for score

ETQ_data_clean<- analyze_etq_data(ETQ_data_clean)

mean(ETQ_data_clean$overall_score)

# visualize the overall ETQ scores
visualize_ETQ_scores(ETQ_data_clean)

# check for outliers

# Calculate z-scores
z_scores_ETQ <- scale(ETQ_data_clean$overall_score)

# Identify outliers (with z-score > 3 or < -3)
outliers_ETQ <- ETQ_data_clean$overall_score[abs(z_scores_ETQ) > 3]
outliers_ETQ

#check for group differnces

mean_accuracy_by_condition_ETQ <- ETQ_data_clean %>%
  group_by(group) %>%
  summarize(
    mean_overall = mean(overall_score, na.rm = TRUE),
    se = sd(overall_score, na.rm = TRUE) / sqrt(n())
  )
# visualize group differences

# Create the bar plot with error bars
ggplot(mean_accuracy_by_condition_ETQ, aes(x = group, y = mean_overall, fill = group, color = group)) +
  geom_bar(stat = "identity", position = position_dodge(), size = 0.5) +  # Add outline to bars
  geom_errorbar(aes(ymin = mean_overall - se, ymax = mean_overall + se), 
                position = position_dodge(0.9), width = 0.25, color = "red") +  # Add error bars
  scale_fill_manual(values = c("white", "black")) +  # Change the fill colors to white and black
  scale_color_manual(values = c("black", "black"), guide = FALSE) +  # Change the outline colors to black, hide the color legend
  scale_x_discrete(labels = c("NF/control" = "NF", "sham/control" = "Sham")) +  # Change the block labels
  labs(title = "ETQ Score by Group",
       x = "Group",
       y = "Mean Score") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove the legend
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),  # Adjust the text size and make it bold
        strip.text.x = element_blank())  # Remove facet labels

## **t test**  Explicit task knowledge

# Normality of the data
shapiro.test(ETQ_data_clean$overall_score[ETQ_data_clean$group == "NF/control"])
shapiro.test(ETQ_data_clean$overall_score[ETQ_data_clean$group == "sham/control"])

# Homogeneity of variances
leveneTest(overall_score ~ group, data = ETQ_data_clean)

# t test
t_test_result_ETQ<- t.test(overall_score ~ group, data = ETQ_data_clean)
mann_whitney_result_ETQ <- wilcox.test(overall_score ~ group, data = ETQ_data_clean)


# Read and clean WPT Data
WPT_file_list <- list.files(WPT_path, pattern = "*.csv", full.names = TRUE)

WPT_data <- lapply(WPT_file_list, load_WPT_file) %>%
  bind_rows() %>%
  clean_WPT_data() %>%
  rename(VPN = n) %>%
  mutate(VPN = sapply(VPN, format_vpn)) %>%
  left_join(info_data_clean %>% select(VPN, group), by = "VPN") %>%
  filter(!is.na(group)) %>%
  filter(!VPN %in% c("VP_016", "VP_009"))  # Exclude both VP_016 and VP_009

unique(WPT_data$VPN)

str(WPT_data)

# Calculate overall accuracy
WPT_accuracy_VPN <- WPT_data %>%
  group_by(VPN) %>%
  summarise(accuracy = mean(correctness))


# Calculate z-scores
z_scores_WPT_acc <- scale(WPT_accuracy_VPN$accuracy)

# Identify outliers (with z-score > 3 or < -3)
outliers_WPT_acc <- WPT_accuracy_VPN$accuracy[abs(z_scores_WPT_acc) > 3]
outliers_WPT_acc

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
  geom_smooth(method = "lm", col = "red") +  # Add regression line
  labs(title = "Plot of WPT Accuracy vs. ETQ Score",
       x = "ETQ Score",
       y = "WPT Accuracy") +
  theme_minimal()

# WPT accuracy per blocks

WPT_accuracy_blocks <- WPT_data %>%
  group_by(blockNumber) %>%
  summarise(accuracy = mean(correctness))

str(WPT_accuracy_blocks)

# Visualize the data
ggplot(WPT_accuracy_blocks, aes(x = factor(blockNumber), y = accuracy)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_point(size = 3) +
  geom_line(aes(group = 1)) +
  labs(title = "Mean accuracy across Blocks", x = "Block Number", y = " Mean Accuracy") +
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

# Ungroup the data first
WPT_accuracy_VPN_blocks <- WPT_accuracy_VPN_blocks %>% ungroup()

# Calculate Z-scores for each block using the scale() function
WPT_accuracy_VPN_blocks <- WPT_accuracy_VPN_blocks %>%
  mutate(
    z_block_1 = scale(block_1),
    z_block_2 = scale(block_2),
    z_block_3 = scale(block_3),
    z_block_4 = scale(block_4)
  )

# Identify outliers (with z-score > 3 or < -3) for each block
outliers_block_1 <- WPT_accuracy_VPN_blocks$block_1[abs(WPT_accuracy_VPN_blocks$z_block_1) > 3]
outliers_block_2 <- WPT_accuracy_VPN_blocks$block_2[abs(WPT_accuracy_VPN_blocks$z_block_2) > 3]
outliers_block_3 <- WPT_accuracy_VPN_blocks$block_3[abs(WPT_accuracy_VPN_blocks$z_block_3) > 3]
outliers_block_4 <- WPT_accuracy_VPN_blocks$block_4[abs(WPT_accuracy_VPN_blocks$z_block_4) > 3]

# Print the outliers for each block
outliers_block_1
outliers_block_2
outliers_block_3
outliers_block_4



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
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Calculate mean accuracy and standard error for each block and group
mean_accuracy_by_block <- WPT_accuracy_VPN_blocks %>%
  pivot_longer(cols = starts_with("block_"), names_to = "block", values_to = "accuracy") %>%
  group_by(group, block) %>%
  summarise(
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    se = sd(accuracy, na.rm = TRUE) / sqrt(n())
  )

# Visualize WPT accuracy per block between groups
ggplot(mean_accuracy_by_block, aes(x = block, y = mean_accuracy, fill = group, color = group)) +
  geom_bar(stat = "identity", position = position_dodge(), size = 0.5) +  # Add outline to bars
  geom_errorbar(aes(ymin = mean_accuracy - se, ymax = mean_accuracy + se), 
                position = position_dodge(0.9), width = 0.25, color = "red") +  # Add error bars
  scale_fill_manual(values = c("white", "black")) +  # Change the fill colors to white and black
  scale_color_manual(values = c("black", "black"), guide = FALSE) +  # Change the outline colors to black, hide the color legend
  scale_x_discrete(labels = c("block_1" = "Block 1", "block_2" = "Block 2", "block_3" = "Block 3", "block_4" = "Block 4")) +  # Change the block labels
  labs(title = "WPT Accuracy by Block and Group",
       x = "Block",
       y = "Mean Accuracy") +
  theme_minimal() +
  theme(legend.position = "none",  # Remove the legend
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),  # Adjust the text size and make it bold
        strip.text.x = element_blank())  # Remove facet labels


