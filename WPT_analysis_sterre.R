rm(list = ls())
gc()

library(dplyr)
library(tidyr)
library(ggplot2)

# Define the directories
dir_WPT_log <- "/Users/test/Desktop/Bachelorarbeit/BA_analyses/data/data_24.05/WPT_results/Results"
output_dir <- "/Users/test/Desktop/Bachelorarbeit/BA_analyses/data/data_24.05/WPT_results/Data/Behavior/WPT"
dir_WPT_strategies <- file.path(output_dir, "strategies")

# Ensure the output and strategies directories exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
if (!dir.exists(dir_WPT_strategies)) {
  dir.create(dir_WPT_strategies, recursive = TRUE)
}

# List all CSV files in the directory
file_names <- list.files(path = dir_WPT_log, pattern = "VP_.*\\.csv", full.names = TRUE)

# Initialize a list to store the data frames
result_list <- list()

# Iterate over each file to load and merge data
for (file_path in file_names) {
  # Load the CSV file
  data <- read.csv(file_path, sep = ";")
  
  # Ensure the `response` column is of type character
  data$response <- as.character(data$response)
  
  # Extract the subject ID from the file name using a regular expression
  file_name <- basename(file_path)
  subject_id <- sub("VP_(\\d+)_.*", "\\1", file_name)
  
  # Ensure the subject ID is correctly extracted
  if (nchar(subject_id) == 0 || is.na(as.integer(subject_id))) {
    warning("Could not extract a valid subject ID from file name: ", file_name)
    next
  }
  
  subject_id <- sprintf("%03d", as.integer(subject_id))  # Convert to integer and format
  
  # Add Subject ID to the data frame
  data$Subject <- subject_id
  
  # Debugging: Print subject_id to ensure it's correct
  print(paste("Extracted Subject ID:", subject_id, "from file:", file_name))
  
  # Append the data frame to the list
  result_list <- append(result_list, list(data))
}



system("sysctl hw.memsize")

str(result_list)

# Combine all data frames into one (in chunks size otherwise R crashes for me)
# Function to process and bind rows in chunks
process_in_chunks <- function(data_list, chunk_size) {
  num_chunks <- ceiling(length(data_list) / chunk_size)
  combined_df <- NULL
  
  for (i in 1:num_chunks) {
    start_index <- (i - 1) * chunk_size + 1
    end_index <- min(i * chunk_size, length(data_list))
    chunk <- data_list[start_index:end_index]
    
    # Bind rows of the current chunk
    chunk_df <- bind_rows(chunk)
    
    if (is.null(combined_df)) {
      combined_df <- chunk_df
    } else {
      combined_df <- bind_rows(combined_df, chunk_df)
    }
    
    # Perform garbage collection to free memory
    gc()
  }
  
  return(combined_df)
}


chunk_size <- 1000  #
allSubjData <- process_in_chunks(result_list, chunk_size)

# Save the merged results as .RData file
save(allSubjData, file = file.path(output_dir, "merged_results.RData"))

# Print structure of the combined data
str(allSubjData)

load(file.path(output_dir, "merged_results.RData"))

# Define subjects according to counterbalancing version
subjects_SR_1234 <- c(1:3, 6:57)


# define compute strategy function
computeStrategy <- function(conditionMatrix, responseMatrix, subj_idx, settings) {
  
  # INITIALIZE VARIABLES
  brightSide <- 1  # left cards are linked to high p(Sun)
  miss <- c(999, -1, -9, NA)
  
  # Save data to variables
  conditions <- conditionMatrix$stimulus
  response <- responseMatrix$response
  
  # Partitioning of the data (7 blocks: 4 blocks - 2 blocks - 1 block)
  blockCount <- 7
  trialSize <- length(conditions)
  isEven <- trialSize %% 4 == 0
  block <- floor(trialSize / 4)
  blockTrials <- matrix(c(1, block, block + 1, 2 * block, 2 * block + 1, 3 * block,
                          3 * block + 1, trialSize, 1, 2 * block, 2 * block + 1, trialSize, 1, trialSize),
                        ncol = 2, byrow = TRUE)
  
  # DEFINE IDEAL STRATEGIES
  strategyCount <- 7  # number of analyzed strategies
  idealStrategy <- list(
    c(1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0),       # Cue 1
    c(1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0),       # Cue 2
    c(1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0),       # Cue 3
    c(1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0),       # Cue 4
    c(0.5, 1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0, 0.5),   # Singleton
    c(1, 1, 1, 1, 1, 1, 0.5, 0.5, 0, 0, 0, 0, 0, 0),   # Multi-Cue-Max
    c(0.889, 0.857, 0.833, 0.750, 0.667, 0.625, 0.5, 0.5, 0.375, 0.333, 0.25, 0.167, 0.143, 0.111)  # Multi-Cue-Match
  )
  
  # COMPUTE STRATEGY COEFFICIENTS
  strategyCoefficientOutput <- matrix(0, nrow = strategyCount, ncol = blockCount)
  sumRespond <- rep(0, 14)
  sumPresentationFreq <- rep(0, 14)
  sumMiss <- rep(0, 14)
  
  for (blockIndex in 1:blockCount) {
    # Reset temporary variables
    sumRespond <- rep(0, 14)
    sumPresentationFreq <- rep(0, 14)
    sumMiss <- rep(0, 14)
    
    # Compute sum of presentations and sum of responses
    for (trial in blockTrials[blockIndex, 1]:blockTrials[blockIndex, 2]) {
      if (!is.na(response[trial]) && response[trial] == brightSide) {
        sumRespond[conditions[trial] %% 100] <- sumRespond[conditions[trial] %% 100] + 1
      }
      
      if (is.na(response[trial]) || response[trial] %in% miss) {
        sumMiss[conditions[trial] %% 100] <- sumMiss[conditions[trial] %% 100] + 1
      } else {
        sumPresentationFreq[conditions[trial] %% 100] <- sumPresentationFreq[conditions[trial] %% 100] + 1
      }
    }
    
    # Compute coefficients
    for (coefficient in 1:strategyCount) {
      sumDenominator <- 0
      sumDivisor <- 0
      for (pattern in 1:14) {
        sumDenominator <- sumDenominator + (sumPresentationFreq[pattern] * idealStrategy[[coefficient]][pattern] - sumRespond[pattern])^2
        sumDivisor <- sumDivisor + sumPresentationFreq[pattern]^2
      }
      strategyCoefficientOutput[coefficient, blockIndex] <- sumDenominator / sumDivisor
    }
  }
  
  missTotal <- sum(sumMiss)
  
  # SAVE RESULTS
  # Define column and row headings
  strategyHeader <- c('Cue1', 'Cue2', 'Cue3', 'Cue4', 'Singleton', 'MultiCue-Max', 'MultiCue-Match')
  RESULT <- data.frame(matrix(0, nrow = strategyCount + 1, ncol = 8))
  names(RESULT) <- c('Strategie', 'K 1/4', 'K 2/4', 'K 3/4', 'K 4/4', 'K 1/2', 'K 2/2', 'K 1/1')
  RESULT[1, ] <- c('Strategie', 'K 1/4', 'K 2/4', 'K 3/4', 'K 4/4', 'K 1/2', 'K 2/2', 'K 1/1')
  
  # Copy coefficients to RESULT structure array
  for (l in 1:strategyCount) {
    RESULT[l + 1, 1] <- strategyHeader[l]
    RESULT[l + 1, 2] <- strategyCoefficientOutput[l, 1]
    RESULT[l + 1, 3] <- strategyCoefficientOutput[l, 2]
    RESULT[l + 1, 4] <- strategyCoefficientOutput[l, 3]
    RESULT[l + 1, 5] <- strategyCoefficientOutput[l, 4]
    RESULT[l + 1, 6] <- strategyCoefficientOutput[l, 5]
    RESULT[l + 1, 7] <- strategyCoefficientOutput[l, 6]
    RESULT[l + 1, 8] <- strategyCoefficientOutput[l, 7]
  }
  
  # Display results in console
  print(RESULT)
  cat("\n")
  cat(trialSize, "trials\n")
  cat(missTotal, "missed\n")
  if (!isEven) {
    cat("The number of trials cannot be divided by four, resulting in uneven block sizes\n")
  }
  
  # Convert RESULT to a data frame and add the subject ID
  RESULT$Subject <- subj_idx
  
  # Return the RESULT data frame
  return(RESULT)
}


strategy_results_list <- list()

for (subject_id in subjects_SR_1234) {
  subject_id <- sprintf("%03d", subject_id)
  cat("Processing participant", subject_id, "...\n")
  
  # Filter the data for the current subject
  subject_data <- allSubjData %>% filter(Subject == subject_id)
  
  if (nrow(subject_data) == 0) {
    warning("No data found for participant", subject_id)
    next
  }
  
  # Extract PCL trials ('weather')
  PCL_trials <- subject_data %>% filter(trialType == 'weather')
  
  # Initialize condition and response matrices
  conditionMatrix <- PCL_trials %>%
    select(trialNumber, blockNumber, stimulus, actualOutcome) %>%
    mutate(actualOutcome = ifelse(actualOutcome == 'rain', 0, ifelse(actualOutcome == 'sun', 1, NA)))
  
  responseMatrix <- PCL_trials %>%
    select(response) %>%
    mutate(response = ifelse(response == 'rain', 0, ifelse(response == 'sun', 1, -9)))
  
  # Call computeStrategy function
  strategy_results <- computeStrategy(conditionMatrix, responseMatrix, subject_id, settings)
  strategy_results_list <- append(strategy_results_list, list(strategy_results))
}

# Combine all strategy results into one data frame
all_strategy_results <- bind_rows(strategy_results_list)

# Save the strategy results as .RData file
save(all_strategy_results, file = file.path(output_dir, "strategy_results.RData"))

# List of NF and sham subjects without the "VP_" prefix
nf_subjects <- c("001", "002", "003", "007", "009", "011", "012", 
                 "014", "015", "016", "019", "020", "028", "033", 
                 "041", "043", "044")
sham_subjects <- c("025", "026", "027", "030", "034", "037", "038", 
                   "039", "045", "046", "051", "054", "056", "057")

# Combine the list of subjects and add group information
group_mapping <- data.frame(
  Subject = c(nf_subjects, sham_subjects),
  Group = c(rep("NF", length(nf_subjects)), rep("Sham", length(sham_subjects)))
)

# Filter the data to only include the relevant subjects
filtered_strategy_results <- all_strategy_results %>%
  filter(Subject %in% group_mapping$Subject) %>%
  left_join(group_mapping, by = "Subject")


filtered_strategy_results <- merge(filtered_strategy_results, group_mapping, by = "Subject")

str(filtered_strategy_results)

# Convert relevant columns to numeric
filtered_strategy_results[, 3:9] <- lapply(filtered_strategy_results[, 3:9], function(x) as.numeric(gsub("K \\d/\\d", "", x)))


long_strategy_data <- filtered_strategy_results %>%
  gather(key = "Block", value = "Score", `K 1/4`:`K 1/1`)

str(long_strategy_data)

# Filter strategies with a score <= 0.16
long_strategy_data_filtered <- long_strategy_data %>%
  filter(Score <= 0.16)

# check for lowest strategy
lowest_strategy_data <- long_strategy_data_filtered %>%
  group_by(Subject, Block) %>%
  slice(which.min(Score)) %>%
  ungroup()


# chi square test
chi_square_results <- list()

for(block in unique(lowest_strategy_data$Block)) {
  block_data <- lowest_strategy_data %>%
    filter(Block == block) %>%
    count(Group.x, Strategie)
  
  contingency_table <- xtabs(n ~ Group.x + Strategie, data = block_data)
  
  chi_square_results[[block]] <- chisq.test(contingency_table)
}

# Display results
chi_square_results


# analyze stratagiessimpliefied vs multicue

multi_cue_strategies <- c("MultiCue-Max", "MultiCue-Match ", "Cue3")  # Update this list with the actual multi-cue strategy names

long_strategy_data_filtered_simplified <- long_strategy_data_filtered %>%
  mutate(Strategie = ifelse(Strategie %in% multi_cue_strategies, "multi cue", "simple strategies"))

# check for lowest strategy
lowest_strategy_data_simplified <- long_strategy_data_filtered_simplified %>%
  group_by(Subject, Block) %>%
  slice(which.min(Score)) %>%
  ungroup()


# chi square test
chi_square_results_simplified <- list()

for(block in unique(lowest_strategy_data_simplified$Block)) {
  block_data <- lowest_strategy_data_simplified %>%
    filter(Block == block) %>%
    count(Group.x, Strategie)
  
  contingency_table <- xtabs(n ~ Group.x + Strategie, data = block_data)
  
  chi_square_results_simplified[[block]] <- chisq.test(contingency_table)
}

# Display results
chi_square_results_simplified


# visualization 

# Ensure the data is in the correct format for plotting
plot_data <- lowest_strategy_data_simplified %>%
  count(Group.x, Block, Strategie) %>%
  group_by(Block, Strategie) %>%
  mutate(Percentage = n / sum(n) * 100)

filtered_plot_data <- plot_data %>%
  filter(Block %in% c("K 1/4", "K 2/4", "K 3/4", "K 4/4"))


# Create the bar plot
# Create the bar plot
library(ggplot2)

# Create the bar plot
ggplot(filtered_plot_data, aes(x = Block, y = Percentage, fill = Group.x)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~ Strategie, scales = "free_x") +
  labs(title = "Strategy Usage by Group and Block",
       x = "Block",
       y = "% Subjects",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# necxt steps:
visualize the data (simpliefied and not)
