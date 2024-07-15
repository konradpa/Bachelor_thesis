library(dplyr)

# Define a function to rearrange the stimulus pattern based on the order
rearrange_pattern <- function(pattern, order) {
  if (is.na(order)) {
    return(pattern)
  }
  
  # Split the pattern into individual characters
  pattern_split <- strsplit(pattern, " ")[[1]]
  
  # Convert the order to numeric indices
  order_indices <- as.numeric(strsplit(order, "")[[1]])
  
  # Rearrange the pattern based on the order
  rearranged_pattern <- pattern_split[order_indices]
  
  # Join the rearranged pattern back into a string
  rearranged_pattern_str <- paste(rearranged_pattern, collapse = " ")
  
  return(rearranged_pattern_str)
}

# "simulate" results for speific strategy 
## Function to determine strategy_singleton based on stimulusPattern
determine_strategy_singleton <- function(stimulus_pattern) {
  # Split the stimulus pattern into individual characters
  pattern <- strsplit(stimulus_pattern, " ")[[1]]
  
  # Count the number of "1"s in the pattern
  count_ones <- sum(as.numeric(pattern))
  
  if (count_ones > 1) {
    # If more than one "1", assign a random value of "rain" or "sun"
    return(sample(c("rain", "sun"), 1))
  } else if (pattern[3] == "1" || pattern[4] == "1") {
    # If there's only one "1" and it's in the third or fourth place, assign "rain"
    return("rain")
  } else if (pattern[1] == "1" || pattern[2] == "1") {
    # If there's only one "1" and it's in the first or second place, assign "sun"
    return("sun")
  } else {
    # Default case, if no specific conditions are met (should not occur based on given rules)
    return(NA)
  }
}

# Functions to determine cue-based strategies
determine_cue_1 <- function(stimulus_pattern) {
  pattern <- strsplit(stimulus_pattern, " ")[[1]]
  if (pattern[1] == "1") {
    return("sun")
  } else {
    return("rain")
  }
}

determine_cue_2 <- function(stimulus_pattern) {
  pattern <- strsplit(stimulus_pattern, " ")[[1]]
  if (pattern[2] == "1") {
    return("sun")
  } else {
    return("rain")
  }
}

determine_cue_3 <- function(stimulus_pattern) {
  pattern <- strsplit(stimulus_pattern, " ")[[1]]
  if (pattern[3] == "1") {
    return("rain")
  } else {
    return("sun")
  }
}

determine_cue_4 <- function(stimulus_pattern) {
  pattern <- strsplit(stimulus_pattern, " ")[[1]]
  if (pattern[4] == "1") {
    return("rain")
  } else {
    return("sun")
  }
}

calculate_percentage_match <- function(data) {
  data %>%
    group_by(VPN) %>%
    summarise(
      multicue_match = mean(response == strategy_multicue) * 100,
      singleton_match = mean(response == strategy_singleton) * 100,
      cue_1_match = mean(response == cue_1, na.rm = TRUE) * 100,
      cue_2_match = mean(response == cue_2, na.rm = TRUE) * 100,
      cue_3_match = mean(response == cue_3, na.rm = TRUE) * 100,
      cue_4_match = mean(response == cue_4, na.rm = TRUE) * 100
    )
}

# Function to calculate the normalized score for each strategy
calculate_normalized_scores <- function(group) {
  n_presentations <- nrow(group)
  
  # Calculate the squared differences for each trial
  multicue_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_multicue == "sun", 1, 0))^2)
  singleton_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_singleton == "sun", 1, 0))^2)
  cue_1_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_1 == "sun", 1, 0))^2, na.rm = TRUE)
  cue_2_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_2 == "sun", 1, 0))^2, na.rm = TRUE)
  cue_3_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_3 == "rain", 1, 0))^2, na.rm = TRUE)
  cue_4_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_4 == "rain", 1, 0))^2, na.rm = TRUE)
  
  # Normalize the scores
  multicue_score <- multicue_squared_diff / n_presentations
  singleton_score <- singleton_squared_diff / n_presentations
  cue_1_score <- cue_1_squared_diff / n_presentations
  cue_2_score <- cue_2_squared_diff / n_presentations
  cue_3_score <- cue_3_squared_diff / n_presentations
  cue_4_score <- cue_4_squared_diff / n_presentations
  
  return(data.frame(
    VPN = unique(group$VPN),
    multicue_score = multicue_score,
    singleton_score = singleton_score,
    cue_1_score = cue_1_score,
    cue_2_score = cue_2_score,
    cue_3_score = cue_3_score,
    cue_4_score = cue_4_score
  ))
}

# Define the function to calculate normalized scores for a single group (participant and block)
calculate_normalized_scores_blocks <- function(group) {
  n_presentations <- nrow(group)
  
  # Calculate the squared differences for each trial
  multicue_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_multicue == "sun", 1, 0))^2)
  singleton_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_singleton == "sun", 1, 0))^2)
  cue_1_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_1 == "sun", 1, 0))^2, na.rm = TRUE)
  cue_2_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_2 == "sun", 1, 0))^2, na.rm = TRUE)
  cue_3_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_3 == "rain", 1, 0))^2, na.rm = TRUE)
  cue_4_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$cue_4 == "rain", 1, 0))^2, na.rm = TRUE)
  
  # Normalize the scores
  multicue_score <- multicue_squared_diff / n_presentations
  singleton_score <- singleton_squared_diff / n_presentations
  cue_1_score <- cue_1_squared_diff / n_presentations
  cue_2_score <- cue_2_squared_diff / n_presentations
  cue_3_score <- cue_3_squared_diff / n_presentations
  cue_4_score <- cue_4_squared_diff / n_presentations
  
  return(data.frame(
    VPN = unique(group$VPN),
    blockNumber = unique(group$blockNumber),
    multicue_score = multicue_score,
    singleton_score = singleton_score,
    cue_1_score = cue_1_score,
    cue_2_score = cue_2_score,
    cue_3_score = cue_3_score,
    cue_4_score = cue_4_score
  ))
}

# Function to find best strategies
find_best_strategies <- function(strategy_scores_blocks, strategy_scores_VPN) {
  # Helper function to find the strategy with the score closest to zero
  find_best_strategy <- function(scores) {
    strategies <- names(scores)
    best_strategy <- strategies[which.min(abs(scores))]
    return(best_strategy)
  }
  
  # Pivot the data longer
  long_strategy_scores_blocks <- strategy_scores_blocks %>%
    pivot_longer(cols = multicue_score:cue_4_score, names_to = "strategy", values_to = "score")
  
  # Calculate best strategies for each block for each participant
  best_strategies_blocks <- long_strategy_scores_blocks %>%
    group_by(VPN, blockNumber) %>%
    summarize(best_strategy_block = strategy[which.min(abs(score))], .groups = 'drop')
  
  # Pivot back to wide format
  best_strategies_blocks_wide <- best_strategies_blocks %>%
    pivot_wider(names_from = blockNumber, values_from = best_strategy_block, names_prefix = "best_strategy_block_")
  
  # Pivot the VPN data longer
  long_strategy_scores_VPN <- strategy_scores_VPN %>%
    pivot_longer(cols = multicue_score:cue_4_score, names_to = "strategy", values_to = "score")
  
  # Calculate best strategies overall for each participant
  best_strategies_overall <- long_strategy_scores_VPN %>%
    group_by(VPN) %>%
    summarize(best_strategy_overall = strategy[which.min(abs(score))], .groups = 'drop')
  
  # Combine results
  result <- best_strategies_blocks_wide %>%
    left_join(best_strategies_overall, by = "VPN")
  
  return(result)
}
