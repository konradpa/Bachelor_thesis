library(dplyr)


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
    # If there's only one "1" and it's in the third or fourth place, assign "sun"
    return("rain")
  } else if (pattern[1] == "1" || pattern[2] == "1") {
    # If there's only one "1" and it's in the first or second place, assign "rain"
    return("sun")
  } else {
    # Default case, if no specific conditions are met (should not occur based on given rules)
    return(NA)
  }
}

determine_one_cue_good <- function(stimulus_pattern) {
  # Split the stimulus pattern into individual characters
  pattern <- strsplit(stimulus_pattern, " ")[[1]]
  
  # Check the first digit and assign "sun" or "rain" accordingly
  if (pattern[1] == "1") {
    return("sun")
  } else if (pattern[1] == "0") {
    return("rain")
  } else {
    return(NA)  # In case of unexpected values
  }
}

determine_one_cue_bad <- function(stimulus_pattern) {
  # Split the stimulus pattern into individual characters
  pattern <- strsplit(stimulus_pattern, " ")[[1]]
  
  # Check the second digit and assign "sun" or "rain" accordingly
  if (pattern[2] == "1") {
    return("sun")
  } else if (pattern[2] == "0") {
    return("rain")
  } else {
    return(NA)  # In case of unexpected values
  }
}


## Function to calculate the percentage match
calculate_percentage_match <- function(data) {
  data %>%
    group_by(VPN) %>%
    summarise(
      multicue_match = mean(response == strategy_multicue) * 100,
      singleton_match = mean(response == strategy_singleton) * 100,
      one_cue_good_match = mean(response == one_cue_good) * 100,
      one_cue_bad_match = mean(response == one_cue_bad) * 100
    )
}

# Function to calculate the normalized score for each strategy
calculate_normalized_scores <- function(group) {
  n_presentations <- nrow(group)
  
  # Calculate the squared differences for each trial
  multicue_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_multicue == "sun", 1, 0))^2)
  singleton_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_singleton == "sun", 1, 0))^2)
  one_cue_good_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$one_cue_good == "sun", 1, 0))^2)
  one_cue_bad_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$one_cue_bad == "sun", 1, 0))^2)
  
  # Normalize the scores
  multicue_score <- multicue_squared_diff / n_presentations
  singleton_score <- singleton_squared_diff / n_presentations
  one_cue_good_score <- one_cue_good_squared_diff / n_presentations
  one_cue_bad_score <- one_cue_bad_squared_diff / n_presentations
  
  return(data.frame(
    VPN = unique(group$VPN),
    multicue_score = multicue_score,
    singleton_score = singleton_score,
    one_cue_good_score = one_cue_good_score,
    one_cue_bad_score = one_cue_bad_score
  ))
}

## Define the function to calculate normalized scores for a single group (participant and block)
calculate_normalized_scores_blocks <- function(group) {
  n_presentations <- nrow(group)
  
  # Calculate the squared differences for each trial
  multicue_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_multicue == "sun", 1, 0))^2)
  singleton_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$strategy_singleton == "sun", 1, 0))^2)
  one_cue_good_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$one_cue_good == "sun", 1, 0))^2)
  one_cue_bad_squared_diff <- sum((ifelse(group$response == "sun", 1, 0) - ifelse(group$one_cue_bad == "sun", 1, 0))^2)
  
  # Normalize the scores
  multicue_score <- multicue_squared_diff / n_presentations
  singleton_score <- singleton_squared_diff / n_presentations
  one_cue_good_score <- one_cue_good_squared_diff / n_presentations
  one_cue_bad_score <- one_cue_bad_squared_diff / n_presentations
  
  return(data.frame(
    VPN = unique(group$VPN),
    blockNumber = unique(group$blockNumber),
    multicue_score = multicue_score,
    singleton_score = singleton_score,
    one_cue_good_score = one_cue_good_score,
    one_cue_bad_score = one_cue_bad_score
  ))
}

# find best strategies

find_best_strategies <- function(strategy_scores_blocks, strategy_scores_VPN) {
  # Helper function to find the strategy with the score closest to zero
  find_best_strategy <- function(scores) {
    strategies <- names(scores)
    best_strategy <- strategies[which.min(abs(scores))]
    return(best_strategy)
  }
  
  # Pivot the data longer
  long_strategy_scores_blocks <- strategy_scores_blocks %>%
    pivot_longer(cols = multicue_score:one_cue_bad_score, names_to = "strategy", values_to = "score")
  
  # Calculate best strategies for each block for each participant
  best_strategies_blocks <- long_strategy_scores_blocks %>%
    group_by(VPN, blockNumber) %>%
    summarize(best_strategy_block = strategy[which.min(abs(score))], .groups = 'drop')
  
  # Pivot back to wide format
  best_strategies_blocks_wide <- best_strategies_blocks %>%
    pivot_wider(names_from = blockNumber, values_from = best_strategy_block, names_prefix = "best_strategy_block_")
  
  # Pivot the VPN data longer
  long_strategy_scores_VPN <- strategy_scores_VPN %>%
    pivot_longer(cols = multicue_score:one_cue_bad_score, names_to = "strategy", values_to = "score")
  
  # Calculate best strategies overall for each participant
  best_strategies_overall <- long_strategy_scores_VPN %>%
    group_by(VPN) %>%
    summarize(best_strategy_overall = strategy[which.min(abs(score))], .groups = 'drop')
  
  # Combine results
  result <- best_strategies_blocks_wide %>%
    left_join(best_strategies_overall, by = "VPN")
  
  return(result)
}
