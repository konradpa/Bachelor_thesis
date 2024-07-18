library(dplyr)
library(readr)
library(tidyr)
library(writexl)

# Define Ideal Strategies
define_ideal_strategies <- function() {
  idealStrategy <- list(
    Cue1 = c(1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0),
    Cue2 = c(1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0),
    Cue3 = c(1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0),
    Cue4 = c(1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0),
    Singleton = c(0.5, 1, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0, 0.5),
    MultiCueMax = c(1, 1, 1, 1, 1, 1, 0.5, 0.5, 0, 0, 0, 0, 0, 0),
    MultiCueMatch = c(0.889, 0.857, 0.833, 0.75, 0.667, 0.625, 0.5, 0.5, 0.375, 0.333, 0.25, 0.167, 0.143, 0.111)
  )
  return(idealStrategy)
}

map_stimulus_pattern <- function(pattern) {
  pattern <- gsub(" ", "", pattern)  # Remove spaces
  binary_patterns <- c("1110", "1100", "1011", "1001", "0111", "0101", "0010", "0001", "0000", "1111", "1101", "1010", "0110", "0100")
  return(match(pattern, binary_patterns))
}

compute_strategy_coefficients <- function(data, idealStrategy) {
  strategyCount <- length(idealStrategy)
  trialSize <- nrow(data)
  blockCount <- 7
  block <- floor(trialSize / 4)
  blockTrials <- list(
    c(1, block),
    c(block + 1, 2 * block),
    c(2 * block + 1, 3 * block),
    c(3 * block + 1, trialSize),
    c(1, 2 * block),
    c(2 * block + 1, trialSize),
    c(1, trialSize)
  )
  
  strategyCoefficientOutput <- matrix(NA, nrow = strategyCount, ncol = blockCount)
  
  for (blockIndex in 1:blockCount) {
    subset_data <- data[blockTrials[[blockIndex]][1]:blockTrials[[blockIndex]][2], ]
    sumRespond <- rep(0, 14)
    sumPresentationFreq <- rep(0, 14)
    sumMiss <- rep(0, 14)
    
    for (trial in seq_len(nrow(subset_data))) {
      condition <- map_stimulus_pattern(subset_data$stimulusPattern[trial])
      response <- subset_data$response[trial]
      
      if (!is.na(condition) && condition >= 1 && condition <= 14) {
        if (response == "sun") {
          sumRespond[condition] <- sumRespond[condition] + 1
        }
        
        sumPresentationFreq[condition] <- sumPresentationFreq[condition] + 1
        if (is.na(response) || response %in% c("", "miss", "NA")) {
          sumMiss[condition] <- sumMiss[condition] + 1
        }
      }
    }
    
    print(list(sumRespond = sumRespond, sumPresentationFreq = sumPresentationFreq, sumMiss = sumMiss))
    
    for (strategy in seq_len(strategyCount)) {
      strategy_values <- idealStrategy[[strategy]]
      
      if (length(sumRespond) == length(strategy_values) && length(sumPresentationFreq) == length(sumMiss)) {
        correctResponses <- sumRespond * strategy_values
        totalResponses <- sumPresentationFreq - sumMiss
        
        if (all(!is.na(totalResponses)) && sum(totalResponses) > 0) {
          strategyCoefficientOutput[strategy, blockIndex] <- sum(correctResponses, na.rm = TRUE) / sum(totalResponses, na.rm = TRUE)
        } else {
          strategyCoefficientOutput[strategy, blockIndex] <- NA
        }
      } else {
        strategyCoefficientOutput[strategy, blockIndex] <- NA
      }
    }
  }
  
  return(strategyCoefficientOutput)
}

# Main Function
compute_strategy <- function(data, folderSubj, settings) {
  idealStrategy <- define_ideal_strategies()
  strategyCoefficientOutput <- compute_strategy_coefficients(data, idealStrategy)
  
  strategyHeader <- c("Cue1", "Cue2", "Cue3", "Cue4", "Singleton", "MultiCueMax", "MultiCueMatch")
  RESULT <- data.frame(
    Strategie = strategyHeader,
    K_1_4 = strategyCoefficientOutput[, 1],
    K_2_4 = strategyCoefficientOutput[, 2],
    K_3_4 = strategyCoefficientOutput[, 3],
    K_4_4 = strategyCoefficientOutput[, 4],
    K_1_2 = strategyCoefficientOutput[, 5],
    K_2_2 = strategyCoefficientOutput[, 6],
    K_1_1 = strategyCoefficientOutput[, 7]
  )
  
  print(RESULT)
  
  # Save data to Excel file
  filename <- paste0(settings$dir_WPT_strategies, "/VP_", folderSubj, "_strategy.xlsx")
  write_xlsx(RESULT, filename)
  
  return(RESULT)
}

