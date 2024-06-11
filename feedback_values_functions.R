# Install necessary package if not already installed
if (!require("R.matlab")) {
  install.packages("R.matlab")
}
library(R.matlab)

# Function to extract last 5 values from vectNFBs.PSC in .mat file
extract_last_5_values <- function(file) {
  # Read the .mat file
  mat_data <- readMat(file)
  
  # Initialize the return value with NAs
  last_5_values <- rep(NA, 5)
  
  # Check if 'vectNFBs.PSC' key exists and has sufficient length
  if ("vectNFBs.PSC" %in% names(mat_data)) {
    vectNFBs_PSC <- mat_data[["vectNFBs.PSC"]]
    if (length(vectNFBs_PSC) >= 5) {
      # Extract the last 5 values
      last_5_values <- tail(vectNFBs_PSC, 5)
    } else {
      cat("File", file, "has 'vectNFBs.PSC' key but insufficient length\n")
    }
  } else {
    cat("File", file, "does not contain 'vectNFBs.PSC' key\n")
  }
  
  # Return a named vector with the file name and values
  result <- c(Filename = basename(file), last_5_values)
  return(result)
}

# Function to process all .mat files in a directory and return a dataframe
process_mat_files <- function(directory) {
  # List all .mat files in the directory
  files <- list.files(directory, pattern = "\\.mat$", full.names = TRUE)
  
  # Initialize an empty list to store the extracted data
  data_list <- lapply(files, extract_last_5_values)
  
  # Filter out any non-conforming results
  data_list <- Filter(function(x) length(x) == 6, data_list)
  
  # Convert the list to a dataframe if not empty
  if (length(data_list) > 0) {
    df <- do.call(rbind, lapply(data_list, as.data.frame))
    colnames(df) <- c("Filename", "Value1", "Value2", "Value3", "Value4", "Value5")
  } else {
    df <- data.frame(Filename = character(),
                     Value1 = numeric(), Value2 = numeric(), 
                     Value3 = numeric(), Value4 = numeric(), 
                     Value5 = numeric(), stringsAsFactors = FALSE)
    cat("No valid data extracted from the .mat files.\n")
  }
  
  return(df)
}
