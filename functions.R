library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(readxl)
library(tidyr)
library(emmeans)
library(broom)  
library(car)
library(purrr)  
library(gtools)





ETQ_path <- "Data/ETQ_final/LimeSurveyResults_ExplicitTaskKnowledge_19072024.csv"

info_path <- "Data/VP_info_final/VP_information_18072024.xlsx"

WPT_path <- "Data/WPT_final/Results/"

feedback_path <- sprintf("Data/reward_final/Reward/NFBs_subj_%03d.mat", 1:77)


format_vpn <- function(vpn) {
  paste0("VP_", sprintf("%03d", as.numeric(vpn)))
}


# ETQ score analysis

# Define the shapes
shapes <- c("Quadrate", "Karos", "Kreise", "Dreiecke")

# Generate all permutations of the numbers 1, 2, 3, and 4
permutations <- gtools::permutations(n = 4, r = 4, v = c(1, 2, 3, 4))

# Initialize an empty list to store the correct orders
correct_orders <- list()

# Loop through each permutation
for (i in 1:nrow(permutations)) {
  # Create a permutation string (e.g., "1234")
  perm_string <- paste(permutations[i, ], collapse = "")
  
  # Map the permutation to the corresponding shapes
  correct_orders[[perm_string]] <- shapes[permutations[i, ]]
}

# View the resulting list
correct_orders

# Function to check if participant's answer matches the correct order
check_correct_order <- function(row, correct_orders) {
  correct_order <- correct_orders[[as.character(row$wpt_random_card_order_map)]]
  if (is.null(correct_order)) {
    return(0) # Return 0 if the order map is not found or is NA
  }
  participant_order <- c(
    row$g02q12_1_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_1,
    row$g02q12_2_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_2,
    row$g02q12_3_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_3,
    row$g02q12_4_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_4
  )
  return(ifelse(all(participant_order == correct_order), 1, 0))
}


analyze_etq_data <- function(ETQ_data) {
  
  # Function to calculate the score for each card dynamically
  calculate_score <- function(card_name, answer_column, correct_orders, wpt_random_card_order_map_char) {
    card_position = match(card_name, correct_orders[[wpt_random_card_order_map_char]])
    
    correct_sun_range_lower = case_when(
      card_position == 1 ~ 80.7,
      card_position == 2 ~ 57.5,
      card_position == 3 ~ 32.5,
      card_position == 4 ~ 9.3,
      TRUE ~ NA_real_
    )
    correct_sun_range_upper = case_when(
      card_position == 1 ~ 90.7,
      card_position == 2 ~ 67.5,
      card_position == 3 ~ 43.5,
      card_position == 4 ~ 19.3,
      TRUE ~ NA_real_
    )
    
    participant_answer = as.numeric(answer_column)
    
    score = ifelse(participant_answer >= correct_sun_range_lower & participant_answer <= correct_sun_range_upper, 1, 0)
    
    return(score)
  }
  
  # Logic for determining the card most associated with rain (position 4)
  get_card_for_rain <- function(cardOrder) {
    return(cardOrder[4])
  }
  
  # Logic for determining the card most associated with sun (position 1)
  get_card_for_sun <- function(cardOrder) {
    return(cardOrder[1])
  }
  
  # Score for the first question
  ETQ_data <- ETQ_data %>%
    mutate(score_q2 = ifelse(g02q02_sq004_wie_viele_verschiedene_karten_wurden_ihnen_bei_der_wettervorhersageaufgabe_gezeigt_4 == "Ja", 1, 0))
  
  # Score for the second question
  ETQ_data <- ETQ_data %>%
    mutate(score_q3 = ifelse(g02q03_sq003_wie_viele_quadrate_waren_auf_der_karte_mit_den_quadraten_abgebildet_7 == "Ja", 1, 0))
  
  # Score for the third question
  ETQ_data <- ETQ_data %>%
    mutate(score_q4 = ifelse(g02q04_sq001_wie_viele_kreise_waren_auf_der_karte_mit_den_kreisen_abgebildet_9 == "Ja", 1, 0))
  
  # Score for the fourth question
  ETQ_data <- ETQ_data %>%
    mutate(score_q5 = ifelse(g02q05_sq001_wie_viele_karten_wurden_ihnen_pro_durchgang_gezeigt_1_3_karten == "Ja", 1, 0))
  
  # Score for questions 6 to 9 (Sun percentage for each card)
  ETQ_data <- ETQ_data %>%
    rowwise() %>%
    mutate(
      wpt_random_card_order_map_char = as.character(wpt_random_card_order_map),
      
      # Score for the Quadrate (Squares) card, using g02q09 as the answer column
      score_q6 = calculate_score("Quadrate", g02q09_wenn_nur_die_karte_mit_den_quadraten_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne, correct_orders, wpt_random_card_order_map_char),
      
      # Score for the Karos (Diamonds) card, using g02q08 as the answer column
      score_q7 = calculate_score("Karos", g02q08_wenn_nur_die_karte_mit_den_karos_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne, correct_orders, wpt_random_card_order_map_char),
      
      # Score for the Kreise (Circles) card, using g02q06 as the answer column
      score_q8 = calculate_score("Kreise", g02q06_wenn_nur_die_karte_mit_den_kreisen_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne, correct_orders, wpt_random_card_order_map_char),
      
      # Score for the Dreiecke (Triangles) card, using g02q07 as the answer column
      score_q9 = calculate_score("Dreiecke", g02q07_wenn_nur_die_karte_mit_den_dreiecken_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne, correct_orders, wpt_random_card_order_map_char),
      
      # Score for Question 10: Which card corresponds to "rain" (position 4)
      score_q10 = case_when(
        get_card_for_rain(correct_orders[[wpt_random_card_order_map_char]]) == "Kreise" & g02q10_sq001_nehmen_sie_an_sie_wissen_dass_es_regnen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_kreisen == "Ja" ~ 1,
        get_card_for_rain(correct_orders[[wpt_random_card_order_map_char]]) == "Dreiecke" & g02q10_sq004_nehmen_sie_an_sie_wissen_dass_es_regnen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_dreiecken == "Ja" ~ 1,
        get_card_for_rain(correct_orders[[wpt_random_card_order_map_char]]) == "Quadrate" & g02q10_sq003_nehmen_sie_an_sie_wissen_dass_es_regnen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_quadraten == "Ja" ~ 1,
        get_card_for_rain(correct_orders[[wpt_random_card_order_map_char]]) == "Karos" & g02q10_sq002_nehmen_sie_an_sie_wissen_dass_es_regnen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_karos == "Ja" ~ 1,
        TRUE ~ 0
      ),
      
      # Score for Question 11: Which card corresponds to "sun" (position 1)
      score_q11 = case_when(
        get_card_for_sun(correct_orders[[wpt_random_card_order_map_char]]) == "Kreise" & g02q11_sq001_nehmen_sie_an_sie_wissen_dass_die_sonne_scheinen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_kreisen == "Ja" ~ 1,
        get_card_for_sun(correct_orders[[wpt_random_card_order_map_char]]) == "Dreiecke" & g02q11_sq004_nehmen_sie_an_sie_wissen_dass_die_sonne_scheinen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_dreiecken == "Ja" ~ 1,
        get_card_for_sun(correct_orders[[wpt_random_card_order_map_char]]) == "Quadrate" & g02q11_sq003_nehmen_sie_an_sie_wissen_dass_die_sonne_scheinen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_quadraten == "Ja" ~ 1,
        get_card_for_sun(correct_orders[[wpt_random_card_order_map_char]]) == "Karos" & g02q11_sq002_nehmen_sie_an_sie_wissen_dass_die_sonne_scheinen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_karos == "Ja" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    ungroup()
  
  # Score for the eleventh question: checking card order
  ETQ_data <- ETQ_data %>%
    rowwise() %>%
    mutate(score_q12 = check_correct_order(cur_data(), correct_orders)) %>%
    ungroup()
  
  # Calculate the overall score by summing score_q2 to score_q12
  ETQ_data <- ETQ_data %>%
    mutate(overall_score = score_q2 + score_q3 + score_q4 + score_q5 +
             score_q6 + score_q7 + score_q8 + score_q9 + score_q10 + score_q11 + score_q12)
  
  # Reorder columns to place overall_score as the first column
  ETQ_data <- ETQ_data %>%
    select(overall_score, everything())
  
  return(ETQ_data)
}

visualize_ETQ_scores <- function(ETQ_data) {
  ggplot(ETQ_data, aes(x = overall_score)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Distribution of Overall Scores",
         x = "Overall Score",
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text = element_text(size = 12))
}


#WPT files

extract_participant_id <- function(file_name) {
  id <- sub("VP_(\\d+)_.*", "\\1", file_name)
  return(id)
}

load_WPT_file <- function(file_path) {
  df <- read_delim(file_path, delim = ";", col_names = TRUE)
  df <- mutate(df, response = as.character(response))  # Ensure 'response' is character
  participant_id <- extract_participant_id(basename(file_path))  # Extract participant ID
  df <- df %>% mutate(n = participant_id) %>% select(n, everything())  # Add participant ID as first column
  return(df)
}


clean_WPT_data <- function(df) {
  df_clean <- df %>%
    filter(trialType != "reactivation" & trialType != "control") %>%
    drop_na(trialNumber, blockNumber, trialType, stimulusPattern, pSun, correctOutcome, actualOutcome, response, correctness, reactionTime)
  return(df_clean)
}

# add feedback values
extract_values <- function(feedback_path) {
  data <- readMat(feedback_path)
  values <- NA  # Default to NA if vectNFBs.PSC is not present
  
  if (!is.null(data$vectNFBs.PSC)) {
    values <- data$vectNFBs.PSC[1, ]  # Extract all values
  }
  return(values)
}
