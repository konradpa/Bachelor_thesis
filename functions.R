library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(readxl)



ETQ_path <- "data/data_24.05/LimeSurveyResults_ExplicitTaskKnowledge_03052024.txt.csv"

info_path <- "data/data_24.05/VP_information_03052024.xlsx"

analyze_etq_data <- function(ETQ_data) {
  # Analyze answers
  
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
  
  # Score for the fifth question
  ETQ_data <- ETQ_data %>%
    mutate(score_q6 = ifelse(as.numeric(g02q06_wenn_nur_die_karte_mit_den_kreisen_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) >= 32.5 &
                               as.numeric(g02q06_wenn_nur_die_karte_mit_den_kreisen_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) <= 43.5, 1, 0))
  
  # Score for the sixth question
  ETQ_data <- ETQ_data %>%
    mutate(score_q7 = ifelse(as.numeric(g02q07_wenn_nur_die_karte_mit_den_dreiecken_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) >= 9.3 &
                               as.numeric(g02q07_wenn_nur_die_karte_mit_den_dreiecken_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) <= 19.3, 1, 0))
  
  # Score for the seventh question
  ETQ_data <- ETQ_data %>%
    mutate(score_q8 = ifelse(as.numeric(g02q08_wenn_nur_die_karte_mit_den_karos_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) >= 57.5 &
                               as.numeric(g02q08_wenn_nur_die_karte_mit_den_karos_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) <= 67.5, 1, 0))
  
  # Score for the eighth question
  ETQ_data <- ETQ_data %>%
    mutate(score_q9 = ifelse(as.numeric(g02q09_wenn_nur_die_karte_mit_den_quadraten_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) >= 80.7 &
                               as.numeric(g02q09_wenn_nur_die_karte_mit_den_quadraten_gezeigt_wurde_zu_wie_viel_prozent_schien_die_sonne) <= 90.7, 1, 0))
  
  # Score for the ninth question
  ETQ_data <- ETQ_data %>%
    mutate(score_q10 = ifelse(g02q10_sq004_nehmen_sie_an_sie_wissen_dass_es_regnen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_dreiecken == "Ja", 1, 0))
  
  # Score for the tenth question
  ETQ_data <- ETQ_data %>%
    mutate(score_q11 = ifelse(g02q11_sq003_nehmen_sie_an_sie_wissen_dass_die_sonne_scheinen_wird_welche_karte_wurde_ihnen_hochstwahrscheinlich_gezeigt_die_karte_mit_den_quadraten == "Ja", 1, 0))
  
  # Score for the eleventh question
  ETQ_data <- ETQ_data %>%
    mutate(score_q12 = ifelse(
      g02q12_1_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_1 == "Quadrate" &
        g02q12_2_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_2 == "Karos" &
        g02q12_3_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_3 == "Kreise" &
        g02q12_4_jede_der_karten_wurde_ihnen_immer_an_einer_bestimmten_position_dargeboten_bitte_bringen_sie_die_karten_in_die_richtige_reihenfolge_von_oben_1_wurde_ganz_links_prasentiert_nach_unten_4_wurde_ganz_rechts_prasentiert_rank_4 == "Dreiecke",
      1, 0
    ))
  
  # Score for the twelfth question
  ETQ_data <- ETQ_data %>%
    mutate(score_q13 = ifelse(g02q13_sq002_welche_antwort_beschreibt_am_besten_die_strategie_die_sie_zur_vorhersage_des_wetters_benutzt_haben_ich_konnte_mich_daran_erinnern_welche_kartenkombination_welches_wetter_vorhersagt == "Ja", 1, 0))
  
  # Calculate the overall score by summing score_q1 to score_q13
  ETQ_data <- ETQ_data %>%
    mutate(overall_score = score_q2 + score_q3 + score_q4 + score_q5 + 
             score_q6 + score_q7 + score_q8 + score_q9 + score_q10 + score_q11 + score_q12 + score_q13)
  
  # Reorder columns to place overall_score as the first column
  ETQ_data <- ETQ_data %>%
    select(overall_score, everything())
  
  return(ETQ_data)
}

visualize_overall_scores <- function(ETQ_data) {
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

