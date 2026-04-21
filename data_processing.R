# ============================================================================
# DATA PROCESSING
# Komplette Datenverarbeitung übernommen aus dem Original R-Markdown Skript
# ============================================================================

library(tidyverse)
library(readxl)
library(here)

# ============================================================================
# SIMULATED DATA LOADER
# Diese Funktion simuliert die Datenstruktur basierend auf dem Original
# ERSETZE diese Funktion mit echtem Daten-Import später!
# ============================================================================

load_simulated_data <- function() {
  set.seed(42)
  n <- 1500  # Anzahl simulierte Beobachtungen
  
  df <- tibble(
    # IDs
    orga = sample(1:15, n, replace = TRUE),
    unit = sample(c("-1", "1", "2", "3", "4", "5", "6", "7", "8", "9"), n, replace = TRUE),
    region = sample(c("-1", "1", "2", "3", "4", "5"), n, replace = TRUE),
    
    # Welle
    waveNumber = sample(c("2022/2023", "2023/2024", "2024/2025"), n, replace = TRUE, 
                        prob = c(0.2, 0.3, 0.5)),
    
    # Soziodemographie
    age = sample(1:6, n, replace = TRUE, prob = c(0.05, 0.15, 0.25, 0.30, 0.20, 0.05)),
    gender = sample(1:4, n, replace = TRUE, prob = c(0.45, 0.50, 0.02, 0.03)),
    
    # Anstellungskontext (1 = Ja, 2 = Nein)
    fk = sample(1:2, n, replace = TRUE, prob = c(0.25, 0.75)),
    doku = sample(1:2, n, replace = TRUE, prob = c(0.6, 0.4)),
    doku_2 = sample(1:2, n, replace = TRUE, prob = c(0.6, 0.4)),
    talk = sample(1:2, n, replace = TRUE, prob = c(0.75, 0.25)),
    talk_frequency = sample(1:4, n, replace = TRUE),
    fulltime = sample(1:2, n, replace = TRUE, prob = c(0.65, 0.35)),
    
    # Zeiterfassung
    vaz_01 = sample(1:2, n, replace = TRUE, prob = c(0.3, 0.7)),
    vaz_02 = sample(1:2, n, replace = TRUE, prob = c(0.2, 0.8)),
    vaz_01a = sample(1:3, n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    
    # Homeoffice
    flexwork_1 = rnorm(n, mean = 35, sd = 25) %>% pmax(0) %>% pmin(100),
    
    # Anlaufstelle
    consult = sample(1:3, n, replace = TRUE, prob = c(0.5, 0.2, 0.3)),
    
    # Arbeitsbedingungen (Skalen 1-5)
    cond_auton_02 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_03 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_04 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_05 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_06 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_07 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_08 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_09 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_10 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_11 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_12 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    cond_auton_13 = rnorm(n, 3.5, 0.8) %>% pmax(1) %>% pmin(5),
    
    cond_res_01 = rnorm(n, 3.3, 0.9) %>% pmax(1) %>% pmin(5),
    cond_res_02 = rnorm(n, 3.3, 0.9) %>% pmax(1) %>% pmin(5),
    cond_res_03 = rnorm(n, 3.3, 0.9) %>% pmax(1) %>% pmin(5),
    
    cond_dem_01 = rnorm(n, 2.8, 1.0) %>% pmax(1) %>% pmin(5),
    cond_dem_02 = rnorm(n, 2.8, 1.0) %>% pmax(1) %>% pmin(5),
    cond_dem_03 = rnorm(n, 2.8, 1.0) %>% pmax(1) %>% pmin(5),
    
    # Coping
    cope_seso_01 = rnorm(n, 3.4, 0.8) %>% pmax(1) %>% pmin(5),
    cope_seso_02 = rnorm(n, 3.4, 0.8) %>% pmax(1) %>% pmin(5),
    cope_seso_03 = rnorm(n, 3.4, 0.8) %>% pmax(1) %>% pmin(5),
    
    cope_isg_01 = rnorm(n, 2.5, 0.9) %>% pmax(1) %>% pmin(5),
    cope_isg_02 = rnorm(n, 2.5, 0.9) %>% pmax(1) %>% pmin(5),
    cope_isg_03 = rnorm(n, 2.5, 0.9) %>% pmax(1) %>% pmin(5),
    
    # Wohlbefinden
    wbpos_engage_01 = rnorm(n, 3.8, 0.7) %>% pmax(1) %>% pmin(5),
    wbpos_engage_02 = rnorm(n, 3.8, 0.7) %>% pmax(1) %>% pmin(5),
    wbpos_engage_03 = rnorm(n, 3.8, 0.7) %>% pmax(1) %>% pmin(5),
    
    wbneg_ldb_01 = rnorm(n, 2.6, 1.0) %>% pmax(1) %>% pmin(5),
    wbneg_ldb_02 = rnorm(n, 2.6, 1.0) %>% pmax(1) %>% pmin(5),
    wbneg_ldb_03 = rnorm(n, 2.6, 1.0) %>% pmax(1) %>% pmin(5),
    
    wbneg_fws_01 = rnorm(n, 2.4, 0.9) %>% pmax(1) %>% pmin(5),
    wbneg_fws_02 = rnorm(n, 2.4, 0.9) %>% pmax(1) %>% pmin(5),
    wbneg_fws_03 = rnorm(n, 2.4, 0.9) %>% pmax(1) %>% pmin(5)
  )
  
  # Simulierte Organisationsdaten
  df_orga <- tibble(
    importKey = 1:15,
    organization = paste("Bank", LETTERS[1:15])
  )
  
  df_oe <- tibble(
    unit = as.character(1:9),
    `In welcher Organisationseinheit arbeiten Sie?` = c(
      "Retail Banking",
      "Private Banking",
      "Corporate, Commercial & Investment Banking",
      "Credit & Risk Management",
      "Informatics & IT",
      "Research & Product Development",
      "Backoffice",
      "Staff departments (human resources, legal services, compliance, logistics, etc.)",
      "Other"
    )
  )
  
  df_region <- tibble(
    region = as.character(1:5),
    `In welcher Region arbeiten Sie?` = c(
      "Zürich",
      "Bern",
      "Basel",
      "Genf",
      "Andere"
    )
  )
  
  # Joins
  df <- df %>%
    left_join(df_orga %>% select(importKey, organization), by = c("orga" = "importKey")) %>%
    left_join(df_oe, by = c("unit" = "unit")) %>%
    left_join(df_region, by = c("region" = "region")) %>%
    rename(
      orga_unit_name = `In welcher Organisationseinheit arbeiten Sie?`,
      region_name = `In welcher Region arbeiten Sie?`,
      bank_name = organization
    )
  
  return(df)
}

# ============================================================================
# MAIN DATA PROCESSING FUNCTION
# Übernommen aus dem Original-Skript
# ============================================================================

process_data <- function(df) {
  
  cat("=== DATENVERARBEITUNG STARTET ===\n")
  cat("Anzahl Zeilen:", nrow(df), "\n")
  cat("Anzahl Spalten:", ncol(df), "\n")
  
  # Prüfe wichtige Spalten
  required_cols <- c("orga", "unit", "region", "waveNumber", "age", "gender")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("FEHLER: Folgende Spalten fehlen: ", paste(missing_cols, collapse = ", "))
  }
  
  cat("✓ Alle wichtigen Spalten vorhanden\n")
  
  # Zeige waveNumber Werte
  if ("waveNumber" %in% names(df)) {
    cat("WaveNumber Werte:", paste(unique(df$waveNumber), collapse = ", "), "\n")
  }
  
  # ---- Define time tracking variable ----
  df <- df %>% 
    mutate(
      time_tracking = case_when(
        vaz_01 == 2 | vaz_02 == 1 | vaz_01a == 1 | vaz_01a == 3 ~ 1, 
        TRUE ~ 0
      ),
      time_tracking_type = case_when(
        time_tracking == 0 ~ 0,
        (vaz_01a == 1 | vaz_01 == 2) ~ 1,
        (vaz_01a == 3 | vaz_02 == 1) ~ 2,
        TRUE ~ 99
      )
    ) %>%
    # Remove individuals with "vereinfachte Zeiterfassung"
    filter(time_tracking_type != 2)
  
  # ---- Define doku variable ----
  df <- df %>% 
    mutate(
      doku = case_when(
        doku == 1 | doku_2 == 1 ~ 1,
        doku == 2 | doku_2 == 2 ~ 2,
        TRUE ~ NA_real_
      )
    )
  
  # ---- Define homeoffice variables ----
  df <- df %>% 
    mutate(
      ho1 = case_when(
        flexwork_1 <= 40 ~ "<=40%",
        flexwork_1 > 40 ~ ">40%",
        .default = NA_character_
      ),
      ho2 = case_when(
        flexwork_1 == 0 ~ "0%",
        flexwork_1 > 0 & flexwork_1 <= 40 ~ "<=40%",
        flexwork_1 > 40 ~ ">40%",
        .default = NA_character_
      ),
      homeoffice = case_when(
        flexwork_1 < 10 ~ "<10%",
        flexwork_1 >= 10 & flexwork_1 < 20 ~ "<20%",
        flexwork_1 >= 20 & flexwork_1 < 30 ~ "<30%",
        flexwork_1 >= 30 & flexwork_1 < 40 ~ "<40%",
        flexwork_1 >= 40 & flexwork_1 < 50 ~ "<50%",
        flexwork_1 >= 50 & flexwork_1 < 60 ~ "<60%",
        flexwork_1 >= 60 & flexwork_1 < 70 ~ "<70%",
        flexwork_1 >= 70 & flexwork_1 < 80 ~ "<80%",
        flexwork_1 >= 80 & flexwork_1 < 90 ~ "<90%",
        flexwork_1 >= 90 ~ ">=90%",
        TRUE ~ NA_character_
      )
    )
  
  # ---- Define consult dummy variables ----
  df <- df %>% 
    mutate(
      consult_yes = case_when(consult == 1 ~ 1, TRUE ~ 0),
      consult_donotknow = case_when(consult == 3 ~ 1, TRUE ~ 0)
    )
  
  # ---- Calculate scale means ----
  df <- df %>% 
    mutate(
      cond_auton_m = rowMeans(select(., cond_auton_02:cond_auton_13), na.rm = TRUE),
      cond_res_m = rowMeans(select(., starts_with("cond_res_")), na.rm = TRUE),
      cond_dem_m = rowMeans(select(., starts_with("cond_dem_")), na.rm = TRUE),
      cope_seso_m = rowMeans(select(., starts_with("cope_seso_")), na.rm = TRUE),
      cope_isg_m = rowMeans(select(., starts_with("cope_isg_")), na.rm = TRUE),
      wbpos_engage_m = rowMeans(select(., starts_with("wbpos_engage_")), na.rm = TRUE),
      wbneg_ldb_m = rowMeans(select(., starts_with("wbneg_ldb_")), na.rm = TRUE),
      wbneg_fws_m = rowMeans(select(., starts_with("wbneg_fws_")), na.rm = TRUE)
    )
  
  # ---- Recode binary variables from (1,2) to (0,1) ----
  # WICHTIG: Original-Kodierung ist 1=Ja, 2=Nein
  # Für Regression brauchen wir: 1=Ja, 0=Nein
  df <- df %>% 
    mutate(
      fk = 2 - fk,          # 1 (Ja, FK) → 1, 2 (Nein) → 0
      talk = 2 - talk,      # 1 (Ja) → 1, 2 (Nein) → 0
      doku = 2 - doku,      # 1 (Ja) → 1, 2 (Nein) → 0
      fulltime = 2 - fulltime  # 1 (Ja, 90-100%) → 1, 2 (Nein) → 0
    )
  
  # ---- Korrigiere Alters-Codierung ----
  # Falls age invertiert codiert ist (1=älteste statt 1=jüngste), korrigiere:
  # Prüfe ob Mittelwert-Test sinnvoll ist, ansonsten invertiere
  # WICHTIG: Diese Zeile nur für ECHTE Daten aktivieren, nicht für simulierte!
  # Für echte Daten: Entferne das # vor der nächsten Zeile
  # df <- df %>% mutate(age = 7 - age)  # Invertiert: 1->6, 2->5, 3->4, 4->3, 5->2, 6->1
  
  # ---- Create gender dummy ----
  df <- df %>% 
    mutate(
      gender_female = case_when(
        gender == 1 ~ 1,   # Frauen
        gender == 2 ~ 0,   # Männer (Referenz)
        TRUE ~ NA_real_    # Divers (3) und Keine Angabe (4) → NA
      )
    )
  # HINWEIS: Divers (3) und Keine Angabe (4) sind in deskriptiven Statistiken sichtbar,
  # werden aber bei Inferenzstatistik (MLM) durch NA in gender_female automatisch ausgeschlossen
  
  # ---- Standardize continuous/ordinal predictors ----
  df <- df %>% 
    group_by(orga) %>%
    mutate(
      # Numeric homeoffice
      homeoffice_new = case_when(
        homeoffice == "<10%" ~ 0,
        homeoffice == "<20%" ~ 1,
        homeoffice == "<30%" ~ 2,
        homeoffice == "<40%" ~ 3,
        homeoffice == "<50%" ~ 4,
        homeoffice == "<60%" ~ 5,
        homeoffice == "<70%" ~ 6,
        homeoffice == "<80%" ~ 7,
        homeoffice == "<90%" ~ 8,
        homeoffice == ">=90%" ~ 9,
        TRUE ~ NA_real_
      ),
      
      # Standardize continuous variables
      age_std = (age - mean(age, na.rm = TRUE)) / sd(age, na.rm = TRUE),
      talk_frequency_std = (talk_frequency - mean(talk_frequency, na.rm = TRUE)) / sd(talk_frequency, na.rm = TRUE),
      homeoffice_new_std = (homeoffice_new - mean(homeoffice_new, na.rm = TRUE)) / sd(homeoffice_new, na.rm = TRUE),
      
      # Standardize work conditions
      cond_auton_m_std = (cond_auton_m - mean(cond_auton_m, na.rm = TRUE)) / sd(cond_auton_m, na.rm = TRUE),
      cond_res_m_std = (cond_res_m - mean(cond_res_m, na.rm = TRUE)) / sd(cond_res_m, na.rm = TRUE),
      cond_dem_m_std = (cond_dem_m - mean(cond_dem_m, na.rm = TRUE)) / sd(cond_dem_m, na.rm = TRUE),
      cope_seso_m_std = (cope_seso_m - mean(cope_seso_m, na.rm = TRUE)) / sd(cope_seso_m, na.rm = TRUE),
      cope_isg_m_std = (cope_isg_m - mean(cope_isg_m, na.rm = TRUE)) / sd(cope_isg_m, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # ---- Filter organizations with >= 10 observations ----
  ids_enough_obs <- df %>%
    group_by(orga) %>%
    count() %>%
    ungroup() %>%
    filter(n >= 10) %>%
    pull(orga)
  
  # Finde die aktuellste Welle (falls "2024/2025" nicht existiert)
  if ("waveNumber" %in% names(df)) {
    available_waves <- unique(df$waveNumber)
    cat("Verfügbare Wellen:", paste(available_waves, collapse = ", "), "\n")
    
    # Versuche die neueste Welle zu finden
    current_wave <- if ("2024/2025" %in% available_waves) {
      "2024/2025"
    } else if (length(available_waves) > 0) {
      # Nimm die letzte Welle alphabetisch (oft die neueste)
      sort(available_waves, decreasing = TRUE)[1]
    } else {
      NULL
    }
    
    cat("Verwende Welle:", current_wave, "\n")
  } else {
    current_wave <- NULL
    cat("⚠ Keine waveNumber Spalte gefunden - verwende alle Daten\n")
  }
  
  ids_enough_obs_currwave <- if (!is.null(current_wave)) {
    df %>%
      filter(waveNumber == current_wave) %>%
      group_by(orga) %>%
      count() %>%
      ungroup() %>%
      filter(n >= 10) %>%
      pull(orga)
  } else {
    ids_enough_obs  # Verwende gleiche IDs wenn keine Welle gefunden
  }
  
  # ---- Create final datasets ----
  df_all_waves <- df %>%
    filter(
      !is.na(orga),
      orga != "-1",
      orga %in% ids_enough_obs
    )
  
  df_wave3 <- if (!is.null(current_wave)) {
    df %>%
      filter(
        waveNumber == current_wave,
        !is.na(orga),
        orga != "-1",
        orga %in% ids_enough_obs_currwave
      )
  } else {
    # Falls keine Welle gefunden, verwende alle Daten
    cat("⚠ Verwende alle Daten für 'aktuelle Welle'\n")
    df_all_waves
  }
  
  cat("\n=== DATENVERARBEITUNG ABGESCHLOSSEN ===\n")
  cat("df_all_waves:", nrow(df_all_waves), "Zeilen\n")
  cat("df_wave3:", nrow(df_wave3), "Zeilen\n")
  cat("=====================================\n\n")
  
  return(list(
    df_all_waves = df_all_waves,
    df_wave3 = df_wave3
  ))
}

# ============================================================================
# LOAD REAL DATA
# Pfade gehen ein Verzeichnis höher (../) weil App im Shinyapp/ Ordner läuft
# ============================================================================

load_real_data <- function() {
  
  # Load raw data
  df <- read_delim(
    file = "../02_data/raw/Baseline-Survey.csv"
  )
  
  df_orga <- read_xlsx(
    path = "../02_data/raw/20250916_keys_orgs_orgseinheit_region.xlsx",
    sheet = "import_organizations"
  ) %>%
    drop_na()
  
  df_oe <- read_xlsx(
    path = "../02_data/raw/20250916_keys_orgs_orgseinheit_region.xlsx",
    sheet = "import_organisationseinheit"
  )
  
  df_region <- read_xlsx(
    path = "../02_data/raw/20250916_keys_orgs_orgseinheit_region.xlsx",
    sheet = "import_region"
  )
  
  # Joins
  df <- df %>%
    left_join(df_orga %>% select(importKey, organization), by = c("orga" = "importKey")) %>%
    left_join(df_oe, by = c("unit" = "unit")) %>%
    left_join(df_region, by = c("region" = "region")) %>%
    rename(
      orga_unit_name = `In welcher Organisationseinheit arbeiten Sie?`,
      region_name = `In welcher Region arbeiten Sie?`,
      bank_name = organization
    )
  
  return(df)
}