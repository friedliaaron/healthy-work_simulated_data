# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(nlme)
library(performance)
library(emmeans)
library(kableExtra)
library(DT)
library(viridis)
library(corrplot)
library(Hmisc)

# ============================================================================
# FREQUENCY TABLES
# ============================================================================

create_frequency_table <- function(df, variable) {
  
  label_map <- list(
    age = c(
      "1" = "< 21 Jahre", "2" = "21 - 30 Jahre", "3" = "31 - 40 Jahre",
      "4" = "41 - 50 Jahre", "5" = "51 - 60 Jahre", "6" = "> 60 Jahre"
    ),
    gender = c(
      "1" = "Weiblich", "2" = "Männlich", "3" = "Divers", "4" = "Keine Angabe"
    ),
    fk       = c("0" = "Nein", "1" = "Ja"),
    doku     = c("0" = "Nein", "1" = "Ja"),
    talk     = c("0" = "Nein", "1" = "Ja"),
    fulltime = c("0" = "Nein", "1" = "Ja (90-100%)"),
    consult  = c("1" = "ja", "2" = "nein", "3" = "weiss nicht"),
    time_tracking_type = c(
      "0" = "Keine Zeiterfassung",
      "1" = "Reguläre Zeiterfassung",
      "2" = "Vereinfachte Zeiterfassung"
    )
  )
  
  freq_table <- df %>%
    select(all_of(variable)) %>%
    pull(1) %>%
    as.character() %>%
    table() %>%
    as.data.frame() %>%
    rename(Value = 1, Haeufigkeit = Freq) %>%
    mutate(Haeufigkeit_Prozent = round((Haeufigkeit / sum(Haeufigkeit)) * 100, 2))
  
  if (variable %in% names(label_map)) {
    freq_table <- freq_table %>%
      mutate(Variable = label_map[[variable]][as.character(Value)]) %>%
      select(Variable, Haeufigkeit, Haeufigkeit_Prozent)
  } else {
    freq_table <- freq_table %>% rename(Variable = Value)
  }
  
  freq_table %>%
    DT::datatable(options = list(pageLength = 10, dom = 't', ordering = FALSE), rownames = FALSE)
}

# ============================================================================
# BOXPLOT FUNCTION
# ============================================================================

plot_scale_boxplots <- function(data, group_var) {
  
  var_labels <- c(
    "age" = "Altersgruppe", "gender" = "Geschlecht", "fk" = "Führungskraft",
    "doku" = "Dokumentation", "talk" = "Gespräche", "fulltime" = "Vollzeitarbeit",
    "homeoffice" = "Homeoffice", "time_tracking_type" = "Zeiterfassung",
    "consult" = "Anlaufstelle", "orga_unit_name" = "Organisationseinheit",
    "region_name" = "Region"
  )
  
  legend_title <- var_labels[group_var]
  if (is.na(legend_title)) legend_title <- group_var
  
  # --- Recode group variable to labels ---
  plot_data <- data %>%
    filter(!is.na(.data[[group_var]])) %>%
    mutate(group_label = case_when(
      group_var == "age" & .data[[group_var]] == 1 ~ "< 21 Jahre",
      group_var == "age" & .data[[group_var]] == 2 ~ "21 - 30 Jahre",
      group_var == "age" & .data[[group_var]] == 3 ~ "31 - 40 Jahre",
      group_var == "age" & .data[[group_var]] == 4 ~ "41 - 50 Jahre",
      group_var == "age" & .data[[group_var]] == 5 ~ "51 - 60 Jahre",
      group_var == "age" & .data[[group_var]] == 6 ~ "> 60 Jahre",
      group_var == "gender" & .data[[group_var]] == 1 ~ "Weiblich",
      group_var == "gender" & .data[[group_var]] == 2 ~ "Männlich",
      group_var == "gender" & .data[[group_var]] == 3 ~ "Divers",
      group_var == "gender" & .data[[group_var]] == 4 ~ "Keine Angabe",
      group_var == "fk" & .data[[group_var]] == 1 ~ "Ja",
      group_var == "fk" & .data[[group_var]] == 0 ~ "Nein",
      group_var == "doku" & .data[[group_var]] == 1 ~ "Ja",
      group_var == "doku" & .data[[group_var]] == 0 ~ "Nein",
      group_var == "talk" & .data[[group_var]] == 1 ~ "Ja",
      group_var == "talk" & .data[[group_var]] == 0 ~ "Nein",
      group_var == "fulltime" & .data[[group_var]] == 1 ~ "Ja (90-100%)",
      group_var == "fulltime" & .data[[group_var]] == 0 ~ "Nein",
      group_var == "time_tracking_type" & .data[[group_var]] == 0 ~ "Keine Zeiterfassung",
      group_var == "time_tracking_type" & .data[[group_var]] == 1 ~ "Reguläre Zeiterfassung",
      group_var == "time_tracking_type" & .data[[group_var]] == 2 ~ "Vereinfachte Zeiterfassung",
      group_var == "consult" & .data[[group_var]] == 1 ~ "ja",
      group_var == "consult" & .data[[group_var]] == 2 ~ "nein",
      group_var == "consult" & .data[[group_var]] == 3 ~ "weiss nicht",
      TRUE ~ as.character(.data[[group_var]])
    )) %>%
    select(group_label, cond_auton_m, cond_res_m, cond_dem_m,
           cope_seso_m, cope_isg_m, wbpos_engage_m, wbneg_ldb_m, wbneg_fws_m) %>%
    pivot_longer(!group_label, names_to = "Scale", values_to = "Skalenmittelwert")
  
  # --- Set factor levels AFTER pivot on the simple column name ---
  if (group_var == "age") {
    plot_data$group_label <- factor(plot_data$group_label,
                                    levels = c("< 21 Jahre", "21 - 30 Jahre", "31 - 40 Jahre",
                                               "41 - 50 Jahre", "51 - 60 Jahre", "> 60 Jahre"))
  }
  
  # --- Build colour scale ---
  if (group_var == "age") {
    fill_scale <- scale_fill_manual(
      name   = legend_title,
      values = c(
        "< 21 Jahre"    = "#A6CEE3",
        "21 - 30 Jahre" = "#1F78B4",
        "31 - 40 Jahre" = "#B2DF8A",
        "41 - 50 Jahre" = "#33A02C",
        "51 - 60 Jahre" = "#FB9A99",
        "> 60 Jahre"    = "#E31A1C"
      ),
      breaks = c("< 21 Jahre", "21 - 30 Jahre", "31 - 40 Jahre",
                 "41 - 50 Jahre", "51 - 60 Jahre", "> 60 Jahre")
    )
  } else {
    fill_scale <- scale_fill_brewer(name = legend_title, palette = "Paired")
  }
  
  ggplot(plot_data, aes(x = Scale, y = Skalenmittelwert, fill = group_label)) +
    geom_boxplot(alpha = 0.7, color = "grey30", outlier.alpha = 0.4) +
    fill_scale +
    scale_x_discrete(labels = c(
      "cond_auton_m"   = "Autonomie",
      "cond_res_m"     = "Ressourcen",
      "cond_dem_m"     = "Stressoren",
      "cope_seso_m"    = "Selbstsorge",
      "cope_isg_m"     = "Interess.\nSelbstgef.",
      "wbpos_engage_m" = "Pos. WB:\nEngagement",
      "wbneg_ldb_m"    = "Neg. WB:\n(Fehlende)\nLife-Domain-Balance",
      "wbneg_fws_m"    = "Neg. WB:\nErschoepfung"
    )) +
    theme_minimal() +
    theme(
      axis.text.x        = element_text(size = 9, vjust = 1),
      legend.title       = element_text(size = 9),
      legend.text        = element_text(size = 8),
      legend.position    = "top",
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    labs(x = NULL, y = "Skalenmittelwert")
}

# ============================================================================
# ICC CALCULATION
# ============================================================================

calculate_icc_table <- function(df) {
  
  outcomes <- c("cond_auton_m", "cond_res_m", "cond_dem_m",
                "cope_seso_m", "cope_isg_m",
                "wbpos_engage_m", "wbneg_ldb_m", "wbneg_fws_m")
  
  icc_results <- map_df(outcomes, function(outcome) {
    model     <- lmer(as.formula(paste0(outcome, " ~ 1 + (1 | orga)")), data = df, REML = FALSE)
    icc_value <- performance::icc(model)[[1]]
    p_value   <- lmerTest::ranova(model)[2, "Pr(>Chisq)"]
    tibble(Themenfeld = outcome, ICC = round(icc_value, 3), `p-Wert` = round(p_value, 4))
  })
  
  icc_results %>%
    mutate(Themenfeld = case_when(
      Themenfeld == "cond_auton_m"   ~ "Autonomie",
      Themenfeld == "cond_res_m"     ~ "Ressourcen",
      Themenfeld == "cond_dem_m"     ~ "Stressoren",
      Themenfeld == "cope_seso_m"    ~ "Selbstsorge",
      Themenfeld == "cope_isg_m"     ~ "Interessierte Selbstgefährdung",
      Themenfeld == "wbpos_engage_m" ~ "Pos. WB: Engagement",
      Themenfeld == "wbneg_ldb_m"    ~ "Neg. WB: Life-Domain-Balance",
      Themenfeld == "wbneg_fws_m"    ~ "Neg. WB: Erschöpfung",
      TRUE ~ Themenfeld
    )) %>%
    DT::datatable(options = list(pageLength = 10, dom = 't', ordering = FALSE), rownames = FALSE) %>%
    DT::formatStyle('p-Wert', backgroundColor = DT::styleInterval(0.05, c('#ffcccc', 'white')))
}

# ============================================================================
# MLM TABLE
# ============================================================================

create_mlm_table <- function(model, av_name, level_significance = 0.05, color_sig_p5perc = "#b81414") {

  is_fallback <- !is.null(attr(model, "is_fallback")) && attr(model, "is_fallback")
  n_obs  <- nobs(model)
  n_orga <- if (is_fallback) "N/A" else length(unique(model$groups$orga))

  if (is_fallback) {
    res_df <- as.data.frame(summary(model)$coefficients)
    colnames(res_df) <- c("Value", "Std.Error", "t-value", "p-value")
    res_df$DF <- model$df.residual
    caption_text <- paste0(
      "⚠ Einfaches Regressionsmodell (MLM fehlgeschlagen): ", av_name,
      " (N = ", n_obs, ")"
    )
  } else {
    res_df       <- as.data.frame(summary(model)$tTable)
    caption_text <- paste0("Modell für ", av_name, " (N = ", n_obs, ", ", n_orga, " Organisationen)")
  }

  res_df  <- round(res_df, 3)
  is_sig  <- res_df$`p-value` < level_significance & rownames(res_df) != "(Intercept)"
  names(is_sig) <- rownames(res_df)

  var_labels <- c(
    "fk" = "Führungskräfte", "doku" = "Personen mit selbstständiger Zeitdokumentation",
    "talk" = "Personen mit regelmäßigen MA-Gesprächen",
    "talk_frequency_std" = "Häufigere MA-Gespräche",
    "time_tracking" = "Personen mit offizieller Zeiterfassung",
    "fulltime" = "Vollzeitbeschäftigte (vs. Teilzeit)",
    "homeoffice_new_std" = "Mehr Homeoffice", "age_std" = "Ältere Personen",
    "gender_female" = "Frauen (vs. Männer)", "consult_yes" = "Personen mit Anlaufstelle",
    "consult_donotknow" = "Personen, die nicht wissen ob Anlaufstelle existiert",
    "waveNumber" = "Spätere Studienwellen"
  )

  res_df$Interpretation <- sapply(rownames(res_df), function(v) {
    if (!is_sig[v] || v == "(Intercept)") return("-")
    label <- ifelse(v %in% names(var_labels), var_labels[v], v)
    if (res_df[v, "Value"] > 0) paste0(label, " haben höhere ", av_name)
    else paste0(label, " haben tiefere ", av_name)
  })

  res_df %>%
    rownames_to_column("Variable") %>%
    DT::datatable(
      caption = caption_text,
      options = list(pageLength = 15, dom = 't', ordering = FALSE), rownames = FALSE
    ) %>%
    DT::formatStyle('p-value',
                    color      = DT::styleInterval(level_significance, c(color_sig_p5perc, 'black')),
                    fontWeight = DT::styleInterval(level_significance, c('bold', 'normal'))
    )
}

# ============================================================================
# MULTILEVEL MODEL FUNCTION
# ============================================================================

run_mlm <- function(data, outcome, include_time_tracking = TRUE, include_wave = FALSE) {
  
  predictors <- c("fk", "talk", "age_std", "gender_female", "fulltime",
                  "homeoffice_new_std", "consult_yes", "consult_donotknow")
  if (include_time_tracking) predictors <- c(predictors, "time_tracking")
  if (include_wave)          predictors <- c(predictors, "waveNumber")
  
  formula_str <- paste0(outcome, " ~ ", paste(predictors, collapse = " + "))
  n_orgs <- length(unique(data$orga[!is.na(data$orga)]))
  n_obs  <- nrow(data[complete.cases(data[, c(outcome, predictors)]), ])
  
  cat("\n=== MODELL-DIAGNOSE ===\n")
  cat("Outcome:", outcome, "\n")
  cat("Anzahl Organisationen:", n_orgs, "\n")
  cat("Vollständige Fälle:", n_obs, "\n")
  if (n_orgs < 5) warning("Nur ", n_orgs, " Organisationen - Multilevel-Modell könnte instabil sein")
  
  model <- tryCatch({
    nlme::lme(fixed = as.formula(formula_str), random = ~ 1 | orga, data = data,
              na.action = na.exclude,
              control = nlme::lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100))
  }, error = function(e) {
    cat("Fallback auf lm:", e$message, "\n")
    lm_model <- lm(as.formula(paste0(outcome, " ~ ", paste(predictors, collapse = " + "))),
                   data = data, na.action = na.exclude)
    attr(lm_model, "is_fallback")     <- TRUE
    attr(lm_model, "error_message")   <- e$message
    lm_model
  })
  cat("======================\n\n")
  return(model)
}

# ============================================================================
# FORMAT MLM TABLE
# ============================================================================

format_mlm_table <- function(model, level_significance = 0.05) {
  
  is_fallback <- !is.null(attr(model, "is_fallback")) && attr(model, "is_fallback")
  
  if (is_fallback) {
    res_df <- as.data.frame(summary(model)$coefficients)
    colnames(res_df) <- c("Value", "Std.Error", "t-value", "p-value")
    res_df$DF <- model$df.residual
  } else {
    res_df <- as.data.frame(summary(model)$tTable)
  }
  res_df  <- round(res_df, 3)
  is_sig  <- res_df$`p-value` < level_significance & rownames(res_df) != "(Intercept)"
  names(is_sig) <- rownames(res_df)
  
  var_labels <- c(
    "fk" = "Führungskräfte", "talk" = "Personen mit regelmäßigen MA-Gesprächen",
    "time_tracking" = "Personen mit offizieller Zeiterfassung",
    "fulltime" = "Vollzeitbeschäftigte (vs. Teilzeit)",
    "homeoffice_new_std" = "Mehr Homeoffice", "age_std" = "Ältere Personen",
    "gender_female" = "Frauen (vs. Männer)", "consult_yes" = "Personen mit Anlaufstelle",
    "consult_donotknow" = "Personen, die nicht wissen ob Anlaufstelle existiert"
  )
  
  res_df$Interpretation <- sapply(rownames(res_df), function(v) {
    if (!is_sig[v] || v == "(Intercept)") return("-")
    label <- ifelse(v %in% names(var_labels), var_labels[v], v)
    if (res_df[v, "Value"] > 0) paste0(label, " haben höhere Werte")
    else paste0(label, " haben tiefere Werte")
  })
  
  caption_text <- if (is_fallback) paste0("⚠ Einfaches lineares Modell: ", attr(model, "error_message"))
  else "Multilevel-Modell Ergebnisse"
  
  res_df %>%
    rownames_to_column("Prädiktor") %>%
    DT::datatable(caption = caption_text,
                  options = list(pageLength = 15, dom = 't', ordering = FALSE), rownames = FALSE) %>%
    DT::formatStyle('Prädiktor', target = 'row',
                    backgroundColor = DT::styleEqual(names(is_sig)[is_sig], rep('#ffcccc', sum(is_sig))))
}

# ============================================================================
# GENERATE INTERPRETATION
# ============================================================================

generate_interpretation <- function(model, outcome_name) {
  
  outcome_labels <- c(
    "cond_auton_m" = "Autonomie", "cond_res_m" = "Ressourcen",
    "cond_dem_m" = "Stressoren", "cope_seso_m" = "Selbstsorge",
    "cope_isg_m" = "Interessierte Selbstgefährdung",
    "wbpos_engage_m" = "Engagement", "wbneg_ldb_m" = "Life-Domain-Balance",
    "wbneg_fws_m" = "Erschöpfung"
  )
  label       <- outcome_labels[outcome_name]
  if (is.na(label)) label <- outcome_name
  is_fallback <- !is.null(attr(model, "is_fallback")) && attr(model, "is_fallback")
  n_obs       <- nobs(model)
  n_orga      <- if (is_fallback) "N/A" else length(unique(model$groups$orga))
  model_type  <- if (is_fallback) "Einfaches lineares Regressionsmodell" else "Multilevel-Modell"
  warning_text <- if (is_fallback) "<div style='background:#fff3cd;padding:10px;border-radius:5px;margin:10px 0;'><strong>⚠</strong> Multilevel-Modell konnte nicht berechnet werden.</div>" else ""
  
  HTML(paste0(warning_text,
              "<p><strong>Modell für: ", label, "</strong></p>",
              "<p>Modelltyp: ", model_type, "<br>N = ", n_obs, "<br>Organisationen: ", n_orga, "</p>",
              "<p>Signifikante Effekte (p < 0.05) sind farblich markiert.</p>"))
}

# ============================================================================
# COEFFICIENT PLOT
# ============================================================================

plot_coefficients <- function(model) {
  
  as.data.frame(summary(model)$tTable) %>%
    rownames_to_column("Variable") %>%
    filter(Variable != "(Intercept)") %>%
    mutate(sig = `p-value` < 0.05, lower = Value - 1.96*Std.Error, upper = Value + 1.96*Std.Error) %>%
    ggplot(aes(x = reorder(Variable, Value), y = Value)) +
    geom_point(aes(color = sig), size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper, color = sig), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "#3c8dbc"), labels = c("n.s.", "p < 0.05")) +
    coord_flip() +
    labs(x = NULL, y = "Fixed Effect (β) mit 95% CI", color = "Signifikanz") +
    theme_minimal() + theme(legend.position = "top")
}

# ============================================================================
# ORG UNIT FUNCTIONS
# ============================================================================

run_org_unit_model <- function(data, outcome) {
  formula_str <- paste0(outcome,
                        " ~ fk + talk + time_tracking + age_std + gender_female + fulltime + ",
                        "homeoffice_new_std + consult_yes + consult_donotknow + orga_unit_name + (1 | bank_name)")
  lmer(as.formula(formula_str), data = data, na.action = na.exclude)
}

plot_org_emmeans <- function(model, outcome) {
  emmeans_df <- as.data.frame(emmeans(model, ~ orga_unit_name, lmerTest.limit = 3518))
  if (!"lower.CL" %in% names(emmeans_df))
    emmeans_df <- emmeans_df %>% mutate(lower.CL = emmean - 1.96*SE, upper.CL = emmean + 1.96*SE)
  grand_mean <- mean(emmeans_df$emmean)
  emmeans_df <- emmeans_df %>% mutate(orga_short = case_when(
    orga_unit_name == "Informatics & IT" ~ "IT",
    orga_unit_name == "Research & Product Development" ~ "R&D",
    orga_unit_name == "Backoffice" ~ "Backoffice",
    orga_unit_name == "Staff departments (human resources, legal services, compliance, logistics, etc.)" ~ "Staff Depts",
    orga_unit_name == "Other" ~ "Other",
    orga_unit_name == "Corporate, Commercial & Investment Banking" ~ "Corp Banking",
    orga_unit_name == "Credit & Risk Management" ~ "Risk Mgmt",
    orga_unit_name == "Retail Banking" ~ "Retail",
    orga_unit_name == "Private Banking" ~ "Private",
    TRUE ~ orga_unit_name
  ))
  ggplot(emmeans_df, aes(x = reorder(orga_short, emmean), y = emmean)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
    geom_hline(yintercept = grand_mean, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = paste0(outcome, " nach Organisationseinheit"),
         subtitle = "Geschätzte Mittelwerte. Rote Linie = Durchschnitt",
         x = "Organisationseinheit", y = outcome) +
    theme_minimal()
}

create_org_contrast_table <- function(model) {
  emmeans_df <- as.data.frame(emmeans(model, ~ orga_unit_name, lmerTest.limit = 3518))
  grand_mean <- mean(emmeans_df$emmean)
  emmeans_df %>%
    mutate(contrast = paste(orga_unit_name, "effect"), estimate = emmean - grand_mean,
           t.ratio = estimate / SE, p.value = 2 * pt(-abs(t.ratio), df)) %>%
    select(contrast, estimate, SE, df, t.ratio, p.value) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    arrange(desc(abs(estimate))) %>%
    DT::datatable(options = list(pageLength = 10, dom = 't', ordering = FALSE), rownames = FALSE) %>%
    DT::formatStyle('p.value', backgroundColor = DT::styleInterval(0.05, c('#ffcccc', 'white')))
}

# ============================================================================
# CORRELATION FUNCTIONS
# ============================================================================

create_correlation_plot <- function(df) {
  corr_data <- df %>%
    select(starts_with("cond_") & ends_with("_m"),
           starts_with("cope_") & ends_with("_m"),
           starts_with("wb")    & ends_with("_m")) %>%
    drop_na()
  corr_matrix <- Hmisc::rcorr(as.matrix(corr_data), type = "spearman")
  labels <- c(
    "cond_auton_m" = "Autonomie", "cond_res_m" = "Ressourcen", "cond_dem_m" = "Stressoren",
    "cope_seso_m" = "Selbstsorge", "cope_isg_m" = "Interessierte Selbstgefaehrdung",
    "wbpos_engage_m" = "Positives WB: Engagement",
    "wbneg_ldb_m" = "Negatives WB: (Fehlende) Life-Domain-Balance",
    "wbneg_fws_m" = "Negatives WB: Erschoepfung"
  )
  rownames(corr_matrix$r) <- labels[rownames(corr_matrix$r)]
  colnames(corr_matrix$r) <- labels[colnames(corr_matrix$r)]
  corrplot::corrplot(corr_matrix$r, tl.cex = 0.7, cl.cex = 0.7, tl.col = "#191919")
}

create_correlation_table <- function(df) {
  corr_data <- df %>%
    select(starts_with("cond_") & ends_with("_m"),
           starts_with("cope_") & ends_with("_m"),
           starts_with("wb")    & ends_with("_m")) %>%
    drop_na()
  labels <- c(
    "cond_auton_m" = "Autonomie", "cond_res_m" = "Ressourcen", "cond_dem_m" = "Stressoren",
    "cope_seso_m" = "Selbstsorge", "cope_isg_m" = "Interess. Selbstgef.",
    "wbpos_engage_m" = "Pos. WB: Engagement", "wbneg_ldb_m" = "Neg. WB: Life-Domain-Balance",
    "wbneg_fws_m" = "Neg. WB: Erschoepfung"
  )
  cor(corr_data, method = "pearson") %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = labels[Variable]) %>%
    rename_with(~labels[.x], -Variable) %>%
    mutate_if(is.numeric, round, 2) %>%
    DT::datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
}

create_scatter_plot <- function(df, outcome) {
  label <- c("wbneg_fws_m" = "Erschöpfung", "wbpos_engage_m" = "Engagement")[outcome]
  p <- df %>%
    filter(!is.na(flexwork_1), !is.na(.data[[outcome]])) %>%
    ggplot(aes(x = flexwork_1, y = .data[[outcome]])) +
    geom_jitter(alpha = 0.4, width = 2) +
    geom_smooth(method = "loess", se = TRUE, color = "#3c8dbc", span = 0.5) +
    labs(x = "Homeoffice (%)", y = label) +
    theme_minimal()
  plotly::ggplotly(p)
}

# ============================================================================
# BENCHMARK FUNCTIONS
# ============================================================================

outcome_labels_bench <- c(
  "cond_auton_m"   = "Autonomie",
  "cond_res_m"     = "Ressourcen",
  "cond_dem_m"     = "Stressoren",
  "cope_seso_m"    = "Selbstsorge",
  "cope_isg_m"     = "Interessierte Selbstgef.",
  "wbpos_engage_m" = "Pos. WB: Engagement",
  "wbneg_ldb_m"    = "Neg. WB: LDB",
  "wbneg_fws_m"    = "Neg. WB: Erschöpfung"
)

run_benchmark_analysis <- function(df, focal_bank) {
  
  outcomes <- names(outcome_labels_bench)
  
  map_df(outcomes, function(outcome) {
    
    vals_focal  <- df %>% filter(bank_name == focal_bank) %>%
      pull(.data[[outcome]]) %>% na.omit()
    vals_others <- df %>% filter(bank_name != focal_bank) %>%
      pull(.data[[outcome]]) %>% na.omit()
    
    mean_others <- mean(vals_others)
    test        <- t.test(vals_focal, mu = mean_others)
    
    tibble(
      outcome   = outcome,
      Skala     = outcome_labels_bench[outcome],
      MW_Bank   = round(mean(vals_focal), 2),
      MW_Andere = round(mean_others, 2),
      Differenz = round(mean(vals_focal) - mean_others, 2),
      CI_low    = round(test$conf.int[1] - mean_others, 2),
      CI_high   = round(test$conf.int[2] - mean_others, 2),
      p_value   = round(test$p.value, 3),
      n_Bank    = length(vals_focal),
      sig       = test$p.value < 0.05
    )
  })
}

plot_benchmark_diverging <- function(results, focal_bank_name) {
  
  red_scales <- c("Stressoren", "Interessierte Selbstgef.",
                  "Neg. WB: LDB", "Neg. WB: Erschöpfung")
  
  plot_data <- results %>%
    select(Skala, MW_Bank, MW_Andere, sig, Differenz) %>%
    arrange(Differenz) %>%
    mutate(
      Skala       = factor(Skala, levels = Skala),
      line_color  = ifelse(Differenz >= 0, "#1D9E75", "#E24B4A"),
      label_color = ifelse(as.character(Skala) %in% red_scales, "#E24B4A", "#1D9E75")
    )
  
  axis_colors <- plot_data$label_color
  
  ggplot(plot_data) +
    geom_segment(
      aes(x = MW_Andere, xend = MW_Bank, y = Skala, yend = Skala),
      color     = plot_data$line_color,
      linewidth = 1.4,
      alpha     = 0.8
    ) +
    geom_point(aes(x = MW_Andere, y = Skala),
               color = "#888780", size = 4) +
    # Bank-Punkt in Farbe der Linie
    geom_point(aes(x = MW_Bank, y = Skala),
               color = plot_data$line_color, size = 4) +
    geom_text(
      data = filter(plot_data, sig),
      aes(x = pmax(MW_Bank, MW_Andere), y = Skala, label = "*"),
      hjust = -0.5, size = 5, color = "black"
    ) +
    scale_x_continuous(
      limits = c(1, 5), breaks = 1:5,
      labels = c("1\n(tief)", "2", "3", "4", "5\n(hoch)")
    ) +
    labs(
      x       = "Skalenmittelwert (1–5)",
      y       = NULL,
      caption = "Grauer Punkt = Alle anderen Banken  |  Farbiger Punkt = Ihre Bank  |  * p < 0.05"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_line(color = "grey92"),
      panel.grid.minor   = element_blank(),
      plot.caption       = element_text(size = 8, color = "grey50"),
      axis.text.y        = element_text(size = 10, color = axis_colors, face = "bold")
    )
}

plot_benchmark_radar <- function(results) {
  
  red_scales <- c("Stressoren", "Interessierte Selbstgef.",
                  "Neg. WB: LDB", "Neg. WB: Erschöpfung")
  
  plot_data <- results %>%
    select(Skala, Differenz, sig) %>%
    arrange(Differenz) %>%
    mutate(
      Skala       = factor(Skala, levels = Skala),
      is_negative_scale = as.character(Skala) %in% red_scales,
      # Inhaltlich korrekte Farbe:
      # positive Skalen: positiv = grün, negativ = rot
      # negative Skalen: negativ = grün (weniger = besser), positiv = rot
      bar_color = case_when(
        !is_negative_scale &  Differenz >= 0 ~ "#1D9E75",  # gut: über Schnitt
        !is_negative_scale &  Differenz <  0 ~ "#E24B4A",  # schlecht: unter Schnitt
        is_negative_scale &  Differenz <  0 ~ "#5DCAA5",  # gut: unter Schnitt (helleres grün)
        is_negative_scale &  Differenz >= 0 ~ "#F0997B"   # schlecht: über Schnitt (helleres rot)
      ),
      label_color = ifelse(is_negative_scale, "#E24B4A", "#1D9E75")
    )
  
  axis_colors <- plot_data$label_color
  
  ggplot(plot_data, aes(x = Differenz, y = Skala)) +
    geom_col(fill = plot_data$bar_color, alpha = 0.8) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
    geom_text(
      data = filter(plot_data, sig),
      aes(x = ifelse(Differenz >= 0, Differenz, Differenz),
          label = "*"),
      hjust = ifelse(filter(plot_data, sig)$Differenz >= 0, -0.5, 1.5),
      size = 5, color = "black"
    ) +
    labs(
      x       = "Differenz zum Mittelwert aller anderen Banken",
      y       = NULL,
      caption = "* p < 0.05"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.caption       = element_text(size = 8, color = "grey50"),
      axis.text.y        = element_text(size = 10, color = axis_colors, face = "bold")
    )
}

benchmark_results_table <- function(results) {
  results %>%
    select(Skala, MW_Bank, MW_Andere, Differenz, p_value, n_Bank) %>%
    rename(
      `MW Ihre Bank`    = MW_Bank,
      `MW Alle anderen` = MW_Andere,
      `Differenz`       = Differenz,
      `p-Wert`          = p_value,
      `n (Ihre Bank)`   = n_Bank
    ) %>%
    DT::datatable(
      options = list(pageLength = 10, dom = 't', ordering = FALSE),
      rownames = FALSE
    ) %>%
    DT::formatStyle(
      'p-Wert',
      color      = DT::styleInterval(0.05, c('#b81414', 'black')),
      fontWeight = DT::styleInterval(0.05, c('bold', 'normal'))
    ) %>%
    DT::formatStyle(
      'Differenz',
      color = DT::styleInterval(0, c('#E24B4A', '#1D9E75'))
    )
}