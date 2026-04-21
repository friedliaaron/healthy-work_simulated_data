# ============================================================================
# Healthy@Work Dashboard
# Interaktives Shiny Dashboard für Hintergrundanalysen
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)

# Set working directory to the app folder (VSCode compatibility)
if (!file.exists("helpers.R")) {
  app_dir <- tryCatch(
    dirname(rstudioapi::getActiveDocumentContext()$path),
    error = function(e) dirname(normalizePath(sys.frames()[[1]]$filename))
  )
  setwd(app_dir)
}

source("helpers.R")
source("data_processing.R")

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- dashboardPage(
  skin = "yellow",
  
  # ---- Header ----
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$span("Healthy@Work Analytics", style = "font-weight: bold; color: #000000;")
    ),
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.fhnw.ch",
        target = "_blank",
        tags$img(src = "fhnw.jpg", height = "40px",
                 style = "margin-top: 5px; margin-right: 15px;")
      )
    )
  ),
  
  # ---- Sidebar ----
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Überblick", tabName = "overview", icon = icon("home")),
      menuItem("Deskriptive Statistiken", tabName = "descriptives", icon = icon("chart-bar")),
      menuItem("Clustering & ICC", tabName = "clustering", icon = icon("layer-group")),
      menuItem("Inferenzstatistik", tabName = "inference", icon = icon("calculator")),
      menuItem("Prädiktor-Rechner", tabName = "predictor", icon = icon("sliders-h")),
      menuItem("Organisationseinheiten", tabName = "org_units", icon = icon("building")),
      menuItem("Zusammenhänge", tabName = "correlations", icon = icon("project-diagram")),
      menuItem("Banken-Benchmark", tabName = "benchmark", icon = icon("flag")),
      menuItem("Report Exportieren", tabName = "export", icon = icon("download"))
    ),
    
    hr(),
    h4("Globale Filter", style = "padding-left: 15px;"),
    
    conditionalPanel(
      condition = "input.tabs == 'inference' || input.tabs == 'predictor'",
      div(
        style = "background-color: #FDE70E; border: 2px solid #000000; padding: 10px; margin: 0 15px 15px 15px; border-radius: 3px;",
        HTML("<strong>⚠️ Filter inaktiv</strong><br>
              <small>Diese Analysen nutzen den vollständigen Datensatz. 
              Filter werden NICHT angewendet.</small>")
      )
    ),
    
    selectInput(
      "dataset_choice", "Datensatz:",
      choices = c("Welle 2024/2025" = "wave3", "Alle Wellen" = "all_waves"),
      selected = "wave3"
    ),
    
    conditionalPanel(
      condition = "input.dataset_choice == 'wave3'",
      selectizeInput(
        "bank_filter", "Banken:", choices = NULL, multiple = TRUE,
        options = list(placeholder = "Alle Banken")
      )
    ),
    
    sliderTextInput(
      "age_filter", "Altersgruppe:",
      choices = c("< 21", "21-30", "31-40", "41-50", "51-60", "> 60"),
      selected = c("< 21", "> 60"), grid = TRUE
    ),
    
    checkboxGroupInput(
      "gender_filter", "Geschlecht:",
      choices = c("Weiblich" = 1, "Männlich" = 2, "Divers" = 3, "Keine Angabe" = 4),
      selected = c(1, 2, 3, 4)
    ),
    
    hr(),
    
    actionButton("apply_filters", "Filter anwenden",
                 class = "btn-primary", icon = icon("filter"),
                 style = "width: 90%; margin-left: 5%; margin-right: 5%;")
  ),
  
  # ---- Body ----
  dashboardBody(
    
    tags$head(tags$style(HTML("
      :root {
        --fhnw-yellow: #FDE70E;
        --fhnw-black: #000000;
        --fhnw-grey: #767573;
        --fhnw-lightgrey: #F1F1EE;
      }
      .main-header .logo {
        background-color: var(--fhnw-yellow) !important;
        color: var(--fhnw-black) !important;
        font-weight: bold; font-size: 18px;
        border-bottom: 2px solid var(--fhnw-black);
      }
      .main-header .navbar {
        background-color: var(--fhnw-yellow) !important;
        border-bottom: 2px solid var(--fhnw-black);
      }
      .skin-yellow .main-sidebar { background-color: #ffffff !important; }
      .skin-yellow .sidebar-menu > li > a {
        color: var(--fhnw-black) !important;
        background-color: #ffffff !important;
      }
      .skin-yellow .sidebar-menu > li:hover > a {
        background-color: var(--fhnw-lightgrey) !important;
        color: var(--fhnw-black) !important;
      }
      .skin-yellow .sidebar-menu > li.active > a {
        border-left-color: var(--fhnw-yellow) !important;
        border-left-width: 4px !important;
        background-color: var(--fhnw-lightgrey) !important;
        color: var(--fhnw-black) !important;
        font-weight: bold;
      }
      .skin-yellow .sidebar-menu > li.header {
        background-color: #ffffff !important;
        color: var(--fhnw-black) !important;
      }
      .sidebar label { color: var(--fhnw-black) !important; }
      .box.box-solid.box-primary > .box-header,
      .box.box-solid.box-success > .box-header,
      .box.box-solid.box-info > .box-header {
        background: #ffffff !important;
        color: var(--fhnw-black) !important;
        font-weight: bold;
        border-bottom: 3px solid var(--fhnw-yellow) !important;
      }
      .btn-primary {
        background-color: var(--fhnw-yellow) !important;
        border-color: var(--fhnw-black) !important;
        color: var(--fhnw-black) !important;
        font-weight: bold;
      }
      .btn-primary:hover {
        background-color: #fbd100 !important;
        border-color: var(--fhnw-black) !important;
      }
      .btn-success {
        background-color: #ffffff !important;
        border: 2px solid var(--fhnw-black) !important;
        color: var(--fhnw-black) !important;
        font-weight: bold;
      }
      .btn-success:hover { background-color: var(--fhnw-lightgrey) !important; }
      .small-box {
        border-radius: 3px;
        border: 2px solid var(--fhnw-grey);
        background-color: #ffffff !important;
      }
      .small-box h3, .small-box p { color: var(--fhnw-black) !important; }
      .small-box .icon { color: rgba(0, 0, 0, 0.1) !important; }
      .info-box {
        min-height: 90px;
        border: 1px solid var(--fhnw-grey);
        background-color: #ffffff !important;
      }
      .nav-tabs-custom > .nav-tabs > li.active {
        border-top-color: var(--fhnw-yellow) !important;
        border-top-width: 3px !important;
      }
      .nav-tabs-custom > .nav-tabs > li.active > a {
        color: var(--fhnw-black) !important;
        font-weight: bold;
      }
      a { color: var(--fhnw-black) !important; text-decoration: underline; }
      a:hover { color: var(--fhnw-grey) !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: var(--fhnw-yellow) !important;
        border-color: var(--fhnw-black) !important;
        color: var(--fhnw-black) !important;
      }
      .irs-bar {
        background: var(--fhnw-yellow) !important;
        border-color: var(--fhnw-black) !important;
      }
      .irs-from, .irs-to, .irs-single {
        background: var(--fhnw-black) !important;
        color: #ffffff !important;
      }
      .box { border-top: 3px solid var(--fhnw-yellow); }
    "))),
    
    tabItems(
      
      # ---- TAB 1: Überblick ----
      tabItem(
        tabName = "overview",
        h2("Überblick über die Daten"),
        fluidRow(
          valueBoxOutput("total_n_box", width = 4),
          valueBoxOutput("total_banks_box", width = 4),
          valueBoxOutput("avg_per_bank_box", width = 4)
        ),
        fluidRow(
          box(title = "Datensatz-Information", status = "primary", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("dataset_info_table"))
        ),
        fluidRow(
          box(title = "Wichtige Hinweise", status = "info", solidHeader = TRUE, width = 12,
              HTML("<ul>
                <li>Nur Organisationen mit mindestens n = 10 Personen sind inkludiert</li>
                <li>Personen mit nicht-validen Bankenzugehörigkeiten wurden ausgeschlossen</li>
                <li>Personen mit 'vereinfachter Zeiterfassung' wurden gefiltert</li>
                <li><strong>Gender-Filterung:</strong> Deskriptive Statistiken zeigen alle Geschlechter.
                Inferenzstatistik (MLM) nutzt nur Männer und Frauen.</li>
              </ul>"))
        ),
        fluidRow(
          box(title = "Relevante Variablen", status = "primary", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("variable_overview_table"))
        )
      ),
      
      # ---- TAB 2: Deskriptive Statistiken ----
      tabItem(
        tabName = "descriptives",
        h2("Verteilung von Dashboard-Variablen"),
        fluidRow(
          box(title = "Variable auswählen", status = "primary", width = 3,
              selectInput("desc_variable", "Variable:",
                          choices = c("Alter" = "age", "Geschlecht" = "gender",
                                      "Führungskraft" = "fk", "Dokumentation" = "doku",
                                      "Gespräche" = "talk", "Vollzeitarbeit" = "fulltime",
                                      "Homeoffice" = "homeoffice", "Zeiterfassung" = "time_tracking_type",
                                      "Anlaufstelle" = "consult", "Organisationseinheit" = "orga_unit_name",
                                      "Region" = "region_name"))
          ),
          box(title = "Häufigkeitstabelle", status = "primary", solidHeader = TRUE, width = 9,
              DT::dataTableOutput("freq_table"))
        ),
        fluidRow(
          box(title = "Boxplots nach Skalen", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("boxplot_scales", height = "600px"))
        )
      ),
      
      # ---- TAB 3: Clustering & ICC ----
      tabItem(
        tabName = "clustering",
        h2("Clustering innerhalb von Banken"),
        fluidRow(
          box(title = "Intraclass Correlation Coefficients (ICC)", status = "primary",
              solidHeader = TRUE, width = 12,
              HTML("<p><strong>Kernfrage: Spielt es eine Rolle, bei welcher Bank man arbeitet?</strong></p>
                    <ul>
                      <li><strong>ICC ≥ 0.05:</strong> Spürbarer 'Banken-Effekt'</li>
                      <li><strong>p-Wert < 0.05:</strong> Der Banken-Effekt ist statistisch signifikant</li>
                    </ul>"),
              DT::dataTableOutput("icc_table"))
        )
      ),
      
      # ---- TAB 4: Inferenzstatistik ----
      tabItem(
        tabName = "inference",
        h2("Einfluss von Dashboard-Variablen auf Gesundheit"),
        
        fluidRow(
          # --- Linke Spalte: Modell-Einstellungen ---
          box(
            title = "Modell-Einstellungen", status = "primary",
            solidHeader = TRUE, width = 3,
            
            selectInput("outcome_var", "Abhängige Variable:",
                        choices = c(
                          "Autonomie" = "cond_auton_m", "Ressourcen" = "cond_res_m",
                          "Stressoren" = "cond_dem_m", "Selbstsorge" = "cope_seso_m",
                          "Interessierte Selbstgefährdung" = "cope_isg_m",
                          "Pos. WB: Engagement" = "wbpos_engage_m",
                          "Neg. WB: Life-Domain-Balance" = "wbneg_ldb_m",
                          "Neg. WB: Erschöpfung" = "wbneg_fws_m"
                        )
            ),
            
            hr(),
            h5("Unabhängige Variablen:", style = "font-weight: bold;"),
            
            checkboxGroupInput("predictors", NULL,
                               choices = c(
                                 "Führungskraft" = "fk", "MA-Gespräche" = "talk",
                                 "Alter (standardisiert)" = "age_std",
                                 "Geschlecht (weiblich)" = "gender_female",
                                 "Vollzeit (90-100%)" = "fulltime",
                                 "Homeoffice (standardisiert)" = "homeoffice_new_std",
                                 "Anlaufstelle: Ja" = "consult_yes",
                                 "Anlaufstelle: Weiß nicht" = "consult_donotknow",
                                 "Zeiterfassung" = "time_tracking"
                               ),
                               selected = c("fk", "talk", "age_std", "gender_female", "fulltime",
                                            "homeoffice_new_std", "consult_yes", "consult_donotknow",
                                            "time_tracking")
            ),
            
            fluidRow(
              column(6, actionButton("select_all", "Alle", class = "btn-sm btn-default", style = "width: 100%;")),
              column(6, actionButton("select_none", "Keine", class = "btn-sm btn-default", style = "width: 100%;"))
            ),
            
            br(),
            actionButton("run_model", "Modell berechnen",
                         class = "btn-primary btn-block", icon = icon("calculator"))
          ),
          
          # --- Rechte Spalte: Explorativer Scatterplot ---
          box(
            title = "Deskriptiver Boxplot", status = "info",
            solidHeader = TRUE, width = 9,
            
            fluidRow(
              column(12,
                     selectInput("scatter_x", "Unabhängige Variable (X):",
                                 choices = c(
                                   "Alter" = "age_std",
                                   "Homeoffice" = "homeoffice_new_std",
                                   "Führungskraft" = "fk",
                                   "MA-Gespräche" = "talk",
                                   "Vollzeit" = "fulltime",
                                   "Zeiterfassung" = "time_tracking",
                                   "Geschlecht" = "gender_female",
                                   "Anlaufstelle" = "consult"
                                 ),
                                 selected = "fk"
                     )
              )
            ),
            
            plotOutput("inference_scatter_plot", height = "420px")
          )
        ),
        
        fluidRow(
          box(title = "Modell-Ergebnisse", status = "primary", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("model_results_table"))
        ),
        fluidRow(
          box(title = "Interpretation", status = "info", solidHeader = TRUE, width = 12,
              htmlOutput("model_interpretation"))
        ),
        fluidRow(
          box(title = "Koeffizienten-Plot", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("coef_plot", height = "500px"))
        )
      ),
      
      # ---- TAB 5: Prädiktor-Rechner ----
      tabItem(
        tabName = "predictor",
        h2("Interaktiver Prädiktor-Rechner"),
        fluidRow(
          box(title = "ℹ️ Hinweis", status = "info", solidHeader = TRUE, width = 12,
              HTML("<strong>Dieser Rechner nutzt den vollständigen Datensatz (keine Sidebar-Filter).</strong>
                    <ul style='margin-top: 5px;'>
                      <li>Modell basiert auf: Alle Personen (nur Männer & Frauen)</li>
                      <li>Vorhersage: Ihre persönliche Schätzung basierend auf eingegebenen Merkmalen</li>
                    </ul>"))
        ),
        fluidRow(
          box(title = "Einstellungen", status = "primary", width = 12,
              HTML("<p>Wählen Sie Ihre Eigenschaften aus und sehen Sie den erwarteten Wert im Vergleich zum Gesamtdurchschnitt.</p>"))
        ),
        fluidRow(
          box(
            title = "Ihre Eigenschaften", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("pred_outcome", "Outcome-Variable:",
                        choices = c(
                          "Autonomie" = "cond_auton_m", "Ressourcen" = "cond_res_m",
                          "Stressoren" = "cond_dem_m", "Selbstsorge" = "cope_seso_m",
                          "Interessierte Selbstgefährdung" = "cope_isg_m",
                          "Pos. WB: Engagement" = "wbpos_engage_m",
                          "Neg. WB: Life-Domain-Balance" = "wbneg_ldb_m",
                          "Neg. WB: Erschöpfung" = "wbneg_fws_m"
                        ), selected = "cond_auton_m"
            ),
            hr(),
            radioButtons("pred_gender", "Geschlecht:",
                         choices = c("Mann" = 0, "Frau" = 1), selected = 0, inline = TRUE),
            sliderTextInput("pred_age", "Altersgruppe:",
                            choices = c("< 21", "21-30", "31-40", "41-50", "51-60", "> 60"),
                            selected = "31-40", grid = TRUE),
            radioButtons("pred_fk", "Führungskraft:",
                         choices = c("Nein" = 0, "Ja" = 1), selected = 0, inline = TRUE),
            radioButtons("pred_fulltime", "Vollzeit (90-100%):",
                         choices = c("Nein" = 0, "Ja" = 1), selected = 1, inline = TRUE),
            sliderInput("pred_homeoffice", "Homeoffice (%):",
                        min = 0, max = 100, value = 40, step = 10, ticks = FALSE, post = "%"),
            radioButtons("pred_talk", "Regelmäßige MA-Gespräche:",
                         choices = c("Nein" = 0, "Ja" = 1), selected = 1, inline = TRUE),
            radioButtons("pred_time_tracking", "Offizielle Zeiterfassung:",
                         choices = c("Nein" = 0, "Ja" = 1), selected = 1, inline = TRUE),
            selectInput("pred_consult", "Anlaufstelle vorhanden:",
                        choices = c("Ja" = "yes", "Nein" = "no", "Weiß nicht" = "donotknow"),
                        selected = "yes"),
            hr(),
            actionButton("calculate_prediction", "Berechnen",
                         class = "btn-success btn-block", icon = icon("calculator"))
          ),
          column(width = 8,
                 box(title = "Ihr erwarteter Wert", status = "info", solidHeader = TRUE, width = 12,
                     htmlOutput("prediction_summary")),
                 box(title = "Waterfall: Wie kommt der Wert zustande?", status = "success",
                     solidHeader = TRUE, width = 12,
                     plotOutput("waterfall_plot", height = "400px")),
                 box(title = "Sensitivity: Was-wäre-wenn Analyse", status = "success",
                     solidHeader = TRUE, width = 12,
                     selectInput("sensitivity_var", "Prädiktor für Sensitivity-Analyse:",
                                 choices = c("Altersgruppe" = "age", "Homeoffice %" = "homeoffice",
                                             "Geschlecht" = "gender", "Führungskraft" = "fk",
                                             "Vollzeit" = "fulltime", "MA-Gespräche" = "talk",
                                             "Zeiterfassung" = "time_tracking"),
                                 selected = "homeoffice"),
                     plotOutput("sensitivity_plot", height = "350px"))
          )
        )
      ),
      
      # ---- TAB 6: Organisationseinheiten ----
      tabItem(
        tabName = "org_units",
        h2("Unterschiede zwischen Organisationseinheiten"),
        fluidRow(
          box(title = "ℹ️ Was sind Estimated Marginal Means (EMMs)?",
              status = "info", solidHeader = TRUE, width = 12,
              collapsible = TRUE, collapsed = FALSE,
              HTML("<p><strong>EMMs zeigen bereinigte Durchschnittswerte pro Organisationseinheit.</strong></p>
                    <p>Systematische Unterschiede in Geschlechterverteilung, Altersstruktur, Führungskräfte-Anteil,
                    Homeoffice-Nutzung etc. werden herausgerechnet – so sehen wir den 'reinen' Unit-Effekt.</p>"))
        ),
        fluidRow(
          box(title = "Einstellungen", status = "primary", width = 3,
              selectInput("org_outcome", "Outcome-Variable:",
                          choices = c(
                            "Autonomie" = "cond_auton_m", "Ressourcen" = "cond_res_m",
                            "Stressoren" = "cond_dem_m", "Selbstsorge" = "cope_seso_m",
                            "Interessierte Selbstgefährdung" = "cope_isg_m",
                            "Pos. WB: Engagement" = "wbpos_engage_m",
                            "Neg. WB: Life-Domain-Balance" = "wbneg_ldb_m",
                            "Neg. WB: Erschöpfung" = "wbneg_fws_m"
                          )
              ),
              actionButton("run_org_model", "Analyse starten", class = "btn-primary btn-block")
          ),
          box(title = "Estimated Marginal Means", status = "primary", solidHeader = TRUE, width = 9,
              plotOutput("org_emmeans_plot", height = "400px"))
        ),
        fluidRow(
          box(title = "Kontrast-Tabelle", status = "primary", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("org_contrast_table"))
        )
      ),
      
      # ---- TAB 7: Zusammenhänge ----
      tabItem(
        tabName = "correlations",
        h2("Zusammenhänge zwischen Variablen"),
        fluidRow(
          box(title = "Korrelationsmatrix", status = "primary", solidHeader = TRUE, width = 12,
              plotOutput("correlation_plot", height = "600px"))
        ),
        fluidRow(
          box(title = "Scatter-Plot: Homeoffice & Gesundheit", status = "primary", width = 6,
              selectInput("scatter_outcome", "Gesundheits-Outcome:",
                          choices = c("Erschöpfung" = "wbneg_fws_m", "Engagement" = "wbpos_engage_m")),
              plotlyOutput("correlations_scatter_plot", height = "400px")),
          box(title = "Korrelations-Details", status = "primary", solidHeader = TRUE, width = 6,
              DT::dataTableOutput("correlation_table"))
        )
      ),
      
      # ---- TAB 8: Banken-Benchmark ----
      tabItem(
        tabName = "benchmark",
        h2("Banken-Benchmark"),
        
        fluidRow(
          box(
            title = "Einstellungen", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("benchmark_bank", "Ihre Bank:",
                        choices = NULL, selected = NULL),
            br(),
            actionButton("run_benchmark", "Analyse starten",
                         class = "btn-primary btn-block", icon = icon("flag")),
            br(), br(),
            uiOutput("benchmark_warning")
          ),
          box(
            title = "ℹ️ Methodik", status = "info", solidHeader = TRUE, width = 8,
            HTML("<p>Die Analyse vergleicht den Mittelwert Ihrer Bank mit dem Mittelwert 
                  aller anderen Banken — separat für jede der 8 Skalen.</p>
                  <ul>
                    <li><strong>Statistik:</strong> One-sample t-Test pro Skala</li>
                    <li><strong>Grün:</strong> Ihre Bank liegt über dem Durchschnitt</li>
                    <li><strong>Rot:</strong> Ihre Bank liegt unter dem Durchschnitt</li>
                    <li><strong>*</strong> Differenz ist statistisch signifikant (p < 0.05)</li>
                  </ul>
                  <p style='color: grey; font-size: 12px;'>
                  p-Werte sind nicht für multiples Testen korrigiert und haben explorativen Charakter.</p>")
          )
        ),
        
        fluidRow(
          box(
            title = "Ihre Bank vs. alle anderen (Skala 1–5)", status = "primary",
            solidHeader = TRUE, width = 7,
            plotOutput("benchmark_diverging", height = "400px")
          ),
          box(
            title = "Abweichung vom Mittelwert", status = "primary",
            solidHeader = TRUE, width = 5,
            plotOutput("benchmark_radar", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailtabelle", status = "primary", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("benchmark_table")
          )
        )
      ),
      
      # ---- TAB 9: Report Exportieren ----
      tabItem(
        tabName = "export",
        h2("Report Exportieren"),
        fluidRow(
          box(title = "1️⃣ Filter-Einstellungen", status = "primary", solidHeader = TRUE, width = 4,
              radioButtons("export_use_filters", "Datenbasis:",
                           choices = c("Aktuelle Filter verwenden" = "filtered",
                                       "Voller Datensatz (keine Filter)" = "full"),
                           selected = "filtered"),
              hr(), h5("Aktuelle Filter:"), uiOutput("export_filter_summary")),
          box(title = "2️⃣ Inhalte auswählen", status = "primary", solidHeader = TRUE, width = 4,
              checkboxGroupInput("export_sections",
                                 "Welche Abschnitte sollen im Report enthalten sein?",
                                 choices = c(
                                   "Überblick (N, Banken, Variablen)" = "overview",
                                   "Deskriptive Statistiken"          = "descriptives",
                                   "Clustering & ICC"                 = "clustering",
                                   "Inferenzstatistik (alle Outcomes)"= "inference",
                                   "Organisationseinheiten (EMMs)"    = "org_units",
                                   "Zusammenhänge / Korrelationen"    = "correlations",
                                   "Banken-Benchmark"                 = "benchmark"
                                 ),
                                 selected = c("overview", "descriptives", "clustering", "inference", "correlations")
              ),
              hr(),
              h5("Benchmark-Bank:"),
              uiOutput("export_benchmark_bank_ui")
          ),
          box(title = "3️⃣ Download", status = "primary", solidHeader = TRUE, width = 4,
              p("Der Report wird als selbst-enthaltene HTML-Datei exportiert und kann direkt geteilt werden."),
              hr(),
              downloadButton("download_report", "Report generieren & herunterladen",
                             class = "btn-primary btn-block",
                             style = "height: 50px; font-size: 16px; font-weight: bold;"),
              br(), br(), htmlOutput("export_status"))
        ),
        fluidRow(
          box(title = "Hinweise", status = "info", solidHeader = TRUE, width = 12,
              HTML("<ul>
                <li><strong>Voller Datensatz:</strong> Nutzt alle verfügbaren Daten (ignoriert Filter)</li>
                <li><strong>Inferenzstatistik / Organisationseinheiten:</strong> Kann 1-3 Minuten dauern (alle 8 Outcomes werden berechnet)</li>
                <li><strong>Banken-Benchmark:</strong> Nur wenn eine Bank ausgewählt ist</li>
                <li>Der HTML-Report ist vollständig eigenständig und kann per E-Mail geteilt werden</li>
              </ul>"))
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {

  # ---- Demo-Hinweis Modal ----
  showModal(modalDialog(
    title = tags$div(
      style = "text-align: center; font-size: 1.3em;",
      "⚠️ WICHTIGER HINWEIS ⚠️"
    ),
    tags$div(
      style = "text-align: center;",
      tags$h4(
        style = "color: #c0392b; font-weight: bold; margin-bottom: 16px;",
        "DEMO-VERSION MIT SIMULIERTEN DATEN"
      ),
      tags$p(
        style = "font-size: 1em; margin-bottom: 12px;",
        "Diese Dashboard-Version nutzt ", tags$strong("AUSSCHLIESSLICH"), " simulierte, fiktive Daten!"
      ),
      tags$ul(
        style = "list-style: none; padding: 0; text-align: left; display: inline-block;",
        tags$li("❌ Alle gezeigten Zahlen, Ergebnisse und Analysen sind NICHT real"),
        tags$li("❌ Die Daten wurden zufällig generiert und repräsentieren keine echte Studie"),
        tags$li("❌ Keine Rückschlüsse auf reale Banken, Organisationen oder Personen möglich"),
        tags$li("✅ Diese Version dient ausschließlich der Demonstration der Funktionalität")
      ),
      tags$hr(),
      tags$p(
        style = "font-size: 0.9em; color: #555;",
        "Für Zugang zur Analyse mit echten Daten kontaktieren Sie bitte die Projektleitung der ",
        tags$strong("Healthy@Work"), " Studie an der FHNW."
      )
    ),
    footer = modalButton("Verstanden"),
    easyClose = FALSE,
    size = "m"
  ))

  # ---- Reactive Data Loading ----
  raw_data <- reactive({ load_simulated_data() })
  
  processed_data <- reactive({
    req(raw_data())
    process_data(raw_data())
  })
  
  current_dataset <- reactive({
    req(processed_data())
    if (input$dataset_choice == "wave3") processed_data()$df_wave3
    else processed_data()$df_all_waves
  })
  
  observe({
    req(current_dataset())
    bank_choices <- current_dataset() %>%
      filter(!is.na(bank_name)) %>% pull(bank_name) %>% unique() %>% sort()
    updateSelectizeInput(session, "bank_filter", choices = bank_choices)
  })
  
  filtered_data <- eventReactive(input$apply_filters, {
    df <- current_dataset()
    age_mapping <- c("< 21" = 1, "21-30" = 2, "31-40" = 3, "41-50" = 4, "51-60" = 5, "> 60" = 6)
    age_min <- age_mapping[input$age_filter[1]]
    age_max <- age_mapping[input$age_filter[2]]
    df <- df %>% filter(age >= age_min, age <= age_max)
    if (!is.null(input$gender_filter))
      df <- df %>% filter(gender %in% as.numeric(input$gender_filter))
    if (!is.null(input$bank_filter) && length(input$bank_filter) > 0)
      df <- df %>% filter(bank_name %in% input$bank_filter)
    df
  }, ignoreNULL = FALSE)
  
  observe({
    if (is.null(filtered_data())) filtered_data <<- reactive({ current_dataset() })
  })
  
  # ---- TAB 1: Overview ----
  output$total_n_box <- renderValueBox({
    valueBox(nrow(current_dataset()), "Teilnehmende", icon = icon("users"), color = "blue")
  })
  output$total_banks_box <- renderValueBox({
    valueBox(length(unique(current_dataset()$orga)), "Banken", icon = icon("building"), color = "green")
  })
  output$avg_per_bank_box <- renderValueBox({
    avg <- current_dataset() %>% group_by(orga) %>% count() %>% pull(n) %>% mean() %>% round(1)
    valueBox(avg, "Ø Personen pro Bank", icon = icon("chart-line"), color = "orange")
  })
  
  output$dataset_info_table <- DT::renderDataTable({
    tibble(
      Datensatz = c("Wellenübergreifend", "Welle 2024/2025"),
      Personen = c(nrow(processed_data()$df_all_waves), nrow(processed_data()$df_wave3)),
      Banken = c(length(unique(processed_data()$df_all_waves$orga)),
                 length(unique(processed_data()$df_wave3$orga))),
      Anmerkung = c("Alle Wellen (2022-2025)", "Standard-Datensatz für Analysen")
    ) %>% DT::datatable(options = list(pageLength = 5, dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$variable_overview_table <- DT::renderDataTable({
    tribble(
      ~`Variablen-Art`, ~Variablenname, ~Bedeutung,
      "Dashboard-Variable: Soziodemographie", "age", "Altersgruppe der Testperson",
      "Dashboard-Variable: Soziodemographie", "gender", "Geschlecht der Testperson",
      "Dashboard-Variable: Anstellungskontext", "fk", "Ist Testperson Führungskraft?",
      "Dashboard-Variable: Anstellungskontext", "doku", "Dokumentiert Testperson selbstständig Arbeitszeiten?",
      "Dashboard-Variable: Anstellungskontext", "talk", "Hat Testperson regelmässige MA-Gespräche?",
      "Dashboard-Variable: Anstellungskontext", "talk_frequency", "Wie häufig hat Testperson MA-Gespräche?",
      "Dashboard-Variable: Anstellungskontext", "time_tracking", "Erfasst Testperson Arbeitszeit offiziell?",
      "Dashboard-Variable: Anstellungskontext", "time_tracking_type", "Art der Zeiterfassung",
      "Dashboard-Variable: Anstellungskontext", "fulltime", "Arbeitet Testperson 90-100%?",
      "Dashboard-Variable: Anstellungskontext", "homeoffice", "Arbeitet Testperson im Homeoffice?",
      "Dashboard-Variable: Anstellungskontext", "consult", "Steht der Testperson eine Anlaufstelle zur Verfügung?",
      "Dashboard-Variable: Studien-Metadaten", "waveNumber", "Welle der Studienerhebung",
      "Psychologisches Konstrukt / Skala: Arbeitsbedingung", "cond_auton_m", "Wahrgenommene Autonomie",
      "Psychologisches Konstrukt / Skala: Arbeitsbedingung", "cond_res_m", "Wahrgenommene Ressourcen",
      "Psychologisches Konstrukt / Skala: Arbeitsbedingung", "cond_dem_m", "Wahrgenommene Stressoren",
      "Psychologisches Konstrukt / Skala: Arbeitsbedingung", "cope_seso_m", "Selbstsorge",
      "Psychologisches Konstrukt / Skala: Arbeitsbedingung", "cope_isg_m", "Interessierte Selbstgefährdung",
      "Psychologisches Konstrukt / Skala: Gesundheit", "wbpos_engage_m", "Positives Wohlbefinden: Engagement",
      "Psychologisches Konstrukt / Skala: Gesundheit", "wbneg_ldb_m", "Negatives Wohlbefinden: Life-Domain Balance",
      "Psychologisches Konstrukt / Skala: Gesundheit", "wbneg_fws_m", "Negatives Wohlbefinden: Erschöpfung"
    ) %>%
      DT::datatable(
        options = list(pageLength = 20, dom = 'tp', ordering = FALSE,
                       columnDefs = list(list(width = '25%', targets = 0),
                                         list(width = '20%', targets = 1),
                                         list(width = '55%', targets = 2))),
        rownames = FALSE
      ) %>%
      DT::formatStyle('Variablen-Art', target = 'row',
                      backgroundColor = DT::styleEqual(
                        c("Dashboard-Variable: Soziodemographie", "Dashboard-Variable: Anstellungskontext",
                          "Dashboard-Variable: Studien-Metadaten",
                          "Psychologisches Konstrukt / Skala: Arbeitsbedingung",
                          "Psychologisches Konstrukt / Skala: Gesundheit"),
                        c("#fff7e6", "#e6f7ff", "#f0f0f0", "#f6ffed", "#fff0f6")
                      ))
  })
  
  # ---- TAB 2: Descriptives ----
  output$freq_table <- DT::renderDataTable({
    req(filtered_data())
    create_frequency_table(filtered_data(), input$desc_variable)
  })
  output$boxplot_scales <- renderPlot({
    req(filtered_data())
    plot_scale_boxplots(filtered_data(), input$desc_variable)
  })
  
  # ---- TAB 3: ICC ----
  output$icc_table <- DT::renderDataTable({
    req(current_dataset())
    calculate_icc_table(current_dataset())
  })
  
  # ---- TAB 4: Inferenzstatistik ----
  
  # Alle/Keine Buttons
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "predictors",
                             selected = c("fk", "talk", "age_std", "gender_female", "fulltime",
                                          "homeoffice_new_std", "consult_yes", "consult_donotknow", "time_tracking"))
  })
  observeEvent(input$select_none, {
    updateCheckboxGroupInput(session, "predictors", selected = character(0))
  })
  
  # Modell fitten
  model_results <- eventReactive(input$run_model, {
    df_full <- current_dataset() %>% filter(gender %in% c(1, 2))
    req(df_full)
    predictors <- input$predictors
    req(length(predictors) > 0)
    cat("\n=== FITTING MLM MODEL ===\n")
    cat("Outcome:", input$outcome_var, "\n")
    cat("Prädiktoren:", paste(predictors, collapse = ", "), "\n")
    model <- nlme::lme(
      fixed = as.formula(paste0(input$outcome_var, " ~ ", paste(predictors, collapse = " + "))),
      random = ~ 1 | orga,
      data = df_full,
      na.action = na.exclude
    )
    cat("=== MODEL FITTED ===\n\n")
    return(model)
  })
  
  output$model_results_table <- DT::renderDataTable({
    req(model_results())
    outcome_labels <- c(
      "cond_auton_m" = "Autonomie", "cond_res_m" = "Ressourcen",
      "cond_dem_m" = "Stressoren", "cope_seso_m" = "Selbstsorge",
      "cope_isg_m" = "Interessierte Selbstgefährdung",
      "wbpos_engage_m" = "Positives Wohlbefinden: Engagement",
      "wbneg_ldb_m" = "Negatives Wohlbefinden: Life-Domain-Balance",
      "wbneg_fws_m" = "Negatives Wohlbefinden: Erschöpfung"
    )
    av_name <- outcome_labels[input$outcome_var]
    if (is.na(av_name)) av_name <- input$outcome_var
    create_mlm_table(model_results(), av_name)
  })
  
  output$model_interpretation <- renderUI({
    req(model_results())
    outcome_labels <- c(
      "cond_auton_m" = "Autonomie", "cond_res_m" = "Ressourcen",
      "cond_dem_m" = "Stressoren", "cope_seso_m" = "Selbstsorge",
      "cope_isg_m" = "Interessierte Selbstgefährdung",
      "wbpos_engage_m" = "Engagement", "wbneg_ldb_m" = "Life-Domain-Balance",
      "wbneg_fws_m" = "Erschöpfung"
    )
    label <- outcome_labels[input$outcome_var]
    if (is.na(label)) label <- input$outcome_var
    HTML(paste0(
      "<p><strong>Interpretation:</strong></p>",
      "<p>Die Tabelle zeigt, welche Faktoren einen Einfluss auf <strong>", label, "</strong> haben.</p>",
      "<ul>",
      "<li><strong>Value (β):</strong> Stärke und Richtung des Effekts.</li>",
      "<li><strong>p-value:</strong> Werte unter 0.05 (rot) sind statistisch signifikant.</li>",
      "</ul>",
      "<p><em>Hinweis: Dieses Modell verwendet ALLE Daten (Filter werden nicht angewendet).</em></p>"
    ))
  })
  
  output$coef_plot <- renderPlot({
    req(model_results())
    plot_coefficients(model_results())
  })
  
  # ---- Adaptiver Scatterplot (Tab 4) ----
  
  # Klassifizierung der X-Variablen
  binary_vars     <- c("fk", "talk", "fulltime", "time_tracking", "gender_female")
  continuous_vars <- c("age_std", "homeoffice_new_std")
  
  output$inference_scatter_plot <- renderPlot({
    
    var_labels_x <- c(
      "age_std"            = "Altersgruppe",
      "homeoffice_new_std" = "Homeoffice",
      "fk"                 = "Führungskraft",
      "talk"               = "MA-Gespräche",
      "fulltime"           = "Vollzeit",
      "time_tracking"      = "Zeiterfassung",
      "gender_female"      = "Geschlecht",
      "consult"            = "Anlaufstelle"
    )
    var_labels_y <- c(
      "cond_auton_m"   = "Autonomie",
      "cond_res_m"     = "Ressourcen",
      "cond_dem_m"     = "Stressoren",
      "cope_seso_m"    = "Selbstsorge",
      "cope_isg_m"     = "Interessierte Selbstgefährdung",
      "wbpos_engage_m" = "Pos. WB: Engagement",
      "wbneg_ldb_m"    = "Neg. WB: Life-Domain-Balance",
      "wbneg_fws_m"    = "Neg. WB: Erschöpfung"
    )
    
    x_label <- var_labels_x[input$scatter_x]
    y_label <- var_labels_y[input$outcome_var]
    
    df_plot <- current_dataset() %>%
      filter(gender %in% c(1, 2)) %>%
      filter(!is.na(.data[[input$outcome_var]])) %>%
      filter(if (input$scatter_x == "consult") !is.na(consult)
             else !is.na(.data[[input$scatter_x]]))
    
    # Recode X into readable factor labels (mit expliziter Reihenfolge)
    if (input$scatter_x %in% binary_vars) {
      df_plot <- df_plot %>%
        mutate(x_group = factor(
          case_when(
            input$scatter_x == "gender_female" & .data[[input$scatter_x]] == 0 ~ "Mann",
            input$scatter_x == "gender_female" & .data[[input$scatter_x]] == 1 ~ "Frau",
            .data[[input$scatter_x]] == 0 ~ "Nein",
            .data[[input$scatter_x]] == 1 ~ "Ja"
          ),
          levels = case_when(
            input$scatter_x == "gender_female" ~ list(c("Mann", "Frau")),
            TRUE                               ~ list(c("Nein", "Ja"))
          )[[1]]
        ))
    } else if (input$scatter_x == "consult") {
      df_plot <- df_plot %>%
        mutate(x_group = factor(
          case_when(
            consult == 1 ~ "Ja",
            consult == 2 ~ "Nein",
            consult == 3 ~ "Weiss nicht"
          ),
          levels = c("Ja", "Nein", "Weiss nicht")
        ))
    } else if (input$scatter_x == "age_std") {
      df_plot <- df_plot %>%
        mutate(x_group = factor(
          case_when(
            age == 1 ~ "< 21",
            age == 2 ~ "21-30",
            age == 3 ~ "31-40",
            age == 4 ~ "41-50",
            age == 5 ~ "51-60",
            age == 6 ~ "> 60"
          ),
          levels = c("< 21", "21-30", "31-40", "41-50", "51-60", "> 60")
        ))
    } else {
      df_plot <- df_plot %>%
        mutate(x_group = factor(
          case_when(
            homeoffice_new <= 1 ~ "0-10%",
            homeoffice_new <= 3 ~ "20-30%",
            homeoffice_new <= 5 ~ "40-50%",
            homeoffice_new <= 7 ~ "60-70%",
            TRUE                ~ "80-100%"
          ),
          levels = c("0-10%", "20-30%", "40-50%", "60-70%", "80-100%")
        ))
    }
    
    summary_df <- df_plot %>%
      group_by(x_group) %>%
      summarise(
        mean_val = mean(.data[[input$outcome_var]], na.rm = TRUE),
        q3       = quantile(.data[[input$outcome_var]], 0.75, na.rm = TRUE),
        .groups  = "drop"
      )
    
    ggplot(df_plot,
           aes(x = x_group,
               y = .data[[input$outcome_var]],
               fill = x_group)) +
      geom_boxplot(alpha = 0.7, color = "grey30", outlier.alpha = 0.4) +
      geom_text(
        data     = summary_df,
        aes(x = x_group, y = q3, label = sprintf("MW: %.2f", mean_val)),
        vjust    = -0.5, size = 3.5, color = "grey20", fontface = "bold",
        inherit.aes = FALSE
      ) +
      scale_fill_brewer(palette = "Paired") +
      labs(x = x_label, y = "Skalenmittelwert") +
      theme_minimal() +
      theme(
        axis.text.x        = element_text(size = 9, vjust = 1),
        legend.position    = "none",
        panel.grid.major.x = element_blank()
      )
  })
  
  # ---- TAB 5: Org Units ----
  org_model <- eventReactive(input$run_org_model, {
    req(current_dataset())
    run_org_unit_model(current_dataset(), input$org_outcome)
  })
  output$org_emmeans_plot <- renderPlot({
    req(org_model())
    plot_org_emmeans(org_model(), input$org_outcome)
  })
  output$org_contrast_table <- DT::renderDataTable({
    req(org_model())
    create_org_contrast_table(org_model())
  })
  
  # ---- TAB 6: Correlations ----
  output$correlation_plot <- renderPlot({
    req(current_dataset())
    create_correlation_plot(current_dataset())
  })
  output$correlations_scatter_plot <- renderPlotly({
    req(current_dataset())
    create_scatter_plot(current_dataset(), input$scatter_outcome)
  })
  output$correlation_table <- DT::renderDataTable({
    req(current_dataset())
    create_correlation_table(current_dataset())
  })
  
  # ---- Predictor Calculator ----
  predictor_model <- eventReactive(input$calculate_prediction, {
    df_full <- current_dataset() %>% filter(gender %in% c(1, 2))
    req(df_full)
    outcome <- input$pred_outcome
    model <- nlme::lme(
      fixed = as.formula(paste0(outcome,
                                " ~ fk + talk + age_std + gender_female + fulltime +
           homeoffice_new_std + consult_yes + consult_donotknow + time_tracking")),
      random = ~ 1 | orga, data = df_full, na.action = na.exclude
    )
    attr(model, "age_mean")     <- mean(df_full$age, na.rm = TRUE)
    attr(model, "age_sd")       <- sd(df_full$age, na.rm = TRUE)
    attr(model, "ho_mean")      <- mean(df_full$homeoffice_new, na.rm = TRUE)
    attr(model, "ho_sd")        <- sd(df_full$homeoffice_new, na.rm = TRUE)
    attr(model, "outcome_mean") <- mean(df_full[[outcome]], na.rm = TRUE)
    return(model)
  })
  
  user_prediction <- reactive({
    req(predictor_model())
    model <- predictor_model()
    age_mapping <- c("< 21" = 1, "21-30" = 2, "31-40" = 3, "41-50" = 4, "51-60" = 5, "> 60" = 6)
    age_std <- (age_mapping[input$pred_age] - attr(model, "age_mean")) / attr(model, "age_sd")
    ho_std  <- (floor(as.numeric(input$pred_homeoffice) / 10) - attr(model, "ho_mean")) / attr(model, "ho_sd")
    new_data <- data.frame(
      fk = as.numeric(input$pred_fk), talk = as.numeric(input$pred_talk),
      age_std = age_std, gender_female = as.numeric(input$pred_gender),
      fulltime = as.numeric(input$pred_fulltime), homeoffice_new_std = ho_std,
      consult_yes = ifelse(input$pred_consult == "yes", 1, 0),
      consult_donotknow = ifelse(input$pred_consult == "donotknow", 1, 0),
      time_tracking = as.numeric(input$pred_time_tracking)
    )
    pred_value <- predict(model, newdata = new_data, level = 0)[1]
    coefs <- fixef(model)
    contributions <- c(
      "Intercept"              = coefs["(Intercept)"],
      "Geschlecht"             = coefs["gender_female"]     * new_data$gender_female,
      "Alter"                  = coefs["age_std"]            * new_data$age_std,
      "Führungskraft"          = coefs["fk"]                 * new_data$fk,
      "Vollzeit"               = coefs["fulltime"]           * new_data$fulltime,
      "Homeoffice"             = coefs["homeoffice_new_std"] * new_data$homeoffice_new_std,
      "MA-Gespräche"           = coefs["talk"]               * new_data$talk,
      "Zeiterfassung"          = coefs["time_tracking"]      * new_data$time_tracking,
      "Anlaufstelle (ja)"      = coefs["consult_yes"]        * new_data$consult_yes,
      "Anlaufstelle (weiß nicht)" = coefs["consult_donotknow"] * new_data$consult_donotknow
    )
    list(value = pred_value, reference = attr(model, "outcome_mean"),
         contributions = contributions, inputs = new_data)
  })
  
  output$prediction_summary <- renderUI({
    req(user_prediction())
    pred <- user_prediction()
    diff <- pred$value - pred$reference
    diff_text <- if (diff > 0) paste0("<span style='color: #27ae60; font-weight: bold;'>+", round(diff, 2), " höher</span>")
    else if (diff < 0) paste0("<span style='color: #e74c3c; font-weight: bold;'>", round(diff, 2), " tiefer</span>")
    else "<span style='font-weight: bold;'>gleich</span>"
    HTML(paste0(
      "<div style='text-align: center; padding: 20px;'>",
      "<h3 style='margin: 0;'>Ihr erwarteter Wert</h3>",
      "<h1 style='font-size: 48px; color: #3c8dbc; margin: 10px 0;'>", round(pred$value, 2), "</h1>",
      "<p style='font-size: 18px;'>Gesamtdurchschnitt: <strong>", round(pred$reference, 2), "</strong></p>",
      "<p style='font-size: 16px;'>Ihre Vorhersage ist ", diff_text, " als der Durchschnitt</p>",
      "</div>"
    ))
  })
  
  output$waterfall_plot <- renderPlot({
    req(user_prediction())
    pred <- user_prediction()
    wf <- data.frame(
      name   = c("Start", names(pred$contributions)[-1], "Ergebnis"),
      amount = c(pred$contributions[1], pred$contributions[-1], 0),
      stringsAsFactors = FALSE
    )
    wf$id    <- seq_along(wf$name)
    wf$end   <- cumsum(wf$amount)
    wf$start <- c(0, head(wf$end, -1))
    wf$end[nrow(wf)]   <- pred$value
    wf$start[nrow(wf)] <- pred$value
    wf$type  <- ifelse(wf$name %in% c("Start", "Ergebnis"), "Gesamt",
                       ifelse(wf$amount > 0, "Positiv", "Negativ"))
    wf$label <- gsub("Anlaufstelle \\(|\\)", "", wf$name)
    ggplot(wf, aes(x = id, fill = type)) +
      geom_rect(aes(xmin = id - 0.4, xmax = id + 0.4, ymin = start, ymax = end), alpha = 0.8) +
      geom_text(aes(x = id, y = end, label = sprintf("%.2f", end)), vjust = -0.5, size = 3.5, fontface = "bold") +
      geom_segment(data = wf[-nrow(wf), ],
                   aes(x = id + 0.4, xend = id + 0.6, y = end, yend = end),
                   linetype = "dashed", color = "gray50") +
      scale_fill_manual(values = c("Gesamt" = "#3498db", "Positiv" = "#27ae60", "Negativ" = "#e74c3c"), name = "Typ") +
      scale_x_continuous(breaks = wf$id, labels = wf$label) +
      labs(y = "Wert", x = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
            legend.position = "top", panel.grid.major.x = element_blank())
  })
  
  output$sensitivity_plot <- renderPlot({
    req(user_prediction(), predictor_model())
    pred  <- user_prediction()
    model <- predictor_model()
    sv    <- input$sensitivity_var
    age_mapping <- c("< 21" = 1, "21-30" = 2, "31-40" = 3, "41-50" = 4, "51-60" = 5, "> 60" = 6)
    if (sv == "age") {
      var_seq <- 1:6; vlabs <- names(age_mapping); xl <- "Altersgruppe"
      cur <- age_mapping[input$pred_age]
      preds <- sapply(var_seq, function(v) {
        nd <- pred$inputs; nd$age_std <- (v - attr(model,"age_mean")) / attr(model,"age_sd")
        predict(model, newdata = nd, level = 0)[1]
      })
    } else if (sv == "homeoffice") {
      var_seq <- seq(0, 100, 10); vlabs <- paste0(var_seq, "%"); xl <- "Homeoffice (%)"
      cur <- as.numeric(input$pred_homeoffice)
      preds <- sapply(var_seq, function(v) {
        nd <- pred$inputs; nd$homeoffice_new_std <- (floor(v/10) - attr(model,"ho_mean")) / attr(model,"ho_sd")
        predict(model, newdata = nd, level = 0)[1]
      })
    } else {
      var_seq <- c(0, 1); vlabs <- c("Nein", "Ja")
      xl  <- switch(sv, "gender" = "Geschlecht", "fk" = "Führungskraft",
                    "fulltime" = "Vollzeit", "talk" = "MA-Gespräche", "time_tracking" = "Zeiterfassung")
      cur <- switch(sv,
                    "gender" = as.numeric(input$pred_gender), "fk" = as.numeric(input$pred_fk),
                    "fulltime" = as.numeric(input$pred_fulltime), "talk" = as.numeric(input$pred_talk),
                    "time_tracking" = as.numeric(input$pred_time_tracking))
      vc  <- switch(sv, "gender" = "gender_female", sv)
      preds <- sapply(var_seq, function(v) {
        nd <- pred$inputs; nd[[vc]] <- v; predict(model, newdata = nd, level = 0)[1]
      })
    }
    plot_df <- data.frame(x = var_seq, y = preds, is_cur = var_seq == cur)
    ggplot(plot_df, aes(x = x, y = y)) +
      geom_line(color = "#3498db", size = 1.2) +
      geom_point(aes(color = is_cur, size = is_cur), alpha = 0.8) +
      scale_color_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "#e74c3c"), guide = "none") +
      scale_size_manual(values  = c("FALSE" = 3, "TRUE" = 5), guide = "none") +
      geom_hline(yintercept = attr(model, "outcome_mean"), linetype = "dashed", color = "gray50", alpha = 0.6) +
      annotate("text", x = mean(var_seq), y = attr(model, "outcome_mean"),
               label = "Durchschnitt", vjust = -0.5, size = 3, color = "gray50") +
      scale_x_continuous(breaks = var_seq, labels = vlabs) +
      labs(y = "Erwarteter Wert", x = xl) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9), panel.grid.minor = element_blank())
  })
  
  # ---- TAB 8: Benchmark ----
  
  observe({
    req(current_dataset())
    bank_choices <- current_dataset() %>%
      filter(!is.na(bank_name)) %>%
      pull(bank_name) %>% unique() %>% sort()
    updateSelectInput(session, "benchmark_bank", choices = bank_choices)
  })
  
  benchmark_results <- eventReactive(input$run_benchmark, {
    req(current_dataset(), input$benchmark_bank)
    run_benchmark_analysis(current_dataset(), input$benchmark_bank)
  })
  
  output$benchmark_warning <- renderUI({
    req(benchmark_results())
    n <- benchmark_results()$n_Bank[1]
    if (n < 30) {
      div(
        style = "background-color: #fff3cd; border: 1px solid #ffc107; padding: 10px; border-radius: 4px;",
        HTML(paste0("<strong>⚠ Hinweis:</strong> Ihre Bank hat n = ", n,
                    " Personen. Bei kleinen Stichproben sind die Schätzungen 
                     weniger zuverlässig (breite Konfidenzintervalle)."))
      )
    }
  })
  
  output$benchmark_diverging <- renderPlot({
    req(benchmark_results())
    plot_benchmark_diverging(benchmark_results(), input$benchmark_bank)
  })
  
  output$benchmark_radar <- renderPlot({
    req(benchmark_results())
    plot_benchmark_radar(benchmark_results())
  })
  
  output$benchmark_table <- DT::renderDataTable({
    req(benchmark_results())
    benchmark_results_table(benchmark_results())
  })
  
  # ---- TAB 9: Export ----
  output$export_filter_summary <- renderUI({
    HTML(paste0(
      "<div style='background-color: #f5f5f5; padding: 10px; border-radius: 3px;'>",
      "<strong>Datensatz:</strong> ", input$dataset_choice, "<br>",
      "<strong>Altersgruppe:</strong> ", input$age_filter[1], " - ", input$age_filter[2], "<br>",
      "<strong>Geschlecht:</strong> ",
      ifelse(is.null(input$gender_filter), "Alle", paste(input$gender_filter, collapse = ", ")), "<br>",
      "<strong>Banken:</strong> ",
      ifelse(is.null(input$bank_filter) || length(input$bank_filter) == 0, "Alle",
             paste0(length(input$bank_filter), " ausgewählt")), "</div>"
    ))
  })

  output$export_benchmark_bank_ui <- renderUI({
    req(current_dataset())
    bank_choices <- c("(keiner)" = "", current_dataset() %>%
      filter(!is.na(bank_name)) %>% pull(bank_name) %>% unique() %>% sort())
    selectInput("export_benchmark_bank", NULL, choices = bank_choices, selected = "")
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Healthy_at_Work_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      withProgress(message = 'Report wird erstellt...', value = 0, {
        incProgress(0.1, detail = "Bereite Daten vor...")
        report_data  <- if (input$export_use_filters == "filtered") filtered_data() else current_dataset()
        dataset_name <- if (input$dataset_choice == "wave3") "Welle 2024/2025" else "Alle Wellen (2022-2025)"
        filter_info  <- if (input$export_use_filters == "filtered") {
          paste0(
            "Alter: ", input$age_filter[1], " - ", input$age_filter[2], " | ",
            "Geschlecht: ", ifelse(is.null(input$gender_filter), "Alle",
                                   paste(input$gender_filter, collapse = ", ")), " | ",
            "Banken: ", ifelse(is.null(input$bank_filter) || length(input$bank_filter) == 0,
                                "Alle", paste0(length(input$bank_filter), " ausgewählt"))
          )
        } else { "Keine Filter angewendet (voller Datensatz)" }

        bench_bank <- if (!is.null(input$export_benchmark_bank) && input$export_benchmark_bank != "")
          input$export_benchmark_bank else ""

        incProgress(0.2, detail = "Rendere Report (kann mehrere Minuten dauern)...")
        tryCatch({
          rmarkdown::render(
            input         = "report_template.Rmd",
            output_format = rmarkdown::html_document(
              theme          = "flatly",
              toc            = TRUE,
              toc_float      = TRUE,
              self_contained = TRUE
            ),
            output_file = file,
            params = list(
              data           = report_data,
              dataset_name   = dataset_name,
              filter_info    = filter_info,
              sections       = input$export_sections,
              benchmark_bank = bench_bank,
              total_n        = nrow(report_data),
              total_banks    = length(unique(report_data$orga))
            ),
            envir = new.env(),
            quiet = FALSE
          )
          incProgress(0.7, detail = "Finalisiere...")
        }, error = function(e) {
          showNotification(paste("Fehler:", e$message), type = "error", duration = 10)
        })
      })
    }
  )
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)