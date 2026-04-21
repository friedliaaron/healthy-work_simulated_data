# 🏥 Healthy@Work Analytics Dashboard

**Interaktives Shiny Dashboard für Arbeitsgesundheitsanalysen**

[![DEMO VERSION](https://img.shields.io/badge/STATUS-DEMO%20VERSION-red?style=for-the-badge)](https://aaronfriedli.shinyapps.io/healthyworksimulated/)
---

## ⚠️ WICHTIGER HINWEIS

**Diese Version nutzt AUSSCHLIESSLICH simulierte, fiktive Daten!**

- ❌ Alle gezeigten Zahlen, Ergebnisse und Analysen sind **NICHT real**
- ❌ Die Daten wurden zufällig generiert und repräsentieren **keine echte Studie**
- ❌ **Keine Rückschlüsse** auf reale Banken, Organisationen oder Personen möglich
- ✅ Diese Version dient **ausschließlich der Demonstration** der Dashboard-Funktionalität

---

## 📊 Über das Projekt

Dieses Dashboard wurde im Rahmen der **Healthy@Work Studie** entwickelt und ermöglicht interaktive Analysen von Arbeitsgesundheitsdaten in Schweizer Banken.

### Analysierte Dimensionen:
- 🧠 **Arbeitsbedingungen**: Autonomie, Ressourcen, Stressoren
- 💪 **Coping-Strategien**: Selbstsorge, Interessierte Selbstgefährdung
- 😊 **Wohlbefinden**: Engagement, Work-Life-Balance, Erschöpfung

### Dashboard-Features:
- **7 interaktive Tabs** mit verschiedenen Analysetypen
- **Multilevel-Modelle** (MLM) zur Berücksichtigung der Organisations-Hierarchie
- **Prädiktor-Rechner** mit Waterfall- und Sensitivity-Analysen
- **EMM-Analysen** für faire Vergleiche zwischen Organisationseinheiten
- **Korrelationsanalysen** und deskriptive Statistiken
- **Report-Export** (Word, PDF, HTML)

---

## 🚀 Installation & Start

### Voraussetzungen

```r
# R Version >= 4.0.0
R.version.string
```

### Benötigte Pakete

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets",
  "tidyverse", "DT", "plotly", 
  "lme4", "lmerTest", "nlme", "performance", "emmeans",
  "corrplot", "viridis", "Hmisc", "kableExtra",
  "readxl", "here", "rmarkdown", "knitr", "RColorBrewer"
))
```

### Dashboard starten

```r
# Navigiere zum Projektordner
setwd("path/to/healthy_at_work_dashboard")

# Starte die App
shiny::runApp()
```

Das Dashboard öffnet sich im Browser unter `http://127.0.0.1:XXXX`

---

## 📁 Projektstruktur

```
healthy_at_work_dashboard/
│
├── app.R                    # Haupt-App (UI + Server)
├── helpers.R                # Analyse-Funktionen
├── data_processing.R        # Datenverarbeitung & Simulation
├── report_template.Rmd      # R Markdown Template für Reports
│
├── www/
│   └── fhnw.jpg            # FHNW Logo
│
└── README.md               # Diese Datei
```

---

## 📖 Dashboard-Tabs im Detail

### 1️⃣ Überblick
- Stichprobenübersicht (N, Banken, Durchschnitt pro Bank)
- Datensatz-Information
- **Variablen-Übersicht** mit allen verwendeten Konstrukten

### 2️⃣ Deskriptive Statistiken
- Häufigkeitstabellen für kategoriale Variablen
- Boxplots nach Gruppen (Alter, Geschlecht, etc.)
- **Filter aktiv**: Alter, Geschlecht, Bank

### 3️⃣ Clustering & ICC
- **Intraclass Correlation Coefficients** (ICC)
- Analyse der Varianz zwischen vs. innerhalb Organisationen
- Signifikanztests

### 4️⃣ Inferenzstatistik
- **Multilevel-Modelle** (Mixed Effects Models)
- Kontrolliert für Organisationszugehörigkeit
- Koeffiziententabellen + Visualisierungen
- **Nur Männer & Frauen** (automatischer Gender-Filter)

### 5️⃣ Prädiktor-Rechner
- Interaktive Vorhersagen basierend auf persönlichen Merkmalen
- **Waterfall-Chart**: Wie kommt der Wert zustande?
- **Sensitivity-Analyse**: Was-wäre-wenn Szenarien

### 6️⃣ Organisationseinheiten
- **Estimated Marginal Means** (EMMs)
- Bereinigte Vergleiche zwischen Units
- Paarweise Kontraste

### 7️⃣ Zusammenhänge
- Korrelationsmatrix
- Interaktive Scatter Plots
- Detaillierte Korrelationstabellen

### 8️⃣ Report Exportieren
- **Auswählbare Abschnitte**
- **Formate**: Word (.docx), PDF, HTML
- Filter-Einstellungen konfigurierbar

---

## 🎨 Design

Das Dashboard folgt dem **FHNW Corporate Design**:
- Primärfarbe: **FHNW Gelb** (#FDE70E)
- Akzentfarbe: Schwarz (#000000)
- Minimalistisches, professionelles Design
- Responsive Layout

---

## 📊 Simulierte Daten

Die Daten werden in `data_processing.R` > `load_simulated_data()` generiert:

- **N = 1500** simulierte Beobachtungen
- **3 Wellen** (2022/2023, 2023/2024, 2024/2025)
- **15 Organisationen** (Banken)
- **Realistische Verteilungen** für alle Variablen
- **Korrelationsstruktur** angelehnt an echte Studien

---

## 🔬 Statistische Methoden

### Multilevel-Modelle (MLM)
```r
nlme::lme(
  fixed = outcome ~ prädiktoren,
  random = ~ 1 | organisation,
  data = df
)
```

**Warum MLM?**
- Berücksichtigt Abhängigkeiten innerhalb Organisationen
- Trennt Varianz zwischen vs. innerhalb Banken
- Robuste Standardfehler

### Estimated Marginal Means (EMMs)
```r
emmeans::emmeans(model, ~ organisationseinheit)
```

**Warum EMMs?**
- Fairer Vergleich zwischen Units
- Kontrolliert für Störvariablen (Alter, Geschlecht, etc.)
- "Gleiche Bedingungen" für alle Units

---

## 👥 Entwickelt von

**FHNW - Fachhochschule Nordwestschweiz**  
Institut für Kooperationsforschung und -entwicklung

**Projekt**: Healthy@Work Studie  
**Kontakt**: [FHNW Website](https://www.fhnw.ch)

---

## 📄 Lizenz

Dieses Projekt ist für **Demonstrations- und Lehrzwecke** erstellt.  
Für die Nutzung mit echten Daten kontaktieren Sie bitte die FHNW.

---

## 🙏 Danksagung

Entwickelt mit:
- [R Shiny](https://shiny.rstudio.com/)
- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [tidyverse](https://www.tidyverse.org/)
- [lme4](https://github.com/lme4/lme4) & [nlme](https://cran.r-project.org/package=nlme)

---

**Für Fragen oder Feedback:**  
📧 Kontaktieren Sie die FHNW Projektleitung

---

*Letzte Aktualisierung: März 2026*
