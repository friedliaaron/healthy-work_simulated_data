# ============================================================================
# INSTALLATION SCRIPT
# Installiert alle benötigten Pakete für das Healthy@Work Dashboard
# ============================================================================

cat("========================================\n")
cat("Healthy@Work Dashboard - Installation\n")
cat("========================================\n\n")

# Liste aller benötigten Pakete
required_packages <- c(
  # Shiny & Dashboard
  "shiny",
  "shinydashboard",
  
  # Data Manipulation
  "tidyverse",
  "readxl",
  "here",
  
  # Interactive Tables & Plots
  "DT",
  "plotly",
  
  # Statistical Modeling
  "lme4",
  "lmerTest",
  "nlme",
  "performance",
  "emmeans",
  
  # Visualization
  "viridis",
  "corrplot",
  "Hmisc",
  "kableExtra"
)

cat("Prüfe installierte Pakete...\n\n")

# Prüfe welche Pakete fehlen
installed_packages <- installed.packages()[, "Package"]
missing_packages <- required_packages[!required_packages %in% installed_packages]

if (length(missing_packages) == 0) {
  cat("✓ Alle Pakete sind bereits installiert!\n")
} else {
  cat("Folgende Pakete werden installiert:\n")
  cat(paste("  -", missing_packages, collapse = "\n"), "\n\n")
  
  cat("Installation startet...\n")
  
  install.packages(missing_packages, dependencies = TRUE)
  
  cat("\n✓ Installation abgeschlossen!\n")
}

# Teste ob alle Pakete geladen werden können
cat("\nTeste Pakete...\n")

test_results <- sapply(required_packages, function(pkg) {
  tryCatch({
    library(pkg, character.only = TRUE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
})

if (all(test_results)) {
  cat("✓ Alle Pakete erfolgreich getestet!\n\n")
  cat("========================================\n")
  cat("Installation erfolgreich!\n")
  cat("Du kannst jetzt die App starten mit:\n")
  cat("  shiny::runApp('path/to/healthy_at_work_dashboard')\n")
  cat("========================================\n")
} else {
  failed_packages <- names(test_results)[!test_results]
  cat("\n⚠ Folgende Pakete konnten nicht geladen werden:\n")
  cat(paste("  -", failed_packages, collapse = "\n"), "\n")
  cat("\nBitte installiere diese manuell:\n")
  cat(paste0("  install.packages(c(", paste0("'", failed_packages, "'", collapse = ", "), "))\n"))
}
