# ============================================================================
# CREATE SECURE CREDENTIALS DATABASE
# Führe dieses Skript EINMAL aus, um die Credential-Datenbank zu erstellen
# ============================================================================

library(shinymanager)

# ============================================================================
# WICHTIG: ÄNDERE DIESE CREDENTIALS VOR PRODUKTION!
# ============================================================================

credentials <- data.frame(
  user = c(
    "admin",           # Administrator
    "fhnw_researcher", # FHNW Forschungsteam
    "bank_analyst1",   # Bank Analyst 1
    "bank_analyst2"    # Bank Analyst 2
  ),
  password = c(
    "Admin#FHNW2025!",      # ⚠️ ÄNDERE DIES!
    "Research!Secure99",    # ⚠️ ÄNDERE DIES!
    "BankAnalytics#42",     # ⚠️ ÄNDERE DIES!
    "HealthData@2025"       # ⚠️ ÄNDERE DIES!
  ),
  admin = c(
    TRUE,   # Admin hat alle Rechte
    TRUE,   # FHNW Team hat Admin-Rechte
    FALSE,  # Bank Analysts sind normale User
    FALSE
  ),
  name = c(
    "Administrator",
    "FHNW Forschungsteam",
    "Bank Analyst 1",
    "Bank Analyst 2"
  ),
  expire = c(
    NA,              # Admin-Account läuft nicht ab
    NA,              # FHNW-Account läuft nicht ab
    "2025-12-31",    # Bank Accounts laufen Ende 2025 ab
    "2025-12-31"
  ),
  stringsAsFactors = FALSE
)

# ============================================================================
# ERSTELLE VERSCHLÜSSELTE DATENBANK
# ============================================================================

# Erstelle database Ordner falls nicht vorhanden
if (!dir.exists("database")) {
  dir.create("database")
  cat("✓ Ordner 'database/' erstellt\n")
}

# ⚠️ WICHTIG: Ändere diese Passphrase zu etwas SEHR sicherem!
# Speichere sie sicher - ohne sie kannst du nicht auf die DB zugreifen!
SECURE_PASSPHRASE <- "CHANGE_THIS_TO_SUPER_SECRET_KEY_12345!@#"

# Erstelle die Datenbank
create_db(
  credentials_data = credentials,
  sqlite_path = "database/credentials.sqlite",
  passphrase = SECURE_PASSPHRASE
)

cat("\n")
cat("============================================================================\n")
cat("✓ CREDENTIAL-DATENBANK ERFOLGREICH ERSTELLT!\n")
cat("============================================================================\n")
cat("\n")
cat("Datei: database/credentials.sqlite\n")
cat("\n")
cat("User erstellt:\n")
for (i in 1:nrow(credentials)) {
  cat(sprintf("  - %s (%s) %s\n", 
              credentials$user[i], 
              credentials$name[i],
              if(credentials$admin[i]) "[ADMIN]" else ""))
}
cat("\n")
cat("⚠️  WICHTIGE NÄCHSTE SCHRITTE:\n")
cat("============================================================================\n")
cat("1. SICHERE die Passphrase an einem sicheren Ort!\n")
cat("   → Passphrase: ", SECURE_PASSPHRASE, "\n")
cat("\n")
cat("2. AKTUALISIERE app.R:\n")
cat("   → Ersetze die credentials Definition mit:\n")
cat("   \n")
cat("   server_secured <- secure_server(\n")
cat("     check_credentials = check_credentials(\n")
cat("       db = \"database/credentials.sqlite\",\n")
cat("       passphrase = \"", SECURE_PASSPHRASE, "\"\n")
cat("     )\n")
cat("   )\n")
cat("\n")
cat("3. FÜGE zu .gitignore hinzu:\n")
cat("   database/*.sqlite\n")
cat("   .Renviron\n")
cat("\n")
cat("4. FÜR MAXIMALE SICHERHEIT - Erstelle .Renviron:\n")
cat("   DB_PASSPHRASE=\"", SECURE_PASSPHRASE, "\"\n")
cat("   \n")
cat("   Dann in app.R:\n")
cat("   passphrase = Sys.getenv(\"DB_PASSPHRASE\")\n")
cat("\n")
cat("============================================================================\n")

# ============================================================================
# TESTE DEN LOGIN (optional)
# ============================================================================

cat("\n")
cat("Teste Credentials...\n")
test_result <- check_credentials(
  db = "database/credentials.sqlite",
  passphrase = SECURE_PASSPHRASE
)

# Teste einen User
test_login <- test_result(
  user = credentials$user[1],
  password = credentials$password[1]
)

if (test_login$result) {
  cat("✓ Test erfolgreich! Login funktioniert.\n")
} else {
  cat("✗ FEHLER: Login-Test fehlgeschlagen!\n")
}

cat("\n")
cat("Fertig! Die App ist jetzt passwortgeschützt.\n")
