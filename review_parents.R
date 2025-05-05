#### Pakete installieren

devtools::install_github("MathiasHarrer/dmetar")

#### Pakete laden

library(tidyverse)
library(easystats)
library(sjPlot)
library(meta)
library(metafor)
library(esc)
library(broom)

#### Daten laden

#review <- data_read("./review.csv")

#### data frame filtern

### für Caregiver

review_p <- review %>%
  filter(out_parent == "yes")


### Fälle löschen, da sie unpassende Outcomes haben

review_p <- review_p %>% filter(!no %in% c(76, 77))

## data frame speichern

#data_write(review_p, "./review_p.csv")
#data_write(review_p, "./review_p.rdata")
#data_write(review_p, "./review_p.xlsx")

### Fälle zu well-being ändern, welche falsche zuordnung haben

review_p <- review_p %>%
  mutate(out_higher_construct = if_else(no %in% c(3, 12, 13, 14, 42, 49, 50), "well-being", out_higher_construct))

## Wie viele Studien?

review_p %>%
  summarise(n_unique_studies = n_distinct(id))

#### Daten bearbeiten

#### Reine Mittelwertsunterscheide berechnen

### Calculate the pure mean differences for post-test

review_p <- review_p %>%
  mutate(mean_difference = int_post_m - con_post_m)

### Calculate the pure mean differences for follow-up, if available

review_p <- review_p %>%
  mutate(
    mw_diff_fol = ifelse(!is.na(int_fol_m) & !is.na(con_fol_m),
                         int_fol_m - con_fol_m,
                         NA))

#### Effektstärken für MW-Unterschiede berechnen

## Berechnung der Effektgrößen für Post-Test-Daten

review_p <- review_p %>%
  mutate(
    effect_size_post = escalc(measure = "SMD", 
                              m1i = int_post_m, sd1i = int_post_sd, n1i = int_post_n,
                              m2i = con_post_m, sd2i = con_post_sd, n2i = con_post_n, 
                              data = review_p)$yi)

## Berechnung der Effektgrößen für Follow-Up-Daten, falls verfügbar

review_p <- review_p %>%
  mutate(
    effect_size_fol = escalc(measure = "SMD", 
                             m1i = int_fol_m, sd1i = int_fol_sd, n1i = int_fol_n,
                             m2i = con_fol_m, sd2i = con_fol_sd, n2i = con_fol_n,
                             data = review_p)$yi)

## Effektstärken umkodieren

# Liste der Werte, bei denen reverse coding erfolgen soll

reverse_no_values <- c(36, 37, 38, 39, 40, 62, 63, 64, 65, 66, 67, 68, 69, 70, 76, 88)

# Reverse coding für die Variable effect_size_post
review_p <- review_p %>%
  mutate(
    effect_size_post = if_else(no %in% reverse_no_values,
                               -effect_size_post, 
                               effect_size_post))

# Reverse coding für die Variable effect_size_fol
review_p <- review_p %>%
  mutate(
    effect_size_fol = if_else(no %in% reverse_no_values,
                               -effect_size_fol, 
                              effect_size_fol))


## Fälle entfernen die NA in effect_size_post haben

review_p <- review_p %>% filter(!is.na(effect_size_post))

## Standardfehler der Effektstärke post

review_p <- review_p %>%
  mutate(se_effect_post = sqrt((int_post_sd^2 / int_post_n) + (con_post_sd^2 / con_post_n)))

## Standardfehler der Effektstärke follow-up

review_p <- review_p %>%
  mutate(se_effect_fol = sqrt((int_fol_sd^2 / int_fol_n) + (con_fol_sd^2 / con_fol_n)))

#### Missing values für alter ersetzen

# Neue Variable age_combined erstellen
review_p <- review_p %>%
  mutate(
    # Splitte den Range in unteres und oberes Alter
    lower_age = as.numeric(sub("–", "-", sub("-.*", "", age_range))),  # Extrahiere unteren Wert
    upper_age = as.numeric(sub(".*-", "", sub("–", "-", age_range))),  # Extrahiere oberen Wert
    
    # Mittelwert aus Range berechnen
    age_mean_from_range = (lower_age + upper_age) / 2,
    
    # Neue Variable age_combined erstellen
    age_combined = if_else(
      is.na(age_m),                 # Wenn age_m fehlt
      age_mean_from_range,          # Nutze den berechneten Mittelwert aus dem Range
      age_m                         # Andernfalls behalte den Wert aus age_m
    )
  )

#### Neue Variable für Moderation von Setting

review_p <- review_p %>%
  mutate(
    int_setting_mod = case_when(
      int_setting_higher == "household" ~ "household",
      int_setting_higher == "household and clinic" ~ "household + clinic",
      int_setting_higher == "clinic" ~ "clinic",
      TRUE ~ NA_character_  # Andere Werte werden zu NA
    ),
    int_setting_mod = factor(int_setting_mod, levels = c("household", "household + clinic", "clinic"))
  )

#### Neue Variable für Moderation von Mode

review_p <- review_p %>%
  mutate(
    int_mode_mod = case_when(
      int_mode_higher %in% c("person", "person and phone", "person and video") ~ "person",
      int_mode_higher %in% c("person and online") ~ "online",
      int_mode_higher %in% c("video", "video and phone") ~ "video",
      TRUE ~ NA_character_  # Falls weitere Werte existieren
    ),
    int_mode_mod = factor(int_mode_mod, levels = c("person", "online", "video"))
  )

### Neue Autoren Variable

review_p <- review_p %>%
  mutate(author_s = paste(sub(",.*", "", author), year))

## Data frame für follow up

review_fol <- review_p %>% filter(!is.na(effect_size_fol))

#### Metaanalysen für jedes Konstrukt

### well-being

## data frame erstellen

review_wb <- review_p %>% filter(out_higher_construct == "well-being")

## NA entfernen

review_wb <- review_wb %>% filter(!is.na(effect_size_post))

## Wie viele Studien?

review_wb %>%
  summarise(n_unique_studies = n_distinct(id))

### Durchführung der Meta-Analyse für 'well-being' Outcomes unter Berücksichtigung der Abhängigkeiten innerhalb der Studien

# Gewichtung an Varianz

res_wb <- rma.mv(
  yi = effect_size_post, 
  V = se_effect_post^2, 
  random = ~ 1 | id, 
  data = review_wb, 
  method = "REML")

## Zusammenfassung der Ergebnisse der Meta-Analyse

summary(res_wb)

model_parameters(res_wb, summary = T)

## I Statistik

# Berechnung der Gewichtungsmatrix und Projektion
W <- diag(1 / res_wb$vi)                          # Gewichtungsmatrix (inverse Varianzen)
X <- model.matrix(res_wb)                         # Designmatrix
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W  # Projektion

# Gesamt-I² berechnen
I2_total <- 100 * sum(res_wb$sigma2) / (sum(res_wb$sigma2) + (res_wb$k - res_wb$p) / sum(diag(P)))

# Ergebnis ausgeben
cat("Gesamt-I²:", round(I2_total, 2), "%\n")

## 95% PI

# Berechnung der mittleren Effektgröße und ihrer Varianz
mu_hat <- res_wb$b
var_mu <- res_wb$vb
tau2 <- res_wb$sigma2

# Kritischer z-Wert für 95%-PI
z <- qnorm(0.975)

# Grenzen des Prädiktionsintervalls
PI_lower <- mu_hat - z * sqrt(var_mu + tau2)
PI_upper <- mu_hat + z * sqrt(var_mu + tau2)

# Ausgabe
cat("95% Prädiktionsintervall: [", PI_lower, ", ", PI_upper, "]\n")

### stress

## data frame erstellen

review_stress <- review_p %>% filter(out_higher_construct == "stress")

## NA entfernen

review_stress <- review_stress %>% filter(!is.na(effect_size_post))

## Wie viele Studien?

review_stress %>%
  summarise(n_unique_studies = n_distinct(id))

## Durchführung der Meta-Analyse für 'stress' Outcomes unter Berücksichtigung der Abhängigkeiten innerhalb der Studien

res_stress <- rma.mv(yi = effect_size_post, 
                     V = se_effect_post^2, 
                     random = ~ 1 | id, 
                     data = review_stress, 
                     method = "REML")

## Zusammenfassung der Ergebnisse der Meta-Analyse

summary(res_stress)

model_parameters(res_stress, summary = T)

## I Statistik

# Berechnung der Gewichtungsmatrix und Projektion
W <- diag(1 / res_stress$vi)                          # Gewichtungsmatrix (inverse Varianzen)
X <- model.matrix(res_stress)                         # Designmatrix
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W  # Projektion

# Gesamt-I² berechnen
I2_total <- 100 * sum(res_stress$sigma2) / (sum(res_stress$sigma2) + (res_stress$k - res_stress$p) / sum(diag(P)))

# Ergebnis ausgeben
cat("Gesamt-I²:", round(I2_total, 2), "%\n")

## 95% PI

# Berechnung der mittleren Effektgröße und ihrer Varianz
mu_hat <- res_stress$b
var_mu <- res_stress$vb
tau2 <- res_stress$sigma2

# Kritischer z-Wert für 95%-PI
z <- qnorm(0.975)

# Grenzen des Prädiktionsintervalls
PI_lower <- mu_hat - z * sqrt(var_mu + tau2)
PI_upper <- mu_hat + z * sqrt(var_mu + tau2)

# Ausgabe
cat("95% Prädiktionsintervall: [", PI_lower, ", ", PI_upper, "]\n")

### healthcare

## data frame erstellen

review_hc <- review_p %>% filter(out_higher_construct == "healthcare utilisation and knowledge")

## NA entfernen

review_hc <- review_hc %>% filter(!is.na(effect_size_post))

## Wie viele Studien?

review_hc %>%
  summarise(n_unique_studies = n_distinct(id))

## Durchführung der Meta-Analyse für 'healthcare' Outcomes unter Berücksichtigung der Abhängigkeiten innerhalb der Studien

res_hc <- rma.mv(yi = effect_size_post, 
                 V = se_effect_post^2, 
                 random = ~ 1 | id, 
                 data = review_hc, 
                 method = "REML")

## Zusammenfassung der Ergebnisse der Meta-Analyse

summary(res_hc)

model_parameters(res_hc, summary = T)

## I Statistik

# Berechnung der Gewichtungsmatrix und Projektion
W <- diag(1 / res_hc$vi)                          # Gewichtungsmatrix (inverse Varianzen)
X <- model.matrix(res_hc)                         # Designmatrix
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W  # Projektion

# Gesamt-I² berechnen
I2_total <- 100 * sum(res_hc$sigma2) / (sum(res_hc$sigma2) + (res_hc$k - res_hc$p) / sum(diag(P)))

# Ergebnis ausgeben
cat("Gesamt-I²:", round(I2_total, 2), "%\n")

## 95% PI

# Berechnung der mittleren Effektgröße und ihrer Varianz
mu_hat <- res_hc$b
var_mu <- res_hc$vb
tau2 <- res_hc$sigma2

# Kritischer z-Wert für 95%-PI
z <- qnorm(0.975)

# Grenzen des Prädiktionsintervalls
PI_lower <- mu_hat - z * sqrt(var_mu + tau2)
PI_upper <- mu_hat + z * sqrt(var_mu + tau2)

# Ausgabe
cat("95% Prädiktionsintervall: [", PI_lower, ", ", PI_upper, "]\n")

review_p %>%
  select(no, author_s, out_category)

review_c %>%
  select(author_s, out_category)
