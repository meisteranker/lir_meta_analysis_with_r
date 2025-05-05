# Das Skript "MetaAnalyseMitR" bietet eine Einführung in die Durchführung von Meta-Analysen mit R, speziell für Einsteiger. Anhand des Beispieldatensatzes "SuicidePrevention" 
# vom metapsy Projekt werden grundlegende Konzepte und Schritte der Meta-Analyse erläutert.
# Das Skript ist in mehrere Abschnitte unterteilt, die jeweils einen bestimmten Aspekt der Meta-Analyse behandeln.
# In R wird das Zeichen # verwendet, um Kommentare im Code zu schreiben, die vom Programm ignoriert werden.

#### Benötigte Pakete installieren

# Pakete sind Sammlungen von Funktionen und Datensätzen, die zusätzliche Funktionalitäten bereitstellen.
# Bevor wir ein Paket verwenden können, müssen wir es zunächst mit dem Befehl install.packages("Paketname") installieren.
# Wenn die Pakete bereits installiert sind, kann dieser Abschnitt übersprungen werden.

# Tidyverse ist eine Sammlung von R-Paketen, die für Datenmanipulation und -visualisierung nützlich sind.
# install.packages("tidyverse")

# Easystats ist eine Sammlung von Paketen, die für statistische Analysen und Visualisierungen nützlich sind.
# install.packages("easystats")

# Meta ist ein Paket, das speziell für die Durchführung von Meta-Analysen entwickelt wurde.
# install.packages("meta")

# Metafor ist ein weiteres Paket für Meta-Analysen, das erweiterte Funktionen bietet.
# install.packages("metafor")

# sjPlot ist ein Paket, das für die Visualisierung von statistischen Modellen und Daten nützlich ist.

# install.packages("sjPlot")

#### Pakete laden

# Nachdem die Pakete installiert sind, müssen sie in jeder R-Sitzung geladen werden, um ihre Funktionen nutzen zu können.

library(tidyverse)
library(easystats)
library(meta)
library(metafor)
library(sjPlot)
library(esc)

#### Datensatz herunterladen

# Die URL des Datensatzes lautet: "https://www.protectlab.org/wp-content/uploads/2023/04/SuicidePrevention.csv"
# wenn der Datensatz lokal gespeichert wurde, muss er in R geladen werden.
# Wir verwenden die Funktion data_read() aus dem easystats-Paket, um den Datensatz zu laden.
# Diese Funktion ist eine Abkürzung für read_csv() aus dem tidyverse-Paket.
# Es lohnt sich meist, den Datensatz möglichst kurz zu benennen, um die Arbeit damit zu erleichtern.

# Lade den Datensatz
suicide_data <- data_read("./SuicidePrevention.xlsx")

#### Erster Überblick über den Datensatz

# Mit der Funktion glimpse() aus dem tidyverse-Paket erhalten wir einen ersten Überblick über die Struktur des Datensatzes.
# Diese Funktion zeigt die Variablen im Datensatz, deren Typen und einige Beispielwerte an.

glimpse(suicide_data)

## Datentypen in R
# <num>: Numerische Daten, z.B. 1.02, 42, -3.14
# Verwendet für Berechnungen und mathematische Operationen

# <chr>: Zeichenketten, z.B. "Hallo", "R ist großartig!", "2023"
# Verwendet für Textdaten wie Wörter oder Sätze

# <log>: Logische Werte, TRUE oder FALSE
# Verwendet für Bedingungen und Kontrollstrukturen

# <factor>: Faktoren, kategoriale Daten, z.B. "männlich", "weiblich", "divers"
# Intern als Zahlen gespeichert, jede Zahl steht für eine Kategorie

#### Datentypen umwandeln

# Einige Datentypen wie "chr" für numerische Werte (z.B. mean.e) sind inkorrekt und können zu Problemen bei der Analyse führen.
# Mit der Funktion mutate() aus dem tidyverse-Paket können wir diese Datentypen umwandeln.
# Gleichzeitig können wir die Variablen umbenennen, da Punkte in Variablennamen oft ungünstig sind.
# Unterstriche (_) sind eine gute Alternative zu Punkten in Variablennamen.

suicide_data <- suicide_data %>% mutate(n_e = as.numeric(n.e))
suicide_data <- suicide_data %>% mutate(mean_e = as.numeric(mean.e))
suicide_data <- suicide_data %>% mutate(sd_e = as.numeric(sd.e))
suicide_data <- suicide_data %>% mutate(n_c = as.numeric(n.c))
suicide_data <- suicide_data %>% mutate(mean_c = as.numeric(mean.c))
suicide_data <- suicide_data %>% mutate(sd_c  = as.numeric(sd.c))
suicide_data <- suicide_data %>% mutate(age_group = as.factor(age_group))
suicide_data <- suicide_data %>% mutate(pubyear = as.factor(pubyear))

#### Datensatz speichern

# Es ist sinnvoll, eine Kopie des Data Frames als Rds/Rdata-Datei auf dem lokalen PC zu speichern, um:
# - Daten dauerhaft zu sichern, auch nach Beendigung der R-Sitzung.
# - Daten mit anderen Personen zu teilen.
# - Einen bestimmten Zustand des Datensatzes für spätere Analysen oder Vergleiche zu bewahren.
# Man kann natürlich auch als csv speichern, allerdings gehen dann die oben durchgeführten
# Transformationen der Daten wieder verloren
# Wir verwenden die data_write() Funktion aus dem easystats Paket

data_write(suicide_data, "./suicide_data.rdata")

#### Kleine Datenvisualisierungen

# mit dem paker sjPlot können wir einfache Visualisierungen erstellen

## Balkendiagramm für Altersgruppe (age_group)

suicide_data %>% select(age_group) %>% 
  plot_frq(ylim = c(0, max(table(suicide_data$age_group)) * 1.4))

# Wir können diese Plots sehr einfach anpassen

suicide_data %>% select(age_group) %>% 
  plot_frq(ylim = c(0, max(table(suicide_data$age_group)) * 1.4), 
           title = "Verteilung der Altersgruppen")

# Wir können auch mittels "theme" sehr einfach die Visualisierung ändern

plot_frq(
  suicide_data$age_group,
  ylim = c(0, max(table(suicide_data$age_group)) * 1.4),
  title = "Verteilung der Altersgruppen") +
  theme_classic()

### Effektgröße (hier SMD)

# Wir könnten die SMD natürlich auch per "Hand" berechnen, nutzen aber lieber das metafor Paket
# mit der Funktion "escalc"
# Berechne für jede Studie die Effektstärke (smd = Hedges' g) und deren Varianz (var_smd).
# Diese Werte können zB für Forest-Plots oder Meta-Analysen mit 'rma()' verwendet werden.

suicide_data <- escalc(
  measure = "SMD",
  m1i = mean_e, sd1i = sd_e, n1i = n_e,
  m2i = mean_c, sd2i = sd_c, n2i = n_c,
  data = suicide_data,
  var.names = c("smd", "var_smd"))

### Meta Analyse: Fixed-Effect Model
# Berechne eine Fixed-Effect Meta-Analyse (d.h. es wird angenommen, 
# dass alle Studien denselben "wahren" Effekt messen).
# Die Effektstärken (Hedges' g) stammen aus den Mittelwerten, 
# SDs und Gruppengrößen der einzelnen Studien.

meta_fixed <- metagen(
  TE = smd,
  seTE = sqrt(var_smd),
  studlab = paste(author, pubyear),
  data = suicide_data,
  sm = "SMD",
  common = TRUE,   
  random = FALSE)

# Wir können uns eine Zusammenfassung der Ergebnisse ausgeben lassen
# Für jede Studie: geschätzte Effektstärke (SMD), 95%-Konfidenzintervall und Gewicht im Modell
# "Common effect model": das Ergebnis der Fixed-Effect Meta-Analyse (ein gewichteter Mittelwert)
# "Quantifying heterogeneity": zeigt, wie unterschiedlich die Studien sind (tau², I², Q-Test)

summary(meta_fixed)

## Forest Plot

forest(meta_fixed,
       sortvar = TE,             # sortiert Studien nach Effektstärke
       xlab = "Standardisierte Mittelwertdifferenz (SMD)",
       leftcols = c("studlab"),  # zeigt Studiennamen links
       rightcols = c("effect", "ci"),  # zeigt SMD und Konfidenzintervall rechts
       digits = 2,               # auf 2 Nachkommastellen runden
       colgap = unit(6, "mm"),   # Abstand zwischen Textspalten
       print.tau2 = FALSE,       # tau braucht man hier nicht (Fixed-Effect)
       common = TRUE,            # zeigt den gemeinsamen Effekt (Fixed)
       random = FALSE            # kein Random-Effects-Modell anzeigen
)

### Meta Analyse: Random-Effects Model
# Random-Effects-Modell berücksichtigt, dass sich die Studien in ihrer Wirkung unterscheiden können
# Schätzt nicht nur den durchschnittlichen Effekt, sondern auch die Streuung der Effekte zwischen Studien
# Wir nutzen hier Restricted Maximum Likelihood (REML) ist ein Schätzverfahren, das bei 
# Random-Effects-Modellen verwendet wird, um die zwischenstudienbezogene Varianz möglichst 
# unverzerrt zu schätzen, es gibt auch noch andere Schätzer (DerSimonian-Laird usw.)

meta_random <- metagen(
  TE = smd,
  seTE = sqrt(var_smd),
  studlab = paste(author, pubyear),
  data = suicide_data,
  sm = "SMD",           # standardisierte Mittelwertdifferenz
  method.tau = "REML",  # gleiche Methode wie im metafor-Modell
  common = FALSE,        # kein Fixed-Effect Modell
  random = TRUE         # Random-Effects Modell
)

## Summary

summary(meta_random)

## Forest Plot

forest(meta_random,
       sortvar = TE,               # sortiert Studien nach Effektstärke
       xlab = "Standardisierte Mittelwertdifferenz (SMD)",
       leftcols = c("studlab"),    # Studienbezeichnung links
       rightcols = c("effect", "ci", "w.random"),  # Effekt + 95%-KI rechts + Gewichtung
       digits = 2,                 # Zwei Nachkommastellen
       colgap = unit(6, "mm"),     # Spaltenabstand
       common = FALSE,             # kein Fixed-Effekt anzeigen
       random = TRUE,              # Random-Effects Modell anzeigen
       print.tau2 = F,             # zeigt tau² (Heterogenität)
       fontsize = 10 
)

### Contour-enhanced funnel plots
# Der Funnel Plot zeigt die Streuung der Effektgrößen in Abhängigkeit von ihrer Präzision.
# Symmetrie deutet auf wenig Publikationsbias hin.
# Die farbigen Bereiche markieren Konfidenzniveaus (zB. p < .01, .05, .10).
# Fehlen von Studien in den hellen Bereichen kann auf Publikationsbias hinweisen.

# Farben für Signifikanzbereiche definieren
col.contour <- c("gray75", "gray85", "gray95")

# Funnel Plot erzeugen (ohne zusätzliche Labels)
meta::funnel(meta_random,
             xlab = "Standardisierte Mittelwertdifferenz (SMD)",
             xlim = c(-0.5, 1.5),        # Achsenbegrenzung anpassen
             contour = c(0.9, 0.95, 0.99),   # p-Wert-Grenzen
             col.contour = col.contour)     # Füllfarben für Konturen

# Legende hinzufügen
legend(x = 1.1, y = 0.02, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour, cex = 0.8)

# Titel ergänzen
title("Contour-Enhanced Funnel Plot (SMD, Random-Effects Modell)")

### Egger´s Test
# Egger's Test untersucht, ob es Hinweise auf Publikationsbias gibt.
# Fall, wenn Studien mit kleinen Stichproben tendenziell größere oder extremere Effekte berichten 
# Test basiert auf einer linearen Regression zwischen Effektstärken und deren Standardfehlern.
# signifikantes Ergebnis deutet auf asymmetrische Verteilung im Funnel Plot hin

metabias(meta_random, method.bias = "linreg", k.min=9)
