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

# Es ist sinnvoll, eine Kopie des Data Frames als CSV-Datei auf dem lokalen PC zu speichern, um:
# - Daten dauerhaft zu sichern, auch nach Beendigung der R-Sitzung.
# - Daten mit anderen Programmen oder Personen zu teilen.
# - Einen bestimmten Zustand des Datensatzes für spätere Analysen oder Vergleiche zu bewahren.
# Wir verwenden die data_write() Funktion aus dem easystats Paket

data_write(suicide_data, "./suicide_data.csv")

#### Kleine Datenvisualisierungen

# mit dem paker sjPlot können wir supereinfache Visualisierungen erstellen

## Balkendiagramm für Altersgruppe (age_group)

suicide_data %>% select(age_group) %>% plot_frq()

# Wir können diese Plots sehr einfach anpassen

suicide_data %>% select(age_group) %>% plot_frq(title = "Verteilung der Altersgruppen")

# Wir können auch mittels "themes" sehr einfach die Visualisierung ändern

suicide_data %>% select(age_group) %>% plot_frq(title = "Verteilung der Altersgruppen") +
  see::theme_modern()

## Histogramm für mean in der Kontrollgruppe

suicide_data %>% select(mean_c) %>% plot_frq() +
  see::theme_modern()

