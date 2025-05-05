#### Funnel Plots

### well-being

funnel(res_wb, level=c(90, 95, 99), refline=0, legend=TRUE)

## Rank Correlation Test for Funnel Plot Asymmetry

ranktest(res_wb)

### stress

funnel(res_stress, level=c(90, 95, 99), refline=0, legend=TRUE)

## Rank Correlation Test for Funnel Plot Asymmetry

ranktest(res_stress)

### healthcare

funnel(res_hc, level=c(90, 95, 99), refline=0, legend=TRUE)

## Rank Correlation Test for Funnel Plot Asymmetry

ranktest(res_hc)

## Sensitivity Analysis

# Schwellenwert für Standardfehler (z. B. oberstes Quartil)
se_threshold <- quantile(review_hc$se_effect_post, 0.75)

# Daten ohne Studien mit großen Standardfehlern
review_hc_sens <- review_hc %>%
  filter(se_effect_post <= se_threshold)

# Wiederholung der Metaanalyse
res_sens <- rma.mv(
  yi = effect_size_post, 
  V = se_effect_post^2, 
  random = ~ 1 | id, 
  data = review_hc_sens, 
  method = "REML"
)

# Ergebnisse der Sensitivitätsanalyse

print(summary(res_sens))
model_parameters(res_sens, summary = T)

# Schwellenwerte für Effektgrößen (oberste und unterste 5%)
effect_thresholds <- quantile(review_hc$effect_size_post, c(0.05, 0.95))

# Daten ohne Extremwerte
review_hc_no_outliers <- review_hc %>%
  filter(effect_size_post >= effect_thresholds[1], 
         effect_size_post <= effect_thresholds[2])

# Wiederholung der Metaanalyse
res_no_outliers <- rma.mv(
  yi = effect_size_post, 
  V = se_effect_post^2, 
  random = ~ 1 | id, 
  data = review_hc_no_outliers, 
  method = "REML"
)

# Ergebnisse des ursprünglichen Modells
print(summary(res_hc))

# Vergleiche mit Sensitivitätsanalysen
print(summary(res_sens))           # Analyse ohne große SE
print(summary(res_no_outliers))    # Analyse ohne Extremwerte

#### Forest Plots

### well-being

forest(res_wb,
       slab = review_wb$author_s,  # Nutze die neue Variable `author_s`
       xlab = "Effect Size (SMD)", 
       mlab = "RE Model",
       cex = 0.8)

### stress

forest(res_stress,
       slab = review_stress$author_s,  # Nutze die neue Variable `author_s`
       xlab = "Effect Size (SMD)", 
       mlab = "RE Model",
       cex = 0.8)

### healthcare

forest(res_hc,
       slab = review_hc$author_s,  # Nutze die neue Variable `author_s`
       xlab = "Effect Size (SMD)", 
       mlab = "RE Model",
       cex = 0.8)

review_p %>%
  select(author_s, out_category)
