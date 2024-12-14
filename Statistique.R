merge_data = read.csv("base_emp_inno.csv")
data_brevet = read.csv("base_brevets.csv")
data_emp = read.csv("base_emp.csv")


#install.packages("knitr")
#library(knitr)
stats_n_patents <- base_brevets %>%
  summarize(
    min = min(n_patents, na.rm = TRUE),
    median = median(n_patents, na.rm = TRUE),
    max = max(n_patents, na.rm = TRUE),
    mean = mean(n_patents, na.rm = TRUE),
    sd = sd(n_patents, na.rm = TRUE),
    missing = sum(is.na(n_patents))
  )
#On renomme les lignes
stats_n_patents <- stats_n_patents %>%
  rename(
    Minimum = min,
    Médiane = median,
    Maximum = max,
    Moyenne = mean,
    Ecart_Type = sd,
    Manquants = missing
  )

Ajouter une ligne nommée
stats_n_patents <- stats_n_patents %>%
  mutate(stat_name = "n patents") %>%
  relocate(stat_name)  # Déplacer la colonne 'stat_name' en premier

print(stats_n_patents)
#| echo: false
kable(stats_n_patents, caption = "Statistiques pour n_patents", format="markdown")
