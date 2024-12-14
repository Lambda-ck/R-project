#ANDRE Emma
#DUCOS Jade
#LASCAUX Julien
#COUSSY Lucas




# Lire les bases
base_brevets <- read.csv("base_brevets.csv", stringsAsFactors = FALSE)
base_emp <- read.csv("base_emp.csv", stringsAsFactors = FALSE)


# Réaliser une jointure complète (full join) sur la colonne firm_name
base_emp_inno <- merge(base_emp, base_brevets, by = "firm_name", all = TRUE)

# Vérifier les résultats
head(base_emp_inno)

write.csv(base_emp_inno, "base_emp_inno.csv", row.names = FALSE)
