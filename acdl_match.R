# Lire les bases
base_brevets <- read.csv("base_brevets.csv", stringsAsFactors = FALSE)
base_emp <- read.csv("base_emp.csv", stringsAsFactors = FALSE)

# Apparier les bases
base_emp_inno <- merge(base_emp, base_brevets, by.x = "firm_name", by.y = "firm_name", all = FALSE)

# Vérifier les résultats
head(base_emp_inno)

#enregistrer la base
write.csv(base_emp_inno, "base_emp_inno.csv", row.names = FALSE)


