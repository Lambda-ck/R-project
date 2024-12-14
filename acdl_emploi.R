#ANDRE Emma
#DUCOS Jade
#LASCAUX Julien
#COUSSY Lucas


# Lire le fichier en séparant les champs par des virgules
data <- read.csv("data/emp_offers_fmt.tsv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Charger les bibliothèques nécessaires
library(dplyr) # Pour la manipulation de données

#charger les fonctions nécessaires : 
source("acdl_src_utilities.R")
#ls()

#création firm_name####

#mettre toutes les entreprises en majuscules
#pour eviter les doublons par exemple "Orano" et "orano"
#=> harmoniser les données 
data$entreprise <- toupper(data$entreprise)
# Extraire les noms uniques des entreprises
firm_name <- unique(data$entreprise[!is.na(data$entreprise) & data$entreprise != ""])

# Créer une base base_emp avec la colonne firm_name
base_emp <- data.frame(firm_name = firm_name)

#création n_offres####

# Initialiser la nouvelle colonne n_offres dans base_emp
base_emp$n_offres <- sapply(base_emp$firm_name, fonction_n_offres)

#création de sector_main####

# Initialiser la nouvelle colonne sector_main dans base_emp
base_emp$sector_main <- sapply(base_emp$firm_name, fonction_sector_main)

# Vérifier les résultats
#head(base_emp)

#création de avg_req_exp####

# Initialiser la nouvelle colonne avg_req_exp dans base_emp
base_emp$avg_req_exp <- sapply(base_emp$firm_name, fonction_avg_req_exp)

# Vérifier les résultats
#head(base_emp)

#Création de top_skill_req####

# Initialiser la nouvelle colonne top_skill_req dans base_emp
base_emp$top_skill_req <- sapply(base_emp$firm_name, fonction_top_skill_req)

# Vérifier les résultats
#head(base_emp)

#création de avg_wage####

#problème : on veut le salaire annuel sauf qu'il y a des salaires journaliers,
#hebdomadaires etc.. 
#On a décidé de prendre en compte que les salaires annuels, les autres on en prend pas compte


# Appliquer la fonction extract_annual_salary à la colonne salaire
data$annual_salary <- sapply(data$salaire, extract_annual_salary)

# Vérifier les résultats
#head(data$annual_salary)

# Initialiser la nouvelle colonne avg_wage dans base_emp
base_emp$avg_wage <- sapply(base_emp$firm_name, moyenne_salaire_annuel)

# Multiplier par 100 pour ajouter deux zéros (équivalent de rajouter "00" à la fin)
base_emp$avg_wage <- base_emp$avg_wage * 100

# Vérifier les résultats
#head(base_emp)

#création de addr_dept_main####
# Initialiser la nouvelle colonne addr_dept_main dans base_emp
base_emp$addr_dept_main <- sapply(base_emp$firm_name, filtrer_dept)

# Vérifier les résultats
head(base_emp)

edit(base_emp)
View(base_emp)

#enregistrer la base
write.csv(base_emp, "base_emp.csv", row.names = FALSE)


