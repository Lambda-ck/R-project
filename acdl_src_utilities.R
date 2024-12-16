fonction_n_offres <- function(x) {
  # Compter les occurrences de l'entreprise x dans la colonne entreprise du fichier original
  sum(data$entreprise == x, na.rm = TRUE)
}

fonction_sector_main <- function(x) {
  # Extraire les valeurs uniques de la colonne secteur correspondant à l'entreprise x
  secteurs <- unique(data$secteur[data$entreprise == x])
  
  # Si plusieurs secteurs sont trouvés, choisir le premier, sinon NA
  if (length(secteurs) > 0) {
    return(secteurs[1])
  } else {
    return(NA)
  }
}

fonction_avg_req_exp <- function(x) {
  # Extraire les valeurs de la colonne experience_requise pour l'entreprise x
  exp_values <- data$experience_requise[data$entreprise == x]
  
  # Calculer la moyenne en ignorant les valeurs NA et arrondir à 2 chiffres après la virgule
  if (length(exp_values) > 0 && !all(is.na(exp_values))) {
    return(round(mean(exp_values, na.rm = TRUE), 2))
  } else {
    return(NA)  # Si toutes les valeurs sont manquantes, retourner NA
  }
}

fonction_top_skill_req <- function(x) {
  # Extraire les compétences requises pour l'entreprise x
  skills <- data$competences_requises[data$entreprise == x]
  
  # Séparer les compétences en individuels, supprimer les NA, et combiner en un vecteur
  skills <- unlist(strsplit(skills[!is.na(skills)], ",\\s*"))
  
  # Identifier les compétences mentionnées au moins deux fois
  if (length(skills) > 0) {
    skill_counts <- table(skills)
    top_skills <- names(skill_counts[skill_counts >= 2])  # Compétences mentionnées au moins 2 fois
    return(paste(top_skills, collapse = ", "))
  } else {
    return(NA)  # Si aucune compétence n'est trouvée
  }
}

extract_annual_salary <- function(salary_text) {
  if (is.na(salary_text)) return(NA)  # Retourner NA si la valeur est manquante
  
  # Vérifier si le texte mentionne "jour", "semaine", ou "mois", et ignorer ces cas
  if (grepl("jour|semaine|mois", salary_text, ignore.case = TRUE)) return(NA)
  
  # Nettoyer le texte pour standardiser les nombres
  salary_text <- gsub("[^0-9,\\.\\s-]", "", salary_text)  # Garder chiffres, points, virgules et tirets
  salary_text <- gsub("\\s+", "", salary_text)            # Supprimer tous les espaces insécables
  
  # Extraire les nombres présents dans le texte
  numbers <- unlist(regmatches(salary_text, gregexpr("\\d{1,3}(?:[.,]\\d{3})*(?:[.,]\\d+)?", salary_text)))
  
  # Convertir les nombres extraits en format numérique
  numbers <- as.numeric(gsub(",", ".", gsub("\\.", "", numbers)))
  
  # Si une fourchette est donnée, prendre la valeur minimale (premier nombre)
  if (length(numbers) >= 2) {
    return(numbers[1])  # Retourner la première valeur (ou changer pour mean(numbers) si besoin)
  } else if (length(numbers) == 1) {
    return(numbers[1])  # Retourner le seul nombre trouvé
  } else {
    return(NA)  # Aucun nombre valide trouvé
  }
}

moyenne_salaire_annuel <- function(x) {
  # Filtrer les salaires annuels pour l'entreprise x
  salaries <- data$annual_salary[data$entreprise == x]
  
  # Calculer la moyenne des salaires annuels en ignorant les NA
  if (length(salaries) > 0 && !all(is.na(salaries))) {
    return(round(mean(salaries, na.rm = TRUE), 2))  # Arrondir à 2 chiffres après la virgule
  } else {
    return(NA)  # Si aucune donnée valide n'est trouvée
  }
}

filtrer_dept <- function(x) {
  # Filtrer les départements associés à l'entreprise x
  depts <- data$departement[data$entreprise == x]
  
  # Identifier le département qui apparaît le plus souvent
  if (length(depts) > 0 && !all(is.na(depts))) {
    dept_counts <- table(depts)  # Compter les occurrences
    return(names(dept_counts[which.max(dept_counts)]))  # Retourner le département le plus fréquent
  } else {
    return(NA)  # Si aucun département valide n'est trouvé
  }
}

