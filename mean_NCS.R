############################################################################################
###          Paper : https://onlinelibrary.wiley.com/doi/abs/10.1111/joes.12420          ###
############################################################################################
rm(list = ls()) #supprimer tous les objets 

### Chargement des packages ----

library(tidyverse)
library(questionr)
library(RPostgres)
library(gtsummary)
library(openxlsx2)
library(openxlsx)
library(readxl)
library(openalexR)
library(purrr)
library(httr)
library(jsonlite)
library(progressr)


### Lecture des données ----
# Economics
bdd_eco <- read_excel("~/Documents/gender diversity AM-YG/bdd eco oracle2.xlsx", 
                              sheet = "Feuil1")

# Management
bdd_gest <- read_excel("~/Documents/gender diversity AM-YG/bdd gest oracle.xlsx", 
                              sheet = "Feuil1")


# Calcul des moyennes par année par type collaboration M-W


result <- aggregate(bdd_eco$Cit_Rel_ALL_iac, 
                    by = list(Annee_Bibliographique = bdd_eco$Annee_Bibliographique, 
                              W_single = bdd_eco$W_single, 
                              M_single = bdd_eco$M_single, 
                              W_W = bdd_eco$`W-W`, 
                              M_M = bdd_eco$`M-M`, 
                              M_W = bdd_eco$`M-W`),
                    FUN = mean)


# Supposons que votre dataframe s'appelle bdd_eco
bdd_eco_long <- bdd_eco %>%
  pivot_longer(cols = c("W_single", "M_single", `W-W`, `M-M`, `M-W`),
               names_to = "gender_collab",
               values_to = "binary_value")%>%
  filter(binary_value == 1)

# Utilisez la fonction aggregate pour calculer la moyenne
result <- aggregate(Cit_Rel_ALL_iac ~ gender_collab + Annee_Bibliographique, data = bdd_eco_long, FUN = mean)



# Créez une fonction personnalisée pour calculer la moyenne et l'intervalle de confiance
calculate_mean_and_ci <- function(x) {
  mean_value <- mean(x)
  n <- length(x)
  std_error <- sd(x) / sqrt(n)
  z <- qnorm(0.975) # 0.975 pour un intervalle de confiance de 95%
  lower_ci <- mean_value - z * std_error
  upper_ci <- mean_value + z * std_error
  return(c(Mean = mean_value, LowerCI = lower_ci, UpperCI = upper_ci))
}

# Utilisez la fonction tapply pour calculer la moyenne et l'intervalle de confiance
result <- tapply(bdd_eco_long$Cit_Rel_ALL_iac, 
                 list(gender_collab = bdd_eco_long$gender_collab, 
                      Annee_Bibliographique = bdd_eco_long$Annee_Bibliographique),
                 FUN = calculate_mean_and_ci)



# Convertissez la matrice en un dataframe
result_df <- as.data.frame(result)

# Ajoutez des noms de colonnes appropriés pour chaque année
colnames(result_df) <- unique(colnames(result))

# Ajoutez les variables "gender_collab" en tant que colonne
result_df$gender_collab <- rownames(result_df)

# Utilisez la bibliothèque tidyr pour réorganiser les données
library(tidyr)
result_df <- pivot_longer(result_df, cols = -gender_collab, names_to = "Annee_Bibliographique", values_to = "Values")

# Divisez le vecteur de trois valeurs en trois colonnes distinctes
result_df <- separate(result_df, Values, into = c("Mean_NCS", "LowerCI_NCS", "UpperCI_NCS"), sep = ",")

# Supprimez les caractères alphabétiques des colonnes
result_df$Mean_NCS <- gsub("c\\(Mean = |\\)", "", result_df$Mean_NCS) %>% as.numeric()
result_df$LowerCI_NCS <- gsub("LowerCI = ", "", result_df$LowerCI_NCS) %>% as.numeric()
result_df$UpperCI_NCS <- gsub("UpperCI = |\\)", "", result_df$UpperCI_NCS) %>% as.numeric()


# Calculer le nombre de chaque gender_collab par année à partir de bdd_eco_long
counts <- bdd_eco_long %>%
  group_by(Annee_Bibliographique, gender_collab) %>%
  summarize(Count_gender_collab = n())

counts$Annee_Bibliographique <- as.numeric(counts$Annee_Bibliographique)
result_df$Annee_Bibliographique <- as.numeric(result_df$Annee_Bibliographique)

# Ajouter la colonne Count_gender_collab dans result_df
result_df <- result_df %>%
  left_join(counts, by = c("Annee_Bibliographique", "gender_collab"))
