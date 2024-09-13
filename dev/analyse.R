# library
library(meta) ; library(metafor) ; library(ggplot2) ; library(dplyr)

# data extraction
data <- readxl::read_excel("metaPreg.xlsx")

# group by major adverse event
data_major_outcomes <- data %>% 
  filter(outcome == "Major congenital malformations" & grepl("(All indications)", exposition)) %>% 
  filter(exposition!="Hydroxychloroquine (All indications except Antiphospholipid Syndrom)") %>%
  filter(controlType !=1) %>% 
  filter(controlType!=3)

# Calculer les tailles d'effet avec correction de la solution de continuité
escalc_results <- escalc(
  measure = "OR",       # Remplacer par la mesure appropriée (e.g., "OR" pour odds ratio)
  ai = X1,              # Nombre d'événements dans le groupe traité
  bi = N1 - X1,         # Nombre de non-événements dans le groupe traité
  ci = X0,              # Nombre d'événements dans le groupe témoin
  di = N0 - X0,         # Nombre de non-événements dans le groupe témoin
  data = data_major_outcomes,
  add = 0.5,            # Correction de la solution de continuité
  to = "only0"          # Correction appliquée uniquement aux études avec zéro événement
)

# rename variable
names(escalc_results)[32] <- "OR"
names(escalc_results)[33] <- "variance" 

# vecteur nom médoc
drug <- escalc_results$exposition %>% 
  unique()


for (i in 1:length(drug)) {
  # selectionne le medicament dans la l'objet drug
  select_drug <- escalc_results %>%
    filter(exposition == drug[i])
  
  # meta-analyse
    res <- meta::metabin(
      event.e = X1,
      n.e = N1,
      event.c = X0,
      n.c = N0,
      subset = exposition == drug[i],
      subgroup = controlType,
      data = select_drug,
      sm = "OR",
      incr = 0.5,
      method.incr = "only0",
    )
    
    # extraction TE et seTE
    res_TE <- list(TE = res$TE.random.w, seTE = res$seTE.random.w)
    
    # stockage des résultats
    if (i == 1) {OR_MA <- list(res_TE)}
    if (i > 1) {OR_MA <- append(OR_MA, list(res_TE))}
    
}

# 
names(OR_MA) <- drug

# meta analyse en sous groupe
metafor::rma(OR, variance, data=escalc_results, subset=exposition)
