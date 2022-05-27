# Automatisation de traitement de données INSEE


``` r
library(readxl)
library(tidyverse)
library(ggpubr)
library(scales)
library(showtext)
library(viridis)
library(reshape2)
```

``` r
setwd("X:/chemin vers le/dossier/avec les données")

```

## Importation des données

``` r
data_pop18 <- read_excel("bases_brutes/Données INSEE 2018/base-cc-evol-struct-pop-2018/base-cc-evol-struct-pop-2018.xlsx", 
                                              range = "A6:DD34957")

data_pop13 <- read_excel("bases_brutes/Données INSEE 2018/base-cc-evol-struct-pop-2018/base-cc-evol-struct-pop-2018.xlsx", 
                                       range = "A6:DD34957", sheet = 2)

data_hist18 <- read_excel("bases_brutes/Données INSEE 2018/base-cc-serie-historique-2018/base-cc-serie-historique-2018.xlsx", 
                                            range = "A6:DD34954")

intercoms <- read_excel("bases_brutes/Intercommunalite-Metropole_au_01-01-2020_v1.xlsx" , sheet = 2 , range = "A6:F34974") 
```

## Indicateurs possibles avec la base "base-cc-evol-struct-pop-2018" 

Nous pouvons obtenir différents indicateurs avec cette base de donnés. 
Ces indicateurs peuvent être regroupés sous la catégorie "contexte démographique", qui est elle-même, divisée en deux sous-catégories:

1. Sexe et âge 2018
2. Migrations résidentielles 


# 1. SEXE ET AGE 2018 - 2013 

Compilation des informations pour l'EPCI

``` r
codepci = "200057958"
```
Extraction des codes des communes de l'EPCI

``` r
communes <- intercoms[intercoms$EPCI == codepci, c(1,2,3)]
``` 

## 1.1 Traitement de données et création du tableau: Population par tranches 20XX.csv 
 
Dans cette première section, nous allons nous concentrer sur les informations permettant d'obtenir: 
Population par tranches d'âge de l'EPCI (sans discrimination par sexe)


``` r
info_ages18 <- data_pop18[,c("CODGEO","P18_POP0014", "P18_POP1529","P18_POP3044","P18_POP4559",
                           "P18_POP6074", "P18_POP7589","P18_POP90P","P18_POP")]

info_ages13 <- data_pop13[,c("CODGEO","P13_POP0014", "P13_POP1529","P13_POP3044","P13_POP4559",
                             "P13_POP6074", "P13_POP7589","P13_POP90P","P13_POP")]

```

Jusqu'ici nous avons les informations pour la France entière. Pourtant, ce qui nous intéresse sont les information pour une EPCI en particulière. Pour cela, nous allons faire une jointure entre les info d'intérêt et les communes d'intérêt. 
Cela nous permettra de conserver les informations que pour l'EPCI.

``` r
tranches_dage18 <- merge(x = communes[,-3], y = info_ages18,
                       by.x = "CODGEO", by.y = "CODGEO", all.x = T)
tranches_dage13 <- merge(x = communes[,-3], y = info_ages13,
                         by.x = "CODGEO", by.y = "CODGEO", all.x = T)
``` 

Nous allons arrondir les valeurs et conserver juste un décimal:

``` r
tranches_dage18[,-c(1,2)] <- lapply(tranches_dage18[,-c(1,2)], round, 1)
tranches_dage13[,-c(1,2)] <- lapply(tranches_dage13[,-c(1,2)], round, 1)
``` 

### Population par tranches pour l'epci complet

``` r
popepci_tranches18 <- colSums(tranches_dage18[,-c(1,2)])
popepci_tranches13 <- colSums(tranches_dage13[,-c(1,2)])

#Nous allons annexer des colonnes avec les pourcentages para tranche d'âge, par commune.

tranches_dage18 <- mutate(tranches_dage18, "%1" = P18_POP0014/P18_POP,
                                           "%2" = P18_POP1529/P18_POP,
                                           "%3" = P18_POP3044/P18_POP,
                                           "%4" = P18_POP4559/P18_POP,
                                           "%5" = P18_POP6074/P18_POP,
                                           "%6" = P18_POP7589/P18_POP,
                                           "%7" = P18_POP90P/P18_POP)

tranches_dage13 <- mutate(tranches_dage13, "%1" = P13_POP0014/P13_POP,
                                           "%2" = P13_POP1529/P13_POP,
                                           "%3" = P13_POP3044/P13_POP,
                                           "%4" = P13_POP4559/P13_POP,
                                           "%5" = P13_POP6074/P13_POP,
                                           "%6" = P13_POP7589/P13_POP,
                                           "%7" = P13_POP90P/P13_POP)

#Pourcentages pour l'EPCI

EPCI_2018 <- c("-","EPCI", popepci_tranches18[1], popepci_tranches18[2], popepci_tranches18[3],popepci_tranches18[4], 
              popepci_tranches18[5], popepci_tranches18[6],popepci_tranches18[7],popepci_tranches18[8],
              popepci_tranches18[1]/popepci_tranches18[8], popepci_tranches18[2]/popepci_tranches18[8],
              popepci_tranches18[3]/popepci_tranches18[8], popepci_tranches18[4]/popepci_tranches18[8],
              popepci_tranches18[5]/popepci_tranches18[8], popepci_tranches18[6]/popepci_tranches18[8],
              popepci_tranches18[7]/popepci_tranches18[8])

tranches_dage18 <- rbind(tranches_dage18,EPCI_2018)
tranches_dage18[3:17] <- sapply(tranches_dage18[3:17], as.numeric)
```
Renommer et organiser les colonnes pour avoir plus de lisibilité:

``` r
tranches_dage18 <- tranches_dage18[, c(1,2,3,11,4,12,5,13,6,14,7,15,8,16,9,17,10)]
tranches_dage13 <- tranches_dage13[, c(1,2,3,11,4,12,5,13,6,14,7,15,8,16,9,17,10)]

colnames(tranches_dage18) <- c("Code INSEE","Echelle","0_14 ans","%I","15_29 ans",
                             "%II","30_44 ans","%III","45_59 ans","%IV",
                             "60_74 ans","%V","75_89 ans","%VI",
                             "90 ans ou plus","%VII","Ensemble")

colnames(tranches_dage13) <- c("Code INSEE","Echelle","0_14 ans","%I","15_29 ans",
                               "%II","30_44 ans","%III","45_59 ans","%IV",
                               "60_74 ans","%V","75_89 ans","%VI",
                               "90 ans ou plus","%VII","Ensemble")
```

### Exportation en csv.
``` r
dir.create(paste0("bases/",codepci,sep=""))
write_csv(tranches_dage13, paste0("bases/",codepci,"/","Population par Tranches 2013.csv"))
write_csv(tranches_dage18, paste0("bases/",codepci,"/","Population par Tranches 2018.csv"))
```

# 1.2 Traitement de données et création du tableau: Sexe et âges 2013-2018.csv  

Dans cette deuxième section, nous allons nous concentrer sur les informations permettant d'obtenir: L'évolution de la pyramide des âges 2013-2018.
Pour y parvenir, nous auront besoin des mêmes informations que ci-dessus, mais différenciées par sexe. 
``` r
info_hommes <- data_pop18[,c("CODGEO","P18_H0014", "P18_H1529","P18_H3044","P18_H4559",
                             "P18_H6074", "P18_H7589","P18_H90P","P18_POPH")]

```
Cette fois-ci, nous allons agréger les informations 2018 et 2013 dans le même tableaux car nous allons en avoir besoin, plus tard, dans le graphique de la pyramide d'âge. 

``` r
info_hommes <- merge(info_hommes, data_pop13[,c("CODGEO","P13_H0014", "P13_H1529","P13_H3044","P13_H4559",
                                                 "P13_H6074", "P13_H7589","P13_H90P","P13_POPH")], by = "CODGEO")


## Maintenant, nous allons répéter les processus mais pour les femmes. [...]
```

Jusqu'ici nous avons séparé les information démographiques qui nous intéressent pour dessiner une pyramide d'âge. Cependant, ces informations sont pour la France entière. Comme avant nous allons fusionner les info d'intérêt et les communes d'intérêt. 

``` r
info_hommes <- merge(x = communes[,-3], y = info_hommes,
                         by.x = "CODGEO", by.y = "CODGEO", all.x = T, all.y = F)
info_femmes <- merge(x = communes[,-3], y = info_femmes,
                         by.x = "CODGEO", by.y = "CODGEO", all.x = T, all.y = F)

#Nous allons arrondir les valeurs et conserver juste un décimal:
info_hommes[,-c(1,2)] <- lapply(info_hommes[,-c(1,2)], round, 1)
info_femmes[,-c(1,2)] <- lapply(info_femmes[,-c(1,2)], round, 1)
```

L'idée est de créer une pyramide pour chacune des communes de l'EPCI (et pour l'EPCI entière).
Pour cela  nous allons réorganiser les données, en laissant les communes comme des colonnes et les informations par tranche d'âge comme lignes. Dans d'autres mots, on va a transposer la matrice info_hommes et la matrice info_femmes.

``` r
tinfo_hommes <- as.data.frame(t(info_hommes[,-c(1,2)]))

#(!) On aura besoin de 4 colonnes: "Intercommunalité" = Somme de la population par tranche d'âge et sexe de toutes les communes de l'EPCI
#                                  "Sexe" = Indiquant à quel sexe appartient l'info
#                                  "Année" = Indiquant à quel année appartient l'info
#                                  "Tranches" = Indiquant la tranche d'âge de l'information     

tinfo_hommes$Intercommunalite <- rowSums(tinfo_hommes)
tinfo_hommes$Sexe <- "Hommes"
tinfo_hommes$Annee <- with(tinfo_hommes, ifelse(startsWith(rownames(tinfo_hommes), "P18"), "2018", "2013"))
tinfo_hommes$Tranches <- c("0_14 ans","15_29 ans","30_44 ans","45_59 ans","60_74 ans","75_89 ans",
                           "90 ans ou plus","Ensemble")

#Réorganisation et noms de colonnes.

colnames(tinfo_hommes)[which(!names(tinfo_hommes)%in%c("Sexe","Intercommunalite","Annee","Tranches"))] <- info_hommes$LIBGEO
tinfo_hommes <- tinfo_hommes[tinfo_hommes$Tranches != "Ensemble",]

#Maintenant pour les femmes [...]

#Jointure hommes et femmes:

info_pyramide <- bind_rows(tinfo_femmes,tinfo_hommes)
```

### Exportation en csv.
``` r
write_csv(info_pyramide, paste0("bases/",codepci,"/","Sexe et âges 2013-2018.csv"))
```

# AUTOMATISATION DES GRAPHIQUES 

Configuration pour plotter avec police Spectral et les couleurs de l'entreprise

font_add_google("Spectral", "Spectral")
windows()
showtext_auto()
color <- c("#6E56A8","#7AF09E","#3B2E5A","#26D07C")


## PYRAMIDE D'AGE

### Création d'une boucle qui reproduit une pyramide d'âge pour chaque commune:

#### Création du dossier où les images seront stockées:

dir.create(paste0("graphiques/",codepci,sep = ""))
dir.create(paste0("graphiques/",codepci,"/pyramides",sep = ""))

Cette ligne est fondamental pour le sucés de la boucle car on veut conserver que les noms de communes parmi les autres informations: 
``` r
nom_communes <- colnames(info_pyramide)[!names(tinfo_hommes)%in%c("Sexe", "Annee","Tranches")]
```

#### La boucle "for"

``` r
for (c in nom_communes){
  
  info_pyramide$commune <- info_pyramide[, c]
  
  
  pyramide <- ggplot(data = info_pyramide, aes(fill = interaction(Sexe, Annee, sep = "-"), 
                                               y = commune,
                                               x = Tranches)) +
    
    geom_col(position = "dodge") +
    scale_fill_manual(values = color, name = "Sexe et Année")+
    scale_y_continuous(labels = abs , expand = c(0,0)) +
    coord_flip() +
    facet_wrap(.~ Sexe, scale = "free_x") +
    theme(strip.text.x = element_blank(),panel.spacing.x = unit(0, "pt")) +
    
    labs(y = "Population", 
         title= "Evolution de la pyramide des âges entre 2013-2018") 
  
  pyramide <- pyramide + theme_classic() + 
    theme(text=element_text(size=12,  family= "Spectral"),
          strip.text.x = element_blank(),panel.spacing.x = unit(0, "pt"))
#(EX)  
  ggsave(filename = paste("graphiques/",codepci,"/pyramides","/", c,".jpeg", sep = ""), 
         device = "jpeg", plot = pyramide, type = 'cairo',width = 7, height = 7, dpi = 100)
  

  print(pyramide)
  
}


showtext_auto(FALSE) 
```
#### Example de résultat:
La boucle va générer le graphique suivant pour chacune des communes de l'EPCI. Si l'EPCI compose 100 communes, alors la boucle enregistrera 100 graphiques:


<img src= https://github.com/CarolinaCABL/Automatisation_graphs/blob/main/pyramide.jpg />


## GRAPHIQUE: "Structure de la population par tranche d'âge en 2013-2018"

### Préparation de la base de données

``` r
info_plotranches <- info_pyramide[,c("Intercommunalite","Sexe","Annee","Tranches")]
info_plotranches$Intercommunalite <- abs(info_plotranches$Intercommunalite)

info_plotranches <- aggregate(Intercommunalite ~ Annee + Tranches ,info_plotranches, sum)
info_plotranches$Total <- ifelse(info_plotranches$Annee == "2013", sum(tranches_dage13$Ensemble), sum(tranches_dage18[-nrow(tranches_dage18),]$Ensemble))
info_plotranches$Pourcentage <- info_plotranches$Intercommunalite / info_plotranches$Total
```

Les couleurs
``` r
color <- c("#213F40","#26D07C")
```

### Le graphique
``` r
windows()
showtext_auto()

plotranches <- ggplot(info_plotranches, aes(Tranches, Pourcentage, fill = Annee)) + 
               geom_col(position = "dodge") +
               scale_fill_manual(values=color) +
               scale_y_continuous(labels = percent)+
               labs(y = "%", 
                    title= "Structure de la population par tranche d'âge en 2013-2018") 

plotranches <- plotranches + theme_classic() + 
               theme(text=element_text(size=16,  family= "Spectral"),
                     axis.text = element_text(size = 16,face = "bold"))

showtext_auto(FALSE)
# (EX) Exportation

ggsave(filename = paste("graphiques/",codepci,"/","pourcentages_tranches_EPCI.jpeg", sep = ""), 
       device = "jpeg", plot = plotranches, type = 'cairo',width = 11, height = 7, dpi = 100)
```



## Indicateurs possibles avec "base-cc-serie-historique-2018" 


### 2. STRUCTURE URABAINE


#### 2.1 Densité des logements 

Il convient de rappeler qu'au début, nous avons défini "communes" comme les communes de l'EPCI. Si pour une raison ou une autre, dans cette deuxième partie, nous voulons travailler avec un autre EPCI, nous devrons redéfinir "communes".

``` r
codepci = "nouveau code EPCI"
communes <- intercoms[intercoms$EPCI == codepci, c(1,2,3)]
``` 

``` r
densite <- data_hist18[, c("CODGEO","SUPERF","D90_LOG","D99_LOG","P08_LOG","P13_LOG","P18_LOG")]

#Juste pour l'EPCI d'intérêt.
densite_logements <- merge(x = communes[,-3], y = densite,
                         by.x = "CODGEO", by.y = "CODGEO", all.x = T, all.y = F)
``` 

_Création des indicateurs:_ Logts par km^2, variation de logts et variation annuelle. Dans densite_logements nous avons déjà les informations qui nous intéressent, donc, nous pouvons faire les calculs dans le tableau directement en utilisant la fonction mutate.
``` r
densite_logements <- mutate(densite_logements,
                            "Logts/Km2_1990" = D90_LOG/SUPERF,
                            "Logts/Km2_1999" = D99_LOG/SUPERF,
                            "Logts/Km2_2008" = P08_LOG/SUPERF, 
                            "Logts/Km2_2013" = P13_LOG/SUPERF, 
                            "Logts/Km2_2018" = P18_LOG/SUPERF,
                            "Variation 1990-1999" = (D99_LOG - D90_LOG)/D90_LOG,
                            "Variation 1999-2008" =(P08_LOG - D99_LOG)/D99_LOG, 
                            "Variation 2008-2013" =(P13_LOG - P08_LOG)/P08_LOG, 
                            "Variation 2013-2018" =(P18_LOG - P13_LOG)/P13_LOG)

#On avait besoin de créer d'abord la densité de logts '90 et '18 pour calculer la variation annuelle

densite_logements$Variation_annuelle = (((densite_logements[,13])/(densite_logements[,9]))^(1/28))-1

#On change les noms des premiers colonnes
colnames(densite_logements)[colnames(densite_logements) %in% 
                              c("CODGEO", "LIBGEO","SUPERF","D90_LOG","D99_LOG","P08_LOG","P13_LOG","P18_LOG")] <- 
                              c("Code INSEE", "Echelle", "Superficie Km2", "Logements 1990", "Logements 1999", "Logements 2008", "Logements 2013", "Logements 2018")


#(EX) Exportation en csv.
write_csv(densite_logements, paste0("bases/",codepci,"/","Densite_logements.csv"))
``` 


#### 2.2 Evolution du parc de logements

``` r
evop <- data_hist18[, c(1,28:59)]

evol_parc <- merge(x = communes[,-3], y = evop,
                by.x = "CODGEO", by.y = "CODGEO", all.x = T, all.y = F)

colnames(evol_parc) <- c("Code INSEE", "Echelle","Logements 2018","Logements 2013","Logements 2008",
                        "Logements 1999","Logements 1990","Logements 1982","Logements 1975","Logements 1968",
                        "Résid_ppales 2018","Résid_ppales 2013","Résid_ppales 2008","Résid_ppales 1999",
                        "Résid_ppales 1990","Résid_ppales 1982","Résid_ppales 1975","Résid_ppales 1968",
                        "Résid_secondaires 2018","Résid_secondaires 2013","Résid_secondaires 2008","Résid_secondaires 1999",
                        "Résid_secondaires 1990","Résid_secondaires 1982","Résid_secondaires 1975","Résid_secondaires 1968",
                        "Logts_vacants 2018","Logts_vacants 2013","Logts_vacants 2008","Logts_vacants 1999",
                        "Logts_vacants 1990","Logts_vacants 1982","Logts_vacants 1975","Logts_vacants 1968")


# (EX) Exportation en csv.
write_csv(evol_parc, paste0("bases/",codepci,"/","Evolution parc de logements.csv"))
``` 

#### GRAPHIQUE: "Répartition du parc de logement"

Réorganisation des données
``` r
residences2018 <- evol_parc[,c("Echelle","Logements 2018","Résid_ppales 2018","Résid_secondaires 2018","Logts_vacants 2018")]
residences2018 <- mutate(residences2018,
                         "Logements vacantes" = residences2018[,5]/residences2018[,2],
                         "Résidences secondaires" = residences2018[,4]/residences2018[,2],
                         "Résidences principales" = residences2018[,3]/residences2018[,2])

residences2018[,-1] <- round(residences2018[,-1],2)
residences2018 <- residences2018[, -c(2,3,4,5)]
residences2018 <- melt(residences2018, id.vars = "Echelle", variable.name = "Logt")
``` 

Graphique:
``` r
color <- c("#3b2e5a", "#7af09e","#26d07c")

windows()
showtext_auto()
  
logts2018 <- ggplot(residences2018, aes(x= Echelle, y=value, fill=Logt)) +
            geom_col() +
            scale_fill_manual(values = color) +
            scale_y_continuous(labels = percent) +
            labs(y = "Structure du parc de logements (en %)", 
            title= "Repartition du parc de logement 2018") 

logts2018 <- logts2018 + theme_classic() + 
  theme(text=element_text(size=14,  family= "Spectral"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 90), axis.title.x=element_blank(),
        legend.title = element_blank())

logts2018

showtext_auto(FALSE)

ggsave(filename = paste("graphiques/",codepci,"/","Repartition du parc de logement.jpeg", sep = ""), 
       device = "jpeg", plot = logts2018, type = 'cairo',width = 11, height = 7, dpi = 100)

```


#### 2.3 Densité de la population ::::::::::::#

``` r
dpop <- data_hist18[, c("CODGEO","SUPERF","D90_POP","D99_POP","P08_POP","P13_POP","P18_POP")]

densite_pop <- merge(x = communes[,-3], y = dpop,
                     by.x = "CODGEO", by.y = "CODGEO", all.x = T, all.y = F)

densite_pop <- mutate(densite_pop,
                      "Hab/Km2_1990" = D90_POP/SUPERF,
                      "Hab/Km2_1999" = D99_POP/SUPERF,
                      "Hab/Km2_2008" = P08_POP/SUPERF,
                      "Hab/Km2_2013" = P13_POP/SUPERF,
                      "Hab/Km2_2018" = P18_POP/SUPERF)

densite_pop <- mutate(densite_pop,
                      "Variation Hab/Km2 90/99" = (densite_pop[,10] - densite_pop[,9]) / densite_pop[,9],
                      "Variation Hab/Km2 99/08" = (densite_pop[,11] - densite_pop[,10]) / densite_pop[,10],
                      "Variation Hab/Km2 08/13" = (densite_pop[,13] - densite_pop[,11]) / densite_pop[,11],
                      "Variation Hab/Km2 13/18" = (densite_pop[,13] - densite_pop[,13]) / densite_pop[,13],
                      "Variation anuelle moyenne 90/18" = ((densite_pop[,13] / densite_pop[,9])^(1/28))-1,
                      "Variation anuelle moyenne 90/13" = ((densite_pop[,13] / densite_pop[,9])^(1/23))-1,
                      "Variation anuelle moyenne 13/18" = ((densite_pop[,13] / densite_pop[,13])^(1/5))-1)


Total_EPCI <- data.frame("CODGEO" = "-",
                         "LIBGEO" = "Total_EPCI",
                         "SUPERF" = sum(densite_pop$SUPERF),
                         "D90_POP" = sum(densite_pop$D90_POP),
                         "D99_POP" = sum(densite_pop$D99_POP),
                         "P08_POP" = sum(densite_pop$P08_POP),
                         "P13_POP" = sum(densite_pop$P13_POP),
                         "P18_POP" = sum(densite_pop$P18_POP))

Total_EPCI <- mutate(Total_EPCI, 
                     "Hab/Km2_1990" = D90_POP/SUPERF,
                     "Hab/Km2_1999" = D99_POP/SUPERF,
                     "Hab/Km2_2008" = P08_POP/SUPERF,
                     "Hab/Km2_2013" = P13_POP/SUPERF,
                     "Hab/Km2_2018" = P18_POP/SUPERF)

Total_EPCI <- mutate(Total_EPCI,"Variation Hab/Km2 90/99" = (Total_EPCI[,10] - Total_EPCI[,9]) / Total_EPCI[,9],
                     "Variation Hab/Km2 99/08" = (Total_EPCI[,11] - Total_EPCI[,10]) / Total_EPCI[,10],
                     "Variation Hab/Km2 08/13" = (Total_EPCI[,13] - Total_EPCI[,11]) / Total_EPCI[,11],
                     "Variation Hab/Km2 13/18" = (Total_EPCI[,13] - Total_EPCI[,13]) / Total_EPCI[,13],
                     "Variation anuelle moyenne 90/18" = ((Total_EPCI[,13] / Total_EPCI[,9])^(1/28))-1,
                     "Variation anuelle moyenne 90/13" = ((Total_EPCI[,13] / Total_EPCI[,9])^(1/23))-1,
                     "Variation anuelle moyenne 13/18" = ((Total_EPCI[,13] / Total_EPCI[,13])^(1/5))-1)

densite_pop <- rbind(densite_pop,Total_EPCI)

colnames(densite_pop)[c(1,2,3,4,5,6,7,8)] <- c("Code INSEE", "Echelle", "Superficie Km2", "Population_1990", "Population_1999", "Population_2008", "Population_2013", "Population_2018")

#(EX) Exportation en csv.
write_csv(densite_pop, paste0("bases/",codepci,"/","Densité population.csv"))
``` 


# CONTEXTE DEMOGRAPHIQUE 

## Evolution démographique entre 1968 et 2018 

``` r
demoev <- data_hist18[, c(1,5:13)]
evol_demo <- merge(x = communes[,-3], y = demoev, by.x = "CODGEO", by.y = "CODGEO", all.x = T, all.y = F )
``` 

## Taux De Croissance Global Et Taux De Croissance Annuel

``` r
croissance_pop <- evol_demo[,c(1,2)]
croissance_pop <- mutate(croissance_pop, 
                         "taux_glob_6875" = (evol_demo[,9] - evol_demo[,10])/evol_demo[,10] ,
                         "taux_annuelle_6875" = ((evol_demo[,9] / evol_demo[,10])^(1/7))-1 ,
                         
                         "taux_glob_7582" = (evol_demo[,8] - evol_demo[,9])/evol_demo[,9] ,
                         "taux_annuelle_7582" = ((evol_demo[,8] / evol_demo[,9])^(1/7))-1 ,
                         
                         "taux_glob_8290" = (evol_demo[,7] - evol_demo[,8])/evol_demo[,8] ,
                         "taux_annuelle_8290" = ((evol_demo[,7] / evol_demo[,8])^(1/8))-1 ,
                         
                         "taux_glob_9099" = (evol_demo[,6] - evol_demo[,7])/evol_demo[,7] ,
                         "taux_annuelle_9099" = ((evol_demo[,6] / evol_demo[,7])^(1/9))-1 ,
                         
                         "taux_glob_9908" = (evol_demo[,5] - evol_demo[,6])/evol_demo[,6] ,
                         "taux_annuelle_9908" = ((evol_demo[,5] / evol_demo[,6])^(1/9))-1 ,
                         
                         "taux_glob_0813" = (evol_demo[,4] - evol_demo[,5])/evol_demo[,5] ,
                         "taux_annuelle_0813" = ((evol_demo[,4] / evol_demo[,5])^(1/5))-1 ,
                         
                         "taux_glob_1318" = (evol_demo[,3] - evol_demo[,4])/evol_demo[,4] ,
                         "taux_annuelle_1318" = ((evol_demo[,3] / evol_demo[,4])^(1/5))-1 )

# (EX) Exportation
write_csv(croissance_pop, paste0("bases/",codepci,"/","Taux global et annuel.csv"))

``` 

## GRAPHIQUE: Evolution démographiques 1968-2018.

Réorganisation des données

``` r
info_evdemo <- croissance_pop[,c(2,4,6,8,10,13,14,16)]
colnames(info_evdemo) <- c("Communes","1968-75","1975-82","1982-90","1990-99","1999-08","2008-13","2013-18")
  
info_evdemo <- melt(data = info_evdemo, id.vars = "Communes",variable.name= "Période")
``` r
  
Graphique:

``` r
windows()
showtext_auto()

evdemo <- ggplot(info_evdemo,aes(x=Période, y=value, group=Communes, colour= Communes)) +
          geom_line(size = 1) +
          scale_color_viridis(discrete = TRUE, option = "D")+
          scale_fill_viridis(discrete = TRUE)  +
           labs(y = "Taux annuel de croissance", 
           title= "Evolutions démographiques 1968-2018") 

  
evdemo <- evdemo + theme_classic() + 
  theme(text=element_text(size=14,  family= "Spectral"),
        axis.text = element_text(size = 14,face = "bold"))

evdemo

ggsave(filename = paste("graphiques/",codepci,"/","Evol_demographiques_1968_2018.jpeg", sep = ""), 
       device = "jpeg", plot = evdemo, type = 'cairo',width = 11, height = 7, dpi = 100)


showtext_auto(FALSE)
``` 


## Soldes naturelles et migratoires ::::::::::::#

``` r
mig <- data_hist18[, c(1,5:13,14:27)]
mig <- merge(x = communes[,-3], y = mig, by.x = "CODGEO", by.y = "CODGEO", all.x = T, all.y = F )

colnames(mig)[c(1:10)] <- c("Code INSEE", "Echelle","Population_2018", "Population_2013", "Population_2008", "Population_1999",
                                 "Population_1990","Population_1982", "Population_1975", "Population_1968")

soldes_natu_et_mig <- mutate(mig, 
                             "Solde_total_6875"= mig[,9]-mig[,10] ,
                             "Dont solde_naturel_6875" = mig[,18]-mig[,24] ,
                             "Dont solde_migratoire_6875" = (mig[,9]-mig[,10]) - (mig[,18]-mig[,24]) ,
                             
                             "Solde_total_7582"= mig[,8]-mig[,9],
                             "Dont solde_naturel_7582" = mig[,16]-mig[,23] ,
                             "Dont solde_migratoire_7582" = (mig[,8]-mig[,9]) - (mig[,16]-mig[,23]),
                             
                             "Solde_total_8290"= mig[,7]-mig[,8],
                             "Dont solde_naturel_8290" = mig[,15]-mig[,22],
                             "Dont solde_migratoire_8290" = (mig[,7]-mig[,8]) - (mig[,15]-mig[,22]),
                             
                             "Solde_total_9099"= mig[,6]-mig[,7],
                             "Dont solde_naturel_9099" = mig[,14]-mig[,21],
                             "Dont solde_migratoire_9099" = (mig[,6]-mig[,7]) - (mig[,14]-mig[,21]),
                             
                             "Solde_total_9908"= mig[,5]-mig[,6],
                             "Dont solde_naturel_9908" = mig[,13]-mig[,20],
                             "Dont solde_migratoire_9908" = (mig[,5]-mig[,6]) - (mig[,13]-mig[,20]),
                             
                             "Solde_total_0813"= mig[,4]-mig[,5],
                             "Dont solde_naturel_0813" = mig[,13]-mig[,19],
                             "Dont solde_migratoire_0813" = (mig[,4]-mig[,5]) - (mig[,13]-mig[,19]),
                             
                             "Solde_total_1318"= mig[,3]-mig[,4] ,
                             "Dont solde_naturel_1318" = mig[,11]-mig[,18],
                             "Dont solde_migratoire_1318" = (mig[,3]-mig[,4]) - (mig[,11]-mig[,18]))

soldes_natu_et_mig <- soldes_natu_et_mig[, -c(3:24)]

#(EX) Exportation en csv.
write_csv(soldes_natu_et_mig, paste0("bases/",codepci,"/","Caractérisation de l'evolu de la pop entre 1968 et 2018.csv"))

```

## GRAPHIQUE: "Solde_naturel et solde_migratoire."

Réorganisation des données

``` r
sol <- soldes_natu_et_mig[,c(2,22,23)]
colnames(sol) <- c("Echelle","Solde Naturel","Solde Migratoire")
sol <- melt(sol, id.vars = "Echelle")

color <- c("#213F40","#26D07C")

windows()
showtext_auto()

soldes <- ggplot(sol, aes(Echelle, value, fill = variable)) + 
                 geom_col(position = "dodge") +
                 scale_fill_manual(values=color) +
                 labs(y = "Evolution de la population entre 2013-2018") 

soldes <- soldes + theme_classic() + 
  theme(text=element_text(size=14,  family= "Spectral"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 90), axis.title.x=element_blank(),
        legend.title = element_blank())

soldes

showtext_auto(FALSE)

#(EX)
ggsave(filename = paste("graphiques/",codepci,"/","solde_naturel et solde_migratoire.jpeg", sep = ""), 
       device = "jpeg", plot = soldes, type = 'cairo',width = 11, height = 7, dpi = 100)

``` 
