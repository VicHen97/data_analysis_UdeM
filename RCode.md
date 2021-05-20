# data_analysis_UdeM
Analysis of the survey about students interest on sustainable development related lectures

########################################################################
######## -------> !! BEFORE ANYTHING ELSE !! <---------##################
############# PLEASE, SET THE RIGHT DIRECTORY #########################
#############   RUN THE FOLLOWING LIBRARIES   #########################
#############      AND LOAD THE DATASETS      #########################
###########################################################################
### Charging the right directory and libraries #########################
setwd("")
library(readxl)
library(ggplot2)
library(plotly)
library(ggthemes)
library(dplyr)
library(tidyr)

# and loading the datasets of the survey to make sure everything works
sond_EDD = read_xlsx("last_Base de données_Dev durable_anonymisée_UdeM_nopassword.xlsx")
sond_initial = read_xlsx("Base de donnéesR_Dev durable_anonymisée_UdeM_nopassword.xlsx")

### ("essai_Base de données_Dev durable_anonymisée_UdeM.csv")
dim(sond_EDD)
#1649 57

str(sond_EDD)

### QUI EST INTERESSES PAR DU DD ##############

b = sond_EDD %>%
  ggplot(aes(x=Secteur,y=Q9)) +
  geom_violin(scale = "area", aes(fill = Secteur)) +
  theme(legend.position = "none") +
  scale_x_discrete(limits = c("Arts, lettres et sciences humaines",
                              "Droit",
                              "Éducation",
                              "Santé",
                              "Sciences pures et appliquées",
                              "Sciences sociales")) +
  ylab("Taux d'accord") +
  xlab("")

ggplotly(b)  %>%
  layout(title = list(text = paste0("Taux d'accord des étudiants avec la question suivante",
                                    "<br>",
                                    "<sup>",
                                    "Je suis intéressé par des enseignements en développement durable",
                                    "</sup>")))

### TENDANCES GENERALES & TOP 3 DES THEMES PAR SECTEUR #########
# Tendances générales

Intérêt_thèmes_general = tibble(Thèmes = c("Conservation,
biodiversité, protection",
                                       "Gouvernance, politiques
droit environnemental",
                                       "Justice sociale,
pauvreté et développement",
                                       "Luttes chang. Clim.
et énergie",
                                       "Pollution et santé
environnementale",
                                       "Production et
consommation durable",
                                       "Responsabilité
sociale des entrep.",
                                       "Systèmes alimentaires
durables",
                                       "Villes et communautés
durables"),
                            Taux_dinteret = c(56,40,58,60,60,52,45,58,46))


Secteur_general_intérêt = Intérêt_thèmes_general %>% ggplot(aes(x=Thèmes,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Thèmes, x = reorder(Thèmes, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Thèmes d'intérêt en DD pour les étudiants de l'UdeM") +
  ylab("Taux d'intérêt") +
  xlab("")


ggplotly(Secteur_general_intérêt)

# ART
Intérêt_thèmes_art = tibble(Thèmes = c("Biodiversité",
                                      "Gouvernance",
                                      "Justice sociale",
                                      "Energie",
                                      "Pollution",
                                      "Economie circulaire",
                                      "Resp. Sociale. Entrep.",
                                      "Alimentation durable",
                                      "Villes durables"),
                                Taux_dinteret = c(52,39,59,56,52,51,46,57,47))


Secteur_art_intérêt = Intérêt_thèmes_art %>% ggplot(aes(x=Thèmes,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Thèmes, x = reorder(Thèmes, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Thèmes d'intérêt en EDD pour les étudiants en Art, lettres et sciences humaines") +
  ylab("Taux d'intérêt")


ggplotly(Secteur_art_intérêt)

# DROIT

Intérêt_thèmes_droit = tibble(Thèmes = c("Biodiversité",
                                       "Gouvernance",
                                       "Justice sociale",
                                       "Energie",
                                       "Pollution",
                                       "Economie circulaire",
                                       "Resp. Sociale. Entrep.",
                                       "Alimentation durable",
                                       "Villes durables"),
                            Taux_dinteret = c(51,73,66,55,62,50,59,55,39))


Secteur_droit_intérêt = Intérêt_thèmes_droit %>% ggplot(aes(x=Thèmes,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Thèmes, x = reorder(Thèmes, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Thèmes d'intérêt en EDD pour les étudiants en Droit") +
  ylab("Taux d'intérêt")


ggplotly(Secteur_droit_intérêt)

# EDUCATION

Intérêt_thèmes_educ = tibble(Thèmes = c("Biodiversité",
                                         "Gouvernance",
                                         "Justice sociale",
                                         "Energie",
                                         "Pollution",
                                         "Economie circulaire",
                                         "Resp. Sociale. Entrep.",
                                         "Alimentation durable",
                                         "Villes durables"),
                              Taux_dinteret = c(46,27,58,54,56,43,38,55,40))


Secteur_educ_intérêt = Intérêt_thèmes_educ %>% ggplot(aes(x=Thèmes,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Thèmes, x = reorder(Thèmes, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Thèmes d'intérêt en EDD pour les étudiants en Education") +
  ylab("Taux d'intérêt")


ggplotly(Secteur_educ_intérêt)

# SANTE

Intérêt_thèmes_sante = tibble(Thèmes = c("Biodiversité",
                                        "Gouvernance",
                                        "Justice sociale",
                                        "Energie",
                                        "Pollution",
                                        "Economie circulaire",
                                        "Resp. Sociale. Entrep.",
                                        "Alimentation durable",
                                        "Villes durables"),
                             Taux_dinteret = c(51,29,52,58,64,50,37,61,42))


Secteur_sante_intérêt = Intérêt_thèmes_sante %>% ggplot(aes(x=Thèmes,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Thèmes, x = reorder(Thèmes, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Thèmes d'intérêt en EDD pour les étudiants en Santé") +
  ylab("Taux d'intérêt")


ggplotly(Secteur_sante_intérêt)

# SCIENCES PURES

Intérêt_thèmes_pure = tibble(Thèmes = c("Biodiversité",
                                         "Gouvernance",
                                         "Justice sociale",
                                         "Energie",
                                         "Pollution",
                                         "Economie circulaire",
                                         "Resp. Sociale. Entrep.",
                                         "Alimentation durable",
                                         "Villes durables"),
                              Taux_dinteret = c(72,42,44,70,65,61,45,58,56))


Secteur_pure_intérêt = Intérêt_thèmes_pure %>% ggplot(aes(x=Thèmes,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Thèmes, x = reorder(Thèmes, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Thèmes d'intérêt en EDD pour les étudiants en Sciences pures et appliquées") +
  ylab("Taux d'intérêt")


ggplotly(Secteur_pure_intérêt)


# SCIENCES SOCIALES

Intérêt_thèmes_social = tibble(Thèmes = c("Biodiversité",
                                        "Gouvernance",
                                        "Justice sociale",
                                        "Energie",
                                        "Pollution",
                                        "Economie circulaire",
                                        "Resp. Sociale. Entrep.",
                                        "Alimentation durable",
                                        "Villes durables"),
                             Taux_dinteret = c(55,44,66,60,57,54,48,56,44))


Secteur_social_intérêt = Intérêt_thèmes_social %>% ggplot(aes(x=Thèmes,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Thèmes, x = reorder(Thèmes, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Thèmes d'intérêt en EDD pour les étudiants en Sciences sociales") +
  ylab("Taux d'intérêt")


ggplotly(Secteur_social_intérêt)

### COMMENT LES ETUDIANTS VEULENT-ILS ËTRE ENSEIGNE AU DD ? ####
# Tendances générales modalités d'enseignement


Modalite_cours_general = tibble(Modalite = c("MOOC",
                                           "Séminaires, ateliers,
conférences",
                                           "Cours d'introduction",
                                           "Cours intégrateur",
                                           "Cours 1 crédit",
                                           "Stage crédité",
                                           "Cheminement en DD
dans programme",
                                           "Programme
interétablissement"),
                                Taux_dinteret = c(48,72,78,75,57,61,65,56))


Modalite_general_intérêt = Modalite_cours_general %>% ggplot(aes(x=Modalite,y=Taux_dinteret)) +
  geom_bar(stat = "identity",
           aes(fill = Modalite, x = reorder(Modalite, -Taux_dinteret)),
           width = 0.5) +
  theme(legend.position='none') +
  labs(title = "Modalite d'enseignement du DD qui intéresse les étudiants en général") +
  ylab("Taux d'intérêt") +
  xlab("")


ggplotly(Modalite_general_intérêt)

### COMMENT LES ETUDIANTS VEULENT-ILS DU DD ? ####

question_comment = sond_EDD %>% select(Q31,Q32,Q33,Q34,Q35)

question_comment = question_comment %>% gather(Question, Val)

etudiant_udem_enseignement = question_comment %>%
  ggplot(aes(x= Question,
             y=Val)) +
  geom_violin(scale = "area", aes(fill = Question)) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylab("Valeur") +
  xlab("Du haut vers le bas") +
  scale_x_discrete(labels = c("Dans le programme
Hors programme",
                              "Pratique
Théorique",
                              "Spécifique au programme
Général pour tous",
                              "Cours dédiés
Au sein des cours existants",
                              "Optionnel
Obligatoire"))

ggplotly(etudiant_udem_enseignement)  %>%
  layout(title = list(text = paste0("Les étudiants à l'Université de Montréal",
                                    "<br>",
                                    "<sup>",
                                    "Comment voient-ils l'enseignement du Dév.Dur.",
                                    "</sup>")))

### QUAND LES ETUDIANTS VEULENT-ILS DES COURS DE DD ###############

question_quand = sond_EDD %>% select(Q30)

question_quand = question_quand %>% gather(Question, Val)

etudiant_udem_quand = question_quand %>%
  ggplot(aes(x= Question,
             y=Val)) +
  geom_violin(fill = "#2E75B6", colour = "#2E75B6") +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylab("Valeur") +
  xlab("Du haut vers le bas") +
  scale_x_discrete(labels = c("Début du parcours
Tout au long
A la fin"))

ggplotly(etudiant_udem_quand)  %>%
  layout(title = list(text = paste0("Les étudiants à l'Université de Montréal",
                                    "<br>",
                                    "<sup>",
                                    "Quand veulent-ils de l'enseignement en Développement Durable",
                                    "</sup>")))

