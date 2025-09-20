###########################
# Mémoire de maîtrise     #
# Section - 2.1 Donnnées  #
# Description             #
###########################

# IMPORTATION DES LIBRAIRIES

library(readr)
library(tidyverse)
library(lubridate)
library(cld3)
library(readxl)

# Données de Lavigne et al. (2023) ----------------------------------------

QC22 <- read_csv("QC22.csv")
View(QC22)

# FILTRAGE DONNÉES PENDANT CAMPAGNE ÉLECTORALE ---------------------------------

## Transformation de create_at en date-heure
QC <- QC22 %>%
  mutate(created_at = ymd_hms(created_at))

## Filtrer les observations postérieures au 28 août 2022
QC22_post <- QC %>%
  filter(created_at >= ymd("2022-08-28"))


# Données descriptives ----------------------------------------------------

## BASE DE DONNÉES DE LAVIGNE ET AL.2023

### NOMBRE D'IDENTIFIANTS UNIQUES ET DE RETWEETS

QC22_summary <- QC22_post %>%
  summarise(
    nb_users_uniques = n_distinct(user_id),
    nb_retweets = sum(!is.na(retweet_id))
  )

### LANGUES

QC22_langue <- QC22_post %>%
  mutate(lang = detect_language(text))

QC22_lang_summary <- QC22_langue %>%
  filter(lang %in% c("fr", "en")) %>%
  group_by(lang) %>%
  summarise(nb_tweets = n())


# Données d'entrainement --------------------------------------------------

## Importation des données

data_ajustement <- read_excel("data_ajustement.xlsx")
View(data_ajustement)

## Détection de la langue

data_ajustement_langue <- data_ajustement %>%
  mutate(lang = detect_language(text))

data_ajustement_summary <- data_ajustement_langue %>%
  filter(lang %in% c("fr", "en")) %>%
  group_by(lang) %>%
  summarise(nb_tweets = n())

## Calculer le nombre et le pourcentage pour chaque valeur de label_binary

summary_label <- data_ajustement %>% 
  group_by(label_binary) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100)

summary_label

## Graphique

install.packages("wordcloud2")
install.packages("tidytext")
install.packages("quanteda")
install.packages("SnowballC")
library(wordcloud2)
library(tidytext)
library(quanteda)
library(SnowballC)
library(stopwords)


# Stopwords français et anglais depuis le package stopwords
stopwords_fr <- tibble(word = stopwords("fr", source = "snowball"))
stopwords_en <- tibble(word = stopwords("en", source = "snowball"))

# Hashtags et mots à exclure
hashtags_exclure <- tibble(word = c(
  "polqc", "polqc2022", "QC2022", "quebec", "assnat", "caq", "coalitionavenir",
  "legault", "PLQ", "anglade", "partiliberalduquebec", "plamondon", "partiquebecois",
  "PCQ", "duhaime", "particonservateur", "quebecsolidaire", "manonmasse",
  "nadeaudubois", "librecheznous", "dehorslacaq", "votezvrai", "dehorslegault",
  "votezcaq", "votezplq", "votezpq", "votezpcq", "votezqs", "qc2022", "quebec", "fait",
  "dit", "ça", "c'est", "https", "québec", "si", "veut", "si", "tout", "va", "3", "plus", "qu'il",
  "ans", "rien", "être", "bien", "non", "pcq", "pq", "qs", "très", "tous", "qu'on", "faire", "comme",
  "dire", "aussi", "2022", "après", "2", "faut", "j'ai", "n'est", "c’est", "	
ceux", "parti", "qu'ils", "quand", "plq", "http", "peu", "j’ai", "qu’il"
))

# Combiner tous les mots à exclure
stopwords_all <- bind_rows(stopwords_fr, stopwords_en, hashtags_exclure)

# Transformer la colonne text en minuscules
data_ajustement <- data_ajustement %>%
  mutate(text = str_to_lower(text))

# Word cloud pour les TRUE (40 mots les plus fréquents)
words_true <- data_ajustement %>%
  filter(label_binary == "TRUE") %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords_all, by = "word") %>%  # enlever stopwords et hashtags
  count(word, sort = TRUE) %>%
  slice_max(order_by = n, n = 30)            # sélectionner les 40 mots les plus fréquents

wordcloud2(words_true, size = 0.5)

# Word cloud pour les FALSE (40 mots les plus fréquents)
words_false <- data_ajustement %>%
  filter(label_binary == "FALSE") %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords_all, by = "word") %>%
  count(word, sort = TRUE) %>%
  slice_max(order_by = n, n = 30)

wordcloud2(words_false, size = 0.5)