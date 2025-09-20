################################
##       Master-Thesis        ##
##    Cleaning & Conversion   ##
##      5-fold creation       ##
##      Llama2 & Qwen3        ##
##        2025-09-20          ##
##       Xlsx à JSONL         ##
################################

# (0) Librairies ----------------------------------------------------------
library(readxl)
library(tidyverse)
library(jsonlite)
library(rsample)

# (1) Data ----------------------------------------------------------------
data <- read_excel("data_ajustement.xlsx")

# (2) Data Cleaning -------------------------------------------------------

## Label verification
unique(data$label_binary)

### [1] "TRUE"        "FALSE"       "NON-VERIFIE"

## Modification - Data cleaning - New numeric variable

data <- data %>%
  mutate(
    label_binary = recode(as.character(label_binary), 
                          "TRUE" = "VRAI", 
                          "FALSE" = "FAUX"),
    label_numeric = recode(label_binary,
                           "VRAI" = 100,
                           "FAUX" = 0,
                           "NON-VERIFIE" = 50))

unique(data$label_binary)
unique(data$label_numeric)

### [1] "VRAI"        "FAUX"        "NON-VERIFIE"
### [1] 100              0                50


# (3) Prompts -------------------------------------------------------------

## Messages for the system

system_msgs <- list(
  ### 1st prompt
  prompt1 = "Vous êtes un assistant conçu pour identifier la désinformation diffusée pendant la campagne électorale québécoise de 2022. Lorsqu’un tweet vous est présenté, vous devez évaluer si l’information est vraie, fausse ou non-vérifiée. Répondez par 'VRAI' si le tweet est factuellement exact, 'FAUX' s’il ne l’est pas ou 'NON VÉRIFIÉ' s’il ne contient pas suffisamment d’éléments factuels pour être évalué. Ne donnez pas d’explication, indiquez uniquement le mot.",
  
  ### 2nd prompt
  prompt2 = "Vous êtes un assistant conçu pour identifier la désinformation diffusée pendant la campagne électorale québécoise de 2022. Lorsqu’un tweet vous est présenté, vous devez évaluer si l’information est vraie, fausse ou non-vérifiée. Répondez par 'VRAI' si la majorité des informations contenues dans le tweet est factuellement exacte, 'FAUX' si la majorité des informations contenues dans le tweet ne l’est pas, ou 'NON VÉRIFIÉ' s’il ne contient pas suffisamment d’éléments factuels pour être évalué ou si le tweet fait état d’une question ouverte. Ne donnez pas d’explication, indiquez uniquement le mot.",
  ### 3rd prompt
  prompt3 = "Vous êtes un assistant conçu pour identifier la désinformation diffusée pendant la campagne électorale québécoise de 2022. Lorsqu’un tweet vous est présenté, vous devez évaluer si l’information est vraie, fausse ou non-vérifiée. Fournissez un score de 0 à 100, où 0 représente totalement faux, 50 signifie qu’il n’y a pas suffisamment d’éléments factuels pour évaluer la véracité, et 100 représente totalement vrai. Ne fournissez aucune explication, répondez uniquement avec le score numérique."
)

label_cols <- c(prompt1 = "label_binary", 
                prompt2 = "label_binary", 
                prompt3 = "label_numeric")

# (4) Conversion to .jsonl format ----------------------------------------

create_jsonl <- function(data, 
                         system_msg, 
                         label_col, 
                         output_file) {
  json_list <- data %>%
    mutate(
      system = system_msg,
      user = text,
      label = .data[[label_col]]
    ) %>%
    select(system, user, label) %>%
    pmap(function(system, user, label) {
      toJSON(list(system = system, 
                  user = user, 
                  label = label), 
             auto_unbox = TRUE)
    })
  
  writeLines(unlist(json_list), output_file)
}

# (5) Creation of 5 folds  -----------------------------------------------

set.seed(123)

folds <- vfold_cv(data, 
                  v = 5, 
                  strata = "label_binary")

# (6) Automatic generation of JSONL files  -------------------------------

for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  for (p in names(system_msgs)) {
    # Training set
    create_jsonl(train_data, system_msgs[[p]], label_cols[[p]], 
                 paste0("fold", i, "_", p, "_train.jsonl"))
    
    # Validation set
    create_jsonl(val_data, system_msgs[[p]], label_cols[[p]], 
                 paste0("fold", i, "_", p, "_val.jsonl"))
  }
}

# (7) Verification -------------------------------------------------------

## Number of observations in each fold

for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  cat("Fold", i, ":\n")
  cat("  Training set:", nrow(train_data), "observations\n")
  cat("  Validation set:", nrow(val_data), "observations\n\n")
}

### Fold 1 :
### Training set: 1775 observations
### Validation set: 445 observations

### Fold 2 :
### Training set: 1775 observations
### Validation set: 445 observations

### Fold 3 :
### Training set: 1776 observations
### Validation set: 444 observations

### Fold 4 :
### Training set: 1776 observations
### Validation set: 444 observations

### Fold 5 :
### Training set: 1778 observations
### Validation set: 442 observations

## Number of labels in each fold
for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  cat("Fold", i, ":\n")
  cat("  Training set:", 
      nrow(train_data), 
      "observations\n")
  
  print(prop.table(table(train_data$label_binary)))
  
  cat("  Validation set:", nrow(val_data), "observations\n")
  print(prop.table(table(val_data$label_binary)))
  cat("\n")
}

### Fold 1 :

### Training set: 1775 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2608451   0.2349296   0.5042254 

### Validation set: 445 observations
### FAUX        NON-VERIFIE  VRAI 
### 0.2606742   0.2359551   0.5033708 


### Fold 2 :

### Training set: 1775 observations
### FAUX        NON-VERIFIE   VRAI 
### 0.2608451   0.2349296   0.5042254 

### Validation set: 445 observations
### FAUX        NON-VERIFIE   VRAI 
### 0.2606742   0.2359551   0.5033708 


### Fold 3 :

### Training set: 1776 observations
### FAUX       NON-VERIFIE    VRAI 
### 0.2606982   0.2353604   0.5039414 

### Validation set: 444 observations
### FAUX       NON-VERIFIE    VRAI 
### 0.2612613   0.2342342   0.5045045 


### Fold 4 :

### Training set: 1776 observations
### FAUX       NON-VERIFIE   VRAI 
### 0.2606982   0.2353604   0.5039414 

### Validation set: 444 observations
### FAUX       NON-VERIFIE   VRAI 
### 0.2612613   0.2342342   0.5045045 


### Fold 5 :

### Training set: 1778 observations
### FAUX       NON-VERIFIE   VRAI 
### 0.2609674   0.2350956   0.5039370 

### Validation set: 442 observations
### FAUX        NON-VERIFIE VRAI 
### 0.2601810   0.2352941   0.5045249 



# Descriptive table

label_summary <- function(df, set_name = "train") {
  df %>%
    group_by(label_binary) %>%
    summarise(
      n = n(),
      pct = round(100 * n() / nrow(df), 1)
    ) %>%
    mutate(set = set_name)
}

## Table
summary_table <- map_dfr(seq_along(folds$splits), function(i) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  train_summary <- label_summary(train_data, "train") %>% 
    mutate(fold = i)
  val_summary   <- label_summary(val_data, "val") %>% 
    mutate(fold = i)
  
  bind_rows(train_summary, val_summary)
})

## print()
options(tibble.print_max = Inf, 
        tibble.width = Inf)

summary_table %>%
  select(fold, set, label_binary, n, pct) %>%
  print(n = Inf)

### A tibble: 30 × 5
###     fold set   label_binary     n   pct
###     <int><chr> <chr>        <int> <dbl>
### 1      1 train FAUX           463  26.1
### 2      1 train NON-VERIFIE    417  23.5
### 3      1 train VRAI           895  50.4
### 4      1 val   FAUX           116  26.1
### 5      1 val   NON-VERIFIE    105  23.6
### 6      1 val   VRAI           224  50.3
### 7      2 train FAUX           463  26.1
### 8      2 train NON-VERIFIE    417  23.5
### 9      2 train VRAI           895  50.4
### 10     2 val   FAUX           116  26.1
### 11     2 val   NON-VERIFIE    105  23.6
### 12     2 val   VRAI           224  50.3
### 13     3 train FAUX           463  26.1
### 14     3 train NON-VERIFIE    418  23.5
### 15     3 train VRAI           895  50.4
### 16     3 val   FAUX           116  26.1
### 17     3 val   NON-VERIFIE    104  23.4
### 18     3 val   VRAI           224  50.5
### 19     4 train FAUX           463  26.1
### 20     4 train NON-VERIFIE    418  23.5
### 21     4 train VRAI           895  50.4
### 22     4 val   FAUX           116  26.1
### 23     4 val   NON-VERIFIE    104  23.4
### 24     4 val   VRAI           224  50.5
### 25     5 train FAUX           464  26.1
### 26     5 train NON-VERIFIE    418  23.5
### 27     5 train VRAI           896  50.4
### 28     5 val   FAUX           115  26  
### 29     5 val   NON-VERIFIE    104  23.5
### 30     5 val   VRAI           223  50.5







# -----------------------------------------------------------------------------
###############################################################################
#                      ADDING DATE IN PROMPT AND DATA                         #
###############################################################################

# (3) Prompts -------------------------------------------------------------

## Messages for the system

system_msgs <- list(
  ### 1st prompt
  prompt1 = "Vous êtes un assistant conçu pour identifier la désinformation diffusée pendant la campagne électorale québécoise de 2022. Lorsqu’un tweet vous est présenté, vous devez évaluer si l’information est vraie, fausse ou non-vérifiée, et ce, en fonction de la date. Répondez par 'VRAI' si le tweet est factuellement exact, 'FAUX' s’il ne l’est pas ou 'NON VÉRIFIÉ' s’il ne contient pas suffisamment d’éléments factuels pour être évalué. Ne donnez pas d’explication, indiquez uniquement le mot.",
  
  ### 2nd prompt
  prompt2 = "Vous êtes un assistant conçu pour identifier la désinformation diffusée pendant la campagne électorale québécoise de 2022. Lorsqu’un tweet vous est présenté, vous devez évaluer si l’information est vraie, fausse ou non-vérifiée, et ce, en fonction de la date. Répondez par 'VRAI' si la majorité des informations contenues dans le tweet est factuellement exacte, 'FAUX' si la majorité des informations contenues dans le tweet ne l’est pas, ou 'NON VÉRIFIÉ' s’il ne contient pas suffisamment d’éléments factuels pour être évalué ou si le tweet fait état d’une question ouverte. Ne donnez pas d’explication, indiquez uniquement le mot.",
  ### 3rd prompt
  prompt3 = "Vous êtes un assistant conçu pour identifier la désinformation diffusée pendant la campagne électorale québécoise de 2022. Lorsqu’un tweet vous est présenté, vous devez évaluer si l’information est vraie, fausse ou non-vérifiée, et ce, en fonction de la date. Fournissez un score de 0 à 100, où 0 représente totalement faux, 50 signifie qu’il n’y a pas suffisamment d’éléments factuels pour évaluer la véracité, et 100 représente totalement vrai. Ne fournissez aucune explication, répondez uniquement avec le score numérique."
)

label_cols <- c(prompt1 = "label_binary", 
                prompt2 = "label_binary", 
                prompt3 = "label_numeric")

# (4) Conversion to .jsonl format ----------------------------------------

create_jsonl <- function(data, system_msg, label_col, output_file) {
  json_list <- data %>%
    mutate(
      system = system_msg,
      date = created_at,
      user = text,
      label = .data[[label_col]]
    ) %>%
    select(system, user, date, label) %>%
    pmap(function(system, user, date, label) {
      toJSON(list(system = system,
                  date = date,
                  user = user,
                  label = label), 
             auto_unbox = TRUE)
    })
  
  writeLines(unlist(json_list), output_file)
}

# (5) Creation of 5 folds  -----------------------------------------------

set.seed(123)

folds <- vfold_cv(data, 
                  v = 5, 
                  strata = "label_binary")

# (6) Automatic generation of JSONL files  -------------------------------

for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  for (p in names(system_msgs)) {
    # Training set
    create_jsonl(train_data, system_msgs[[p]], label_cols[[p]], 
                 paste0("fold", i, "_", p, "_train.jsonl"))
    
    # Validation set
    create_jsonl(val_data, system_msgs[[p]], label_cols[[p]], 
                 paste0("fold", i, "_", p, "_val.jsonl"))
  }
}

# (7) Verification -------------------------------------------------------

## Number of observations in each fold

for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  cat("Fold", i, ":\n")
  cat("  Training set:", nrow(train_data), "observations\n")
  cat("  Validation set:", nrow(val_data), "observations\n\n")
}

### Fold 1 :
### Training set: 1775 observations
### Validation set: 445 observations

### Fold 2 :
### Training set: 1775 observations
### Validation set: 445 observations

### Fold 3 :
### Training set: 1776 observations
### Validation set: 444 observations

### Fold 4 :
### Training set: 1776 observations
### Validation set: 444 observations

### Fold 5 :
### Training set: 1778 observations
### Validation set: 442 observations


## Number of labels in each fold
for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  cat("Fold", i, ":\n")
  cat("  Training set:", 
      nrow(train_data), 
      "observations\n")
  
  print(prop.table(table(train_data$label_binary)))
  
  cat("  Validation set:", nrow(val_data), "observations\n")
  print(prop.table(table(val_data$label_binary)))
  cat("\n")
}

### Fold 1 :

### Training set: 1775 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2608451   0.2349296   0.5042254 

### Validation set: 445 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2606742   0.2359551   0.5033708 


### Fold 2 :
### Training set: 1775 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2608451   0.2349296   0.5042254 

### Validation set: 445 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2606742   0.2359551   0.5033708 


### Fold 3 :
### Training set: 1776 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2606982   0.2353604   0.5039414 

### Validation set: 444 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2612613   0.2342342   0.5045045 


### Fold 4 :
### Training set: 1776 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2606982   0.2353604   0.5039414 

### Validation set: 444 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2612613   0.2342342   0.5045045 


### Fold 5 :
### Training set: 1778 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2609674   0.2350956   0.5039370 

### Validation set: 442 observations
### FAUX NON-VERIFIE        VRAI 
### 0.2601810   0.2352941   0.5045249 


# Descriptive table

label_summary <- function(df, set_name = "train") {
  df %>%
    group_by(label_binary) %>%
    summarise(
      n = n(),
      pct = round(100 * n() / nrow(df), 1)
    ) %>%
    mutate(set = set_name)
}

## Table
summary_table <- map_dfr(seq_along(folds$splits), function(i) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  train_summary <- label_summary(train_data, "train") %>% 
    mutate(fold = i)
  val_summary   <- label_summary(val_data, "val") %>% 
    mutate(fold = i)
  
  bind_rows(train_summary, val_summary)
})

## print()
options(tibble.print_max = Inf, 
        tibble.width = Inf)

summary_table %>%
  select(fold, set, label_binary, n, pct) %>%
  print(n = Inf)

# A tibble: 30 × 5
###     fold set   label_binary     n   pct
###    <int> <chr> <chr>        <int> <dbl>
### 1      1 train FAUX           463  26.1
### 2      1 train NON-VERIFIE    417  23.5
### 3      1 train VRAI           895  50.4
### 4      1 val   FAUX           116  26.1
### 5      1 val   NON-VERIFIE    105  23.6
### 6      1 val   VRAI           224  50.3
### 7      2 train FAUX           463  26.1
### 8      2 train NON-VERIFIE    417  23.5
### 9      2 train VRAI           895  50.4
### 10     2 val   FAUX           116  26.1
### 11     2 val   NON-VERIFIE    105  23.6
### 12     2 val   VRAI           224  50.3
### 13     3 train FAUX           463  26.1
### 14     3 train NON-VERIFIE    418  23.5
### 15     3 train VRAI           895  50.4
### 16     3 val   FAUX           116  26.1
### 17     3 val   NON-VERIFIE    104  23.4
### 18     3 val   VRAI           224  50.5
### 19     4 train FAUX           463  26.1
### 20     4 train NON-VERIFIE    418  23.5
### 21     4 train VRAI           895  50.4
### 22     4 val   FAUX           116  26.1
### 23     4 val   NON-VERIFIE    104  23.4
### 24     4 val   VRAI           224  50.5
### 25     5 train FAUX           464  26.1
### 26     5 train NON-VERIFIE    418  23.5
### 27     5 train VRAI           896  50.4
### 28     5 val   FAUX           115  26  
### 29     5 val   NON-VERIFIE    104  23.5
### 30     5 val   VRAI           223  50.5