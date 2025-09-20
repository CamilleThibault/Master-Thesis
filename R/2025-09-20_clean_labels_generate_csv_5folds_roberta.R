################################
##       Master-Thesis        ##
##    Cleaning & Conversion   ##
##      5-fold creation       ##
##       RoBERTa-Large        ##
##        2025-09-20          ##
##       Xlsx à JSONL         ##
################################

# (1) Librairies ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(rsample)

# (2) Data ----------------------------------------------------------------

data_ajustement <- read_excel("data_ajustement.xlsx")

# (3) Data cleaning -------------------------------------------------------


## Label verification
unique(data$label_binary)

### [1] "TRUE"        "FALSE"       "NON-VERIFIE"

## Modification - Data cleaning - New numeric variable

### 0 -> VRAI
### 1 -> Faux
### 2 -> Non-vérifié

data <- data %>%
  mutate(
    label_binary = recode(as.character(label_binary), 
                          "TRUE" = "VRAI", 
                          "FALSE" = "FAUX"),
    label_numeric = recode(label_binary,
                           "VRAI" = 0,
                           "FAUX" = 1,
                           "NON-VERIFIE" = 2))

unique(data$label_binary)
unique(data$label_numeric)


# (4) 5-folds creation ----------------------------------------------------

set.seed(123)
folds <- vfold_cv(data, 
                  v = 5, 
                  strata = "label_numeric")


# (5) Verification n & % of labels in each fold ---------------------------

for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]])
  val_data   <- testing(folds$splits[[i]])
  
  cat("Fold", i, ":\n")
  cat("  Training set:", 
      nrow(train_data), 
      "\n")
  
  print(prop.table(table(train_data$label_numeric)))
  
  cat("  Validation set:", 
      nrow(val_data), 
      "\n")
  print(prop.table(table(val_data$label_numeric)))
  cat("\n")
}

### Fold 1 :

### Training set: 1775 
### 0         1         2 
### 0.5042254 0.2608451 0.2349296 

### Validation set: 445 
### 0         1         2 
### 0.5033708 0.2606742 0.2359551 


### Fold 2 :

### Training set: 1775 
### 0         1         2 
### 0.5042254 0.2608451 0.2349296 

### Validation set: 445 
### 0         1         2 
### 0.5033708 0.2606742 0.2359551 


### Fold 3 :

### Training set: 1776 
### 0         1         2 
### 0.5039414 0.2606982 0.2353604 

### Validation set: 444 
### 0         1         2 
### 0.5045045 0.2612613 0.2342342 


### Fold 4 :

### Training set: 1776 
### 0         1         2 
### 0.5039414 0.2606982 0.2353604 

### Validation set: 444 
### 0         1         2 
### 0.5045045 0.2612613 0.2342342 


### Fold 5 :

###  Training set: 1778 
### 0         1         2 
### 0.5039370 0.2609674 0.2350956 

### Validation set: 442 
### 0         1         2 
### 0.5045249 0.2601810 0.2352941 


# (6) Exporting CSV files -------------------------------------------------

for (i in seq_along(folds$splits)) {
  train_data <- training(folds$splits[[i]]) %>% 
    select(created_at, text, label_numeric)
  
  val_data   <- testing(folds$splits[[i]]) %>% 
    select(created_at, text, label_numeric)
  
  write.csv(train_data, paste0("roberta_fold", i, "_train.csv"), row.names = FALSE)
  write.csv(val_data, paste0("roberta_fold", i, "_val.csv"), row.names = FALSE)
}
