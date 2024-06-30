## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(metrica)
library(dplyr)
library(purrr)
library(tidyr)

## ----load binary data---------------------------------------------------------
# Load
binary_landCover <- metrica::land_cover

# Printing first observations
head(binary_landCover)

## ----load multiclass data-----------------------------------------------------
# Load
multi_maize_phen <- metrica::maize_phenology

# Printing first observations
head(multi_maize_phen)

## ----confusion_matrix binary, fig.width=6, fig.height=5, dpi=60---------------
# a. Print
binary_landCover %>% confusion_matrix(obs = actual, pred = predicted, 
                                      plot = FALSE,
                                      unit = "count")

# b. Plot
binary_landCover %>% confusion_matrix(obs = actual, pred = predicted, 
                                      plot = TRUE,
                                      colors = c(low="#ffe8d6" , high="#892b64"), 
                                      unit = "count")

# c. Unit = proportion
binary_landCover %>% confusion_matrix(obs = actual, pred = predicted, 
                                      plot = TRUE,
                                      colors = c(low="#f9dbbd" , high="#892b64"), 
                                      unit = "proportion")


## ----confusion_matrix multiclass, fig.width=6, fig.height=5, dpi=60-----------
# a. Print
multi_maize_phen %>% confusion_matrix(obs = actual, pred = predicted, 
                                      plot = FALSE, 
                                      unit = "count")

# b. Plot
multi_maize_phen %>% confusion_matrix(obs = actual, pred = predicted, 
                                      plot = TRUE, 
                                      colors = c(low="grey85" , high="steelblue"), 
                                      unit = "count")


## ----accuracy-----------------------------------------------------------------
# Binary
binary_landCover %>% accuracy(data = ., obs = actual, pred = predicted, tidy = TRUE)

# Multiclass
maize_phenology %>% accuracy(data = ., obs = actual, pred = predicted, tidy = TRUE)

## ----balanced_accuracy--------------------------------------------------------
# Binary
binary_landCover %>% balacc(data = ., obs = actual, pred = predicted, tidy = TRUE)

# Multiclass
maize_phenology %>% balacc(data = ., obs = actual, pred = predicted, tidy = TRUE)

## ----precision----------------------------------------------------------------
# Binary
binary_landCover %>% precision(data = ., obs = actual, pred = predicted, tidy = TRUE)

# Multiclass
maize_phenology %>% precision(data = ., obs = actual, pred = predicted, tidy = TRUE)

## ----metrics_summary----------------------------------------------------------

# Get all at once with metrics_summary()
# Binary
binary_landCover %>% metrics_summary(data = ., obs = actual, pred = predicted, type = "classification")

# Multiclass
multi_maize_phen %>% metrics_summary(data = ., obs = actual, pred = predicted, type = "classification")

## ----metrics_summary_selected-------------------------------------------------
# Get a selected list at once with metrics_summary()
selected_class_metrics <- c("accuracy", "precision", "recall", "fscore")

# Binary
bin_sum <- binary_landCover %>% 
  metrics_summary(data = ., 
                  obs = actual, pred = predicted, 
                  type = "classification",
                  metrics_list = selected_class_metrics, pos_level = 1) 

# Multiclass
multi_maize_phen %>% 
  metrics_summary(data = ., 
                  obs = actual, pred = predicted, 
                  type = "classification",
                  metrics_list = selected_class_metrics)


## ----atom argument------------------------------------------------------------
# Precision
maize_phenology %>% metrica::precision(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# Recall
maize_phenology %>% metrica::recall(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# Specificity
maize_phenology %>% metrica::specificity(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)

# atom = TRUE available for more functions available (remove #)
# F-score
# maize_phenology %>% metrica::fscore(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Adjusted F-score
# maize_phenology %>% metrica::agf(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # G-mean
# maize_phenology %>% metrica::gmean(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Negative predictive value
# maize_phenology %>% metrica::npv(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Prevalence
# maize_phenology %>% metrica::preval(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Prevalence threshold
# maize_phenology %>% metrica::preval_t(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # False omission rate
# maize_phenology %>% metrica::FOR(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # False detection rate
# maize_phenology %>% metrica::FDR(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # False positive rate
# maize_phenology %>% metrica::FPR(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Falase negative rate
# maize_phenology %>% metrica::FNR(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Delta-p
# maize_phenology %>% metrica::deltap(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Critical Success Index
# maize_phenology %>% metrica::csi(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Bookmaker Informedness
# maize_phenology %>% metrica::bmi(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Positive likelihood ratio
# maize_phenology %>% metrica::posLr(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Negative likelihood ratio
# maize_phenology %>% metrica::negLr(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)
# # Diagnostic odds ratio
# maize_phenology %>% metrica::dor(obs = actual, pred = predicted, atom = TRUE, tidy = TRUE)


## ----multiple_models nested---------------------------------------------------

set.seed(15)
# Let's simulated two extra runs of the same model for Land Cover
fold_2 <- data.frame(actual = sample(c(0,1), 285, replace = TRUE),
                     predicted = sample(c(0,1), 285, replace = TRUE))
fold_3 <- data.frame(actual = sample(c(0,1), 285, replace = TRUE),
                     predicted = sample(c(0,1), 285, replace = TRUE))

# a. Create nested df with the folds
binary_nested_folds <- bind_rows(list(fold_1 = binary_landCover, 
                                      fold_2 = fold_2,
                                      fold_3 = fold_3),
                             .id = "id") %>%
  dplyr::group_by(id) %>% tidyr::nest()

head(binary_nested_folds %>% group_by(id) %>% dplyr::slice_head(n=2))

# b. Run 
binary_folds_summary <- binary_nested_folds %>% 
  # Store metrics in new.column "performance"
  dplyr::mutate(performance = 
                  purrr::map(data,
                             ~metrica::metrics_summary(data = ., 
                                                       obs = actual, pred = predicted, 
                                                       type = "classification"))) %>% 
  dplyr::select(-data) %>% 
  tidyr::unnest(cols = performance) %>% 
  dplyr::arrange(Metric)

head(binary_folds_summary)


## ----multiple_models unnested group_map---------------------------------------
non_nested_folds <- binary_nested_folds %>% unnest(cols = "data") 

# Using group_map()
binary_folds_summary_2 <- non_nested_folds %>% 
  dplyr::group_by(id) %>% 
  dplyr::group_map(~metrics_summary(data = ., obs = actual, pred = predicted, type = "classification"))

binary_folds_summary_2

## ----multiple_models unnested summarise---------------------------------------

# Using summarise()
binary_folds_summary_3 <- non_nested_folds %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(metrics_summary(obs = actual, pred = predicted, type = "classification")) %>%
  dplyr::arrange(Metric)


binary_folds_summary_3


## ----scatter_plot print_metrics, fig.width=6, fig.height=5, dpi=60------------

selected_metrics <- c("accuracy", "precision", "recall", "khat",
                   "mcc", "fscore", "agf", "npv", "FPR", "FNR")

binary_matrix_metrics <- 
  binary_landCover %>% 
  confusion_matrix(obs = actual, pred = predicted, 
                   plot = TRUE,
                   colors = c(low="#ffe8d6" , high="#892b64"), 
                   unit = "count",  
                   # Print metrics_summary
                   print_metrics = TRUE,
                   # List of performance metrics
                   metrics_list = selected_metrics,
                   # Position (bottom or top)
                   position_metrics = "bottom")

binary_matrix_metrics

multinomial_matrix_metrics <-
  maize_phenology %>% 
  confusion_matrix(obs = actual, pred = predicted, 
                   plot = TRUE, 
                   colors = c(low="grey85" , high="steelblue"), 
                   unit = "count",
                   # Print metrics_summary
                   print_metrics = TRUE,
                   # List of performance metrics
                   metrics_list = selected_metrics,
                   # Position (bottom or top)
                   position_metrics = "bottom")
  
multinomial_matrix_metrics


## ----scatter_plot.edit, fig.width=6, fig.height=5, dpi=60---------------------

binary_matrix_metrics +
  # Modify labels
  ggplot2::labs(x = "Observed Vegetation", 
       y = "Predicted Vegetation",
       title = "Binary Confusion Matrix")
  

multinomial_matrix_metrics +
  # Modify labels
  ggplot2::labs(x = "Observed Corn Phenology", 
       y = "Predicted Corn Phenology",
       title = "Multinomial Confusion Matrix")+
  # Modify theme
  ggplot2::theme_light()



## ----export metrics_summary, eval=F-------------------------------------------
#  metrics_summary(data = binary_landCover,
#                  obs = obs,
#                  pred = pred,
#                  type = "classification") %>%
#    write.csv("binary_landcover_metrics_summary.csv")
#  

## ----export plot, eval=F------------------------------------------------------
#  
#  ggsave(plot = multinomial_matrix_metrics,
#         "multinomial_matrix_metrics.png",
#         width = 8,
#         height = 7)

