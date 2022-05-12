## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, message=F, warning=F------------------------------------------
library(ggplot2)
library(dplyr)
library(metrica)

## ----load data----------------------------------------------------------------
# Load
data(wheat)

# Printing first observations
head(wheat)

## ----scatter_plot PO----------------------------------------------------------
scatter_plot(data = wheat, 
             obs = obs, 
             pred = pred)

## ----scatter_plot OP----------------------------------------------------------
scatter_plot(data = wheat, 
             obs = obs, 
             pred = pred,
             orientation = "OP")

## ----scatter_plot custom------------------------------------------------------
scatter_plot(data = wheat, 
             obs = obs, 
             pred = pred,
             orientation = "OP")+
  labs(x ="Predicted wheat N content (g N/m2)",
       y = "Observed wheat N content (g N/m2)")+
  theme_dark()

## ----bland-altman-------------------------------------------------------------
bland_altman_plot(data = wheat,
                  obs = obs, 
                  pred = pred)

## ----r2-----------------------------------------------------------------------
R2(data = wheat,
   obs = obs, 
   pred = pred)

## ----rmse---------------------------------------------------------------------
RMSE(data = wheat,
   obs = obs, 
   pred = pred)

## ----metrics summary----------------------------------------------------------
metrics_summary(data = wheat,
   obs = obs, 
   pred = pred)

## ----metrics summary list-----------------------------------------------------

my.metrics <- c("R2","MAE", "RMSE", "RSR", "NSE", "KGE")

metrics_summary(data = wheat,   
                obs = obs,    
                pred = pred,
                metrics_list = my.metrics) 


## ----scatter_plot-------------------------------------------------------------
scatter_plot(data = wheat,
             obs = obs, 
             pred = pred)


## ----scatter_plot print_metrics-----------------------------------------------

my.metrica.plot <- scatter_plot(data = wheat,
             obs = obs, 
             pred = pred,
             print_metrics = TRUE, metrics_list = my.metrics)

my.metrica.plot


## ----scatter_plot.edit--------------------------------------------------------

my.metrica.plot +
  # Modify labels
  labs(x = "Observed (days to emergence)", y = "Predicted (days to emergence)")+
  # Modify theme
  theme_light()

my.metrica.plot +
  # Modify labels
  labs(x = "Observed (Mg/ha)", y = "Predicted (Mg/ha)")+
  # Modify theme
  theme_dark()

## ----export metrics_summary, eval=F-------------------------------------------
#  metrics_summary(data = wheat,
#     obs = obs,
#     pred = pred) %>%
#    write.csv("metrics_summary.csv")
#  

## ----export plot, eval=F------------------------------------------------------
#  
#  ggsave(plot = my.metrica.plot,
#         "scatter_metrics.png",
#         width = 5,
#         height = 5)

