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

## ----scatter_plot PO, fig.width=5, fig.height=4, dpi=90, warning=FALSE, message=FALSE----
scatter_plot(data = wheat, 
             obs = obs, 
             pred = pred)

## ----scatter_plot OP, fig.width=5, fig.height=4, dpi=90, warning=FALSE, message=FALSE----
scatter_plot(data = wheat, 
             obs = obs, 
             pred = pred,
             orientation = "OP")

## ----scatter_plot custom, fig.width=5, fig.height=4, dpi=90, warning=FALSE, message=FALSE----
scatter_plot(data = wheat, 
             obs = obs, 
             pred = pred,
             orientation = "OP",
             regline_color = "#d0f4de",
             shape_color = "#80ed99",
             eq_color = "white",
             )+
  labs(x ="Predicted wheat N content (g N/m2)",
       y = "Observed wheat N content (g N/m2)")+
  theme_dark()


## ----bland-altman, fig.width=5, fig.height=4, dpi=90,  warning=FALSE, message=FALSE----
bland_altman_plot(data = wheat,
                  obs = obs, 
                  pred = pred)

## ----r2-----------------------------------------------------------------------
R2(data = wheat,
   obs = obs, 
   pred = pred, tidy = TRUE)


## ----rmse---------------------------------------------------------------------
RMSE(data = wheat, 
     obs = obs, 
     pred = pred)

## ----metrics summary----------------------------------------------------------
metrics_summary(data = wheat,
                obs = obs, 
                pred = pred, 
                type = "regression")


## ----metrics summary list-----------------------------------------------------

my.metrics <- c("R2","MBE", "RMSE", "RSR", "NSE", "KGE", "CCC")

metrics_summary(data = wheat,   
                obs = obs,    
                pred = pred,
                type = "regression",
                metrics_list = my.metrics) 


## ----metrics time-series, fig.width=6, fig.height=5, dpi=90-------------------
set.seed(165)

wheat_time <- metrica::wheat %>% sample_n(., size = 20) %>% 
  mutate(Year = seq(2001,2020, by =1)) 

# Plot
wheat_time %>% ggplot2::ggplot(aes(x = Year))+
  geom_point(aes(y = pred, fill = "Predicted", shape = "Predicted"))+
  geom_point(aes(y = obs, fill = "Observed", shape = "Observed"))+
  geom_line(aes(y = pred, col = "Predicted", linetype = "Predicted"), size = .75)+
  geom_line(aes(y = obs, col = "Observed", linetype = "Observed"), size = .75)+
  scale_fill_manual(name = "", values = c("dark red","steelblue"))+
  scale_shape_manual(name = "", values = c(21,24))+
  scale_color_manual(name = "", values = c("dark red","steelblue"))+
  scale_linetype_manual(name = "", values = c(1,2))+
  labs(x = "Year", y = "Wheat Grain N (g/m2)")+
  theme_bw()+
  theme(legend.position = "top")

## ----MASE---------------------------------------------------------------------

# MASE estimate, with naive approach (random-walk, i.e. using observation of t-1 as prediction)
metrica::MASE(data = wheat_time, obs = obs, pred = pred, 
              naive_step = 1, tidy = FALSE, time = "Year")

metrica::MASE(data = wheat_time, obs = obs, pred = pred, 
              naive_step = 1, tidy = FALSE)

# MASE estimate, with mae coming from an independent training set.
metrica::MASE(data = wheat_time, obs = obs, pred = pred, 
              naive_step = 1, tidy = FALSE, time = "Year", oob_mae = 6)



## ----scatter_plot, fig.width=6, fig.height=5, dpi=90--------------------------
scatter_plot(data = wheat,
             obs = obs, 
             pred = pred)


## ----scatter_plot print_metrics, fig.width=6, fig.height=5, dpi=90------------

my.metrica.plot <- scatter_plot(data = wheat,
                                obs = obs, 
                                pred = pred,
                                print_metrics = TRUE, metrics_list = my.metrics)

my.metrica.plot


## ----scatter_plot.edit, fig.width=6, fig.height=5, dpi=90---------------------

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
#                  obs = obs,
#                  pred = pred,
#                  type = "regression") %>%
#    write.csv("metrics_summary.csv")
#  

## ----export plot, eval=F------------------------------------------------------
#  
#  ggsave(plot = my.metrica.plot,
#         "scatter_metrics.png",
#         width = 5,
#         height = 5)

