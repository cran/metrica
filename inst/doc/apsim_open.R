## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, include=FALSE------------------------------------------------
library(metrica)
library(dplyr)
library(purrr)
library(tidyr)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Use import_apsim_out for APSIM Classic output
soybean.out <- metrica::import_apsim_out(filepath = "../tests/testthat/examples/soybean.out")

head(soybean.out)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Use import_apsim_db for APSIM NextGeneration output
soybean.db <- metrica::import_apsim_db(filename = "soybean.example.db", folder = "../tests/testthat/examples/")

head(soybean.db)

# If observed.data is already as a dataframe, the user may do the match using a simple code like this:
# PO.dataframe <- simulated.data %>% left_join(., observed.data) *by = "col" arg. could be required*


