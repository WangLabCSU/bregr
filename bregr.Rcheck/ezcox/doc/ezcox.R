## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
library(survival)
library(ezcox)
data(lung)
head(lung)

# Build unvariable models
ezcox(lung, covariates = c("age", "sex", "ph.ecog"))

# Build multi-variable models
# Control variable 'age'
ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age")

## -----------------------------------------------------------------------------
ezcox_parallel(lung, covariates = c("sex", "ph.ecog"), controls = "age")

## -----------------------------------------------------------------------------
lung$ph.ecog = factor(lung$ph.ecog)
zz = ezcox(lung, covariates = "sex", controls = "ph.ecog")

zz

# At default, it will drop all control variables
filter_ezcox(zz)

# You can specify levels to filter out
filter_ezcox(zz, c("0", "2"))
filter_ezcox(zz, c("0", "2"), type = "contrast")
filter_ezcox(zz, c("0", "2"), type = "ref")

# More see ?filter_ezcox

## -----------------------------------------------------------------------------
zz = ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age", return_models=TRUE)
mds = get_models(zz)
str(mds, max.level = 1)

## ---- fig.height=5, fig.width=6-----------------------------------------------
show_models(mds)
# Set model names
show_models(mds, model_names = paste0("Model ", 1:2))
# Merge all models and drop control variables
show_models(mds, merge_models = TRUE, drop_controls = TRUE)

