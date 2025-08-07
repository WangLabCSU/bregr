## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ezcox)

## ---- fig.width=7, fig.height=5-----------------------------------------------
library(survival)

lung$ph.ecog <- factor(lung$ph.ecog)

ezcox_group(lung, grp_var = "sex", covariate = "ph.ecog")
ezcox_group(lung, grp_var = "sex", covariate = "ph.ecog", controls = "age")
ezcox_group(lung, grp_var = "sex", covariate = "ph.ecog", controls = "age", add_all = TRUE)

