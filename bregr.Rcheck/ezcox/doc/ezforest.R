## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(survival)
library(ezcox)

## ----fig.width=4, fig.height=3------------------------------------------------
t1 <- ezcox(lung, covariates = c(
  "age", "sex",
  "ph.karno", "pat.karno"
))
p <- forester(t1, xlim = c(0, 1.5))
p
p2 <- forester(t1, xlim = c(0.5, 1.5))
p2

## ---- fig.width=7, fig.height=5-----------------------------------------------
show_forest(lung, covariates = c("sex", "ph.ecog"), controls = "age")
show_forest(lung, covariates = c("sex", "ph.ecog"), controls = "age", merge_models = TRUE)
show_forest(lung,
  covariates = c("sex", "ph.ecog"), controls = "age", merge_models = TRUE,
  drop_controls = TRUE
)
show_forest(lung,
  covariates = c("sex", "ph.ecog"), controls = "age", merge_models = TRUE,
  vars_to_show = "sex"
)

