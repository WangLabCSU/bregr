test_that("br_compare_models works with continuous variables", {
  library(bregr)

  lung <- survival::lung |>
    dplyr::filter(ph.ecog != 3)

  comparison <- br_compare_models(
    lung,
    y = c("time", "status"),
    x = c("ph.karno", "pat.karno"),
    x2 = c("age", "sex"),
    method = "coxph"
  )

  # Check object class
  expect_s3_class(comparison, "breg_comparison")

  # Check that mode column exists
  expect_true("mode" %in% colnames(comparison$combined_results))
  expect_true("mode" %in% colnames(comparison$combined_results_tidy))

  # Check that we have both univariate and multivariate results
  expect_true("univariate" %in% comparison$combined_results$mode)
  expect_true("multivariate" %in% comparison$combined_results$mode)

  # Check that we have results for all focal variables
  expect_true(all(c("ph.karno", "pat.karno") %in% comparison$combined_results$variable))
})

test_that("br_compare_models works with categorical variables", {
  library(bregr)

  lung <- survival::lung |>
    dplyr::filter(ph.ecog != 3)
  lung$ph.ecog <- factor(lung$ph.ecog)

  comparison <- br_compare_models(
    lung,
    y = c("time", "status"),
    x = c("ph.ecog", "ph.karno"),
    x2 = c("age", "sex"),
    method = "coxph"
  )

  expect_s3_class(comparison, "breg_comparison")
  expect_true("mode" %in% colnames(comparison$combined_results_tidy))
})

test_that("br_compare_models requires at least 2 variables", {
  library(bregr)

  lung <- survival::lung |>
    dplyr::filter(ph.ecog != 3)

  expect_error(
    br_compare_models(
      lung,
      y = c("time", "status"),
      x = "ph.karno", # Only one variable
      x2 = c("age", "sex"),
      method = "coxph"
    ),
    "must contain at least 2 variables"
  )
})

test_that("br_show_forest_comparison creates a forest plot", {
  library(bregr)

  lung <- survival::lung |>
    dplyr::filter(ph.ecog != 3)

  comparison <- br_compare_models(
    lung,
    y = c("time", "status"),
    x = c("ph.karno", "pat.karno"),
    x2 = c("age", "sex"),
    method = "coxph"
  )

  p <- br_show_forest_comparison(comparison)

  expect_s3_class(p, "forestplot")
})

test_that("br_show_forest_comparison validates input", {
  library(bregr)

  # Create a regular breg object (not a comparison)
  m <- br_pipeline(
    mtcars,
    y = "mpg",
    x = c("cyl", "disp"),
    method = "gaussian"
  )

  expect_error(
    br_show_forest_comparison(m),
    "breg_comparison"
  )
})

test_that("print method for breg_comparison works", {
  library(bregr)

  lung <- survival::lung |>
    dplyr::filter(ph.ecog != 3)

  comparison <- br_compare_models(
    lung,
    y = c("time", "status"),
    x = c("ph.karno", "pat.karno"),
    x2 = c("age", "sex"),
    method = "coxph"
  )

  expect_s3_class(comparison, "breg_comparison")
})
