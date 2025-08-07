test_that("diagnostic functions work", {
  skip_if_not_installed("survival")
  
  # Create test data - simple version of lung data
  lung <- survival::lung
  lung <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog != 3, ]
  lung$ph.ecog <- factor(lung$ph.ecog)
  
  # Create a breg object with Cox models
  mds <- br_pipeline(
    lung,
    y = c("time", "status"),
    x = c("ph.ecog", "ph.karno"),
    x2 = c("age", "sex"),
    method = "coxph"
  )
  
  # Test br_test_ph
  ph_test <- br_test_ph(mds)
  expect_s3_class(ph_test, "br_ph_test")
  expect_type(ph_test, "list")
  expect_gt(length(ph_test), 0)
  
  # Test br_diagnose
  diagnostics <- br_diagnose(mds)
  expect_s3_class(diagnostics, "br_diagnostics")
  expect_type(diagnostics, "list")
  expect_gt(length(diagnostics), 0)
  
  # Test that results have the expected structure
  first_diag <- diagnostics[[1]]
  expect_equal(first_diag$model_type, "coxph")
  expect_type(first_diag$summary, "list")
  
  # Test printing doesn't error
  expect_no_error(print(ph_test))
  expect_no_error(print(diagnostics))
})

test_that("diagnostic visualization works", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")
  
  # Create test data
  lung <- survival::lung
  lung <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog != 3, ]
  lung$ph.ecog <- factor(lung$ph.ecog)
  
  # Create a breg object with Cox models
  mds <- br_pipeline(
    lung,
    y = c("time", "status"),
    x = c("ph.ecog"),  # Single variable for simpler test
    x2 = c("age", "sex"),
    method = "coxph"
  )
  
  # Test br_show_diagnostics
  plot_result <- br_show_diagnostics(mds, idx = 1)
  expect_s3_class(plot_result, "ggplot")
})

test_that("error handling works correctly", {
  skip_if_not_installed("survival")
  
  # Create test data with non-Cox models
  mtcars_data <- mtcars
  
  mds_lm <- br_pipeline(
    mtcars_data,
    y = "mpg",
    x = c("cyl", "hp"),
    x2 = "vs",
    method = "gaussian"
  )
  
  # Test that br_test_ph warns for non-Cox models
  expect_warning(
    ph_test <- br_test_ph(mds_lm),
    "No Cox proportional hazards models found"
  )
  expect_null(ph_test)
  
  # Test that br_diagnose works for non-Cox models
  diagnostics <- br_diagnose(mds_lm)
  expect_s3_class(diagnostics, "br_diagnostics")
  expect_equal(diagnostics[[1]]$model_type, "glm")
})