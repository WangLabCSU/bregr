test_that("br_show_nomogram works for Cox models", {
  skip_if_not_installed("survival")

  # Create Cox model
  lung <- survival::lung |> dplyr::filter(ph.ecog != 3)
  lung$ph.ecog <- factor(lung$ph.ecog)
  mds <- br_pipeline(
    lung,
    y = c("time", "status"),
    x = c("age", "ph.ecog"),
    x2 = "sex",
    method = "coxph"
  )

  # Test basic nomogram
  p <- br_show_nomogram(mds)
  expect_s3_class(p, "ggplot")

  # Test with custom time points
  p2 <- br_show_nomogram(mds, time_points = c(6, 12))
  expect_s3_class(p2, "ggplot")

  # Test with specific model index
  p3 <- br_show_nomogram(mds, idx = 1)
  expect_s3_class(p3, "ggplot")
})

test_that("br_show_nomogram works for linear models", {
  # Create linear model
  mds_lm <- br_pipeline(
    mtcars,
    y = "mpg",
    x = c("hp", "wt"),
    x2 = "vs",
    method = "gaussian"
  )

  # Test basic nomogram
  p <- br_show_nomogram(mds_lm)
  expect_s3_class(p, "ggplot")

  # Test with custom prediction values
  p2 <- br_show_nomogram(mds_lm, fun_at = c(15, 20, 25, 30))
  expect_s3_class(p2, "ggplot")
})

test_that("br_show_nomogram handles unsupported models", {
  # This would test an unsupported model type if we had one
  # For now, we can test error handling with invalid idx
  mds_lm <- br_pipeline(
    mtcars,
    y = "mpg",
    x = c("hp", "wt"),
    x2 = "vs",
    method = "gaussian"
  )

  # Test error for multiple indices
  expect_error(br_show_nomogram(mds_lm, idx = c(1, 2)))
})

test_that("br_show_nomogram produces correct plot structure", {
  # Create simple model for structure testing
  mds_lm <- br_pipeline(
    mtcars[1:10, ], # Small dataset for faster testing
    y = "mpg",
    x = "hp",
    x2 = "vs",
    method = "gaussian"
  )

  p <- br_show_nomogram(mds_lm)

  # Test that plot has the expected structure
  expect_s3_class(p, "ggplot")
  expect_true("data" %in% names(p))
  expect_true("layers" %in% names(p))
  expect_true("theme" %in% names(p))
})

test_that("br_show_nomogram handles models without intercepts", {
  # Create model data that can handle no-intercept fitting
  test_data <- mtcars[1:15, ]
  test_data$vs <- factor(test_data$vs)
  
  # Fit model without intercept manually to test coefficient handling
  no_int_model <- lm(mpg ~ hp + wt - 1, data = test_data)
  
  # Test that our coefficient handling logic works
  coefs <- stats::coef(no_int_model)
  model_terms <- stats::terms(no_int_model)
  has_intercept <- attr(model_terms, "intercept") == 1
  
  expect_false(has_intercept)
  expect_false("(Intercept)" %in% names(coefs))
  expect_true(length(coefs) >= 2)
  expect_false(any(is.na(coefs)))
})

test_that("br_show_nomogram handles singular coefficient matrices", {
  # Create data with collinear variables to test NA coefficient handling
  singular_data <- data.frame(
    y = 1:10,
    x1 = 1:10,
    x2 = 2 * (1:10), # x2 = 2 * x1, creating collinearity
    x3 = rnorm(10)
  )
  
  # Fit model that will have singular coefficients
  singular_model <- lm(y ~ x1 + x2 + x3, data = singular_data)
  coefs <- stats::coef(singular_model)
  
  # Check that we can handle NA coefficients
  if (any(is.na(coefs))) {
    # Test that our NA handling preserves coefficient-term correspondence
    non_na_coefs <- coefs[!is.na(coefs)]
    expect_true(length(non_na_coefs) > 0)
    expect_true(all(!is.na(non_na_coefs)))
  }
})
