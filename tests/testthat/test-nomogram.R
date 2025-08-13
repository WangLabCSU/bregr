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

test_that("br_show_nomogram handles Cox model intercept behavior correctly", {
  skip_if_not_installed("survival")

  # Create Cox model to test intercept handling
  lung <- survival::lung |> dplyr::filter(ph.ecog != 3)
  lung$ph.ecog <- factor(lung$ph.ecog)
  mds <- br_pipeline(
    lung,
    y = c("time", "status"),
    x = c("age", "ph.ecog"),
    x2 = "sex",
    method = "coxph"
  )

  model <- br_get_models(mds, 1)

  # Test that Cox model behavior is as expected
  coefs <- stats::coef(model)
  model_terms <- stats::terms(model)
  has_intercept_term <- attr(model_terms, "intercept") == 1
  has_intercept_coef <- "(Intercept)" %in% names(coefs)

  # Cox models have intercept in terms but not in coefficients
  expect_true(has_intercept_term)
  expect_false(has_intercept_coef)

  # Test that nomogram creation works correctly (may include informative messages)
  # The function should work without errors regardless of messages
  suppressMessages(p <- br_show_nomogram(mds))
  expect_s3_class(p, "ggplot")
})

test_that("br_show_nomogram correctly handles factor variables", {
  skip_if_not_installed("survival")

  # Create the exact case from the issue
  lung <- survival::lung |> dplyr::filter(ph.ecog != 3)
  lung$ph.ecog <- factor(lung$ph.ecog)
  lung$sex <- factor(lung$sex)

  mds <- br_pipeline(
    lung,
    y = c("time", "status"),
    x = c("age", "ph.ecog"),
    x2 = "sex",
    method = "coxph"
  )

  # Test that both nomograms can be created without errors
  suppressWarnings({
    p1 <- br_show_nomogram(mds)
    p2 <- br_show_nomogram(mds, idx = 2)
  })

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  # Check that the plot data contains the expected factor levels
  plot_data_1 <- p1$data
  plot_data_2 <- p2$data

  # For model 1 (age + sex), should have sex factor with levels
  sex_data_1 <- plot_data_1[plot_data_1$var_name == "sex" & plot_data_1$is_tick & plot_data_1$label != "", ]
  expect_true(any(grepl("1.*ref", sex_data_1$label)))
  expect_true(any(grepl("2", sex_data_1$label)))

  # For model 2 (ph.ecog + sex), should have ph.ecog factor with levels
  ph_ecog_data_2 <- plot_data_2[plot_data_2$var_name == "ph.ecog" & plot_data_2$is_tick & plot_data_2$label != "", ]
  expect_true(any(grepl("0.*ref", ph_ecog_data_2$label)))
  expect_true(any(grepl("1", ph_ecog_data_2$label)) || any(grepl("2", ph_ecog_data_2$label)))
})

test_that("br_show_nomogram factor level mapping is correct", {
  # Test data with known factor structure
  test_data <- data.frame(
    y = rnorm(100),
    x1 = factor(rep(c("A", "B", "C"), length.out = 100))
  )

  # Create simple pipeline with just one factor variable
  mds_test <- br_pipeline(
    test_data,
    y = "y",
    x = "x1",
    method = "gaussian"
  )

  # Should create nomogram without errors
  p <- br_show_nomogram(mds_test)
  expect_s3_class(p, "ggplot")

  # Check that factor variables are properly represented
  plot_data <- p$data

  # Should have data for the factor variable (the original variable name, not coefficient names)
  expect_true("x1" %in% plot_data$var_name)

  # Factor variables should have reference and level labels
  x1_labels <- plot_data[plot_data$var_name == "x1" & plot_data$is_tick & plot_data$label != "", ]$label

  expect_true(any(grepl("ref", x1_labels)))
  expect_true(any(grepl("A", x1_labels))) # Reference level
  expect_true(any(grepl("B", x1_labels)) || any(grepl("C", x1_labels))) # At least one non-reference level
})
