test_that("br_predict works for Cox regression", {
  skip_if_not_installed("survival")

  # Create test data similar to survival::lung
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = runif(n, 1, 1000),
    status = rbinom(n, 1, 0.6),
    age = rnorm(n, 60, 10),
    sex = factor(sample(c("M", "F"), n, replace = TRUE))
  )

  # Create breg object with Cox regression
  breg_obj <- br_pipeline(
    test_data,
    y = c("time", "status"),
    x = "age",
    x2 = "sex",
    method = "coxph"
  )

  # Test predictions
  predictions <- br_predict(breg_obj)

  expect_true(is.numeric(predictions))
  expect_equal(length(predictions), n)
  expect_true(!any(is.na(predictions)))
})

test_that("br_show_survival_curves works for Cox regression", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")

  # Create test data
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, 0.01),
    status = rbinom(n, 1, 0.7),
    age = rnorm(n, 60, 10),
    sex = factor(sample(c("M", "F"), n, replace = TRUE))
  )

  # Create breg object with Cox regression
  breg_obj <- br_pipeline(
    test_data,
    y = c("time", "status"),
    x = "age",
    x2 = "sex",
    method = "coxph"
  )

  # Test survival curves
  p <- br_show_survival_curves(breg_obj, n_groups = 2)

  expect_s3_class(p, "ggplot")
})

test_that("br_predict fails gracefully for non-coxph models", {
  # Create test data for linear regression
  test_data <- data.frame(
    y = rnorm(50),
    x = rnorm(50)
  )

  # Create breg object with linear regression
  breg_obj <- br_pipeline(
    test_data,
    y = "y",
    x = "x",
    method = "gaussian"
  )

  # Test predictions work for non-coxph
  predictions <- br_predict(breg_obj)
  expect_true(is.numeric(predictions))
})

test_that("br_show_survival_curves fails for non-coxph models", {
  # Create test data for linear regression
  test_data <- data.frame(
    y = rnorm(50),
    x = rnorm(50)
  )

  # Create breg object with linear regression
  breg_obj <- br_pipeline(
    test_data,
    y = "y",
    x = "x",
    method = "gaussian"
  )

  # Test that survival curves fail for non-coxph
  expect_error(
    br_show_survival_curves(breg_obj)
  )
})
