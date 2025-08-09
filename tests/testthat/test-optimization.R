test_that("unnecessary columns are removed from model objects", {
  # Create a dataset with many unnecessary columns
  set.seed(123)
  n_rows <- 50
  n_extra_cols <- 20

  # Create base data (what we actually need)
  base_data <- data.frame(
    y = rnorm(n_rows),
    x1 = rnorm(n_rows),
    x2 = rnorm(n_rows),
    control1 = rnorm(n_rows)
  )

  # Add many unnecessary columns
  extra_data <- replicate(n_extra_cols, rnorm(n_rows), simplify = FALSE)
  names(extra_data) <- paste0("extra_col_", 1:n_extra_cols)
  full_data <- cbind(base_data, extra_data)

  # Run modeling pipeline
  result <- br_pipeline(
    full_data,
    y = "y",
    x = c("x1", "x2"),
    x2 = "control1",
    method = "gaussian"
  )

  # Test that original data is preserved in breg object
  expect_equal(ncol(result@data), ncol(full_data) + 1) # +1 for .row_names

  # Test that individual models only have necessary columns
  model1 <- result@models[["x1"]]
  model2 <- result@models[["x2"]]

  # Each model should only have y + focal_variable + control variables
  # For x1: y, x1, control1 (3 columns)
  expect_equal(ncol(model1$model), 3)
  expect_equal(sort(colnames(model1$model)), sort(c("y", "x1", "control1")))

  # For x2: y, x2, control1 (3 columns)
  expect_equal(ncol(model2$model), 3)
  expect_equal(sort(colnames(model2$model)), sort(c("y", "x2", "control1")))

  # Test that results are still correct
  manual_model1 <- lm(y ~ x1 + control1, data = base_data)
  expect_equal(coef(manual_model1), coef(model1), tolerance = 1e-10)

  manual_model2 <- lm(y ~ x2 + control1, data = base_data)
  expect_equal(coef(manual_model2), coef(model2), tolerance = 1e-10)
})

test_that("necessary columns are identified correctly", {
  # Test the utility function directly
  y <- c("response")
  x <- c("focal1", "focal2", "poly(focal3, 2)")
  x2 <- c("control1", "I(control2^2)")
  group_by <- c("group_var")
  available_cols <- c(
    "response", "focal1", "focal2", "focal3", "control1", "control2",
    "group_var", "extra1", "extra2", ".row_names"
  )

  necessary <- get_necessary_columns(y, x, x2, group_by, available_cols)

  # Should include all variables referenced in y, x, x2, group_by
  expected <- c(
    "response", "focal1", "focal2", "focal3", "control1", "control2",
    "group_var", ".row_names"
  )
  expect_setequal(necessary, expected)

  # Should not include extra columns
  expect_false("extra1" %in% necessary)
  expect_false("extra2" %in% necessary)
})

test_that("optimization works with group_by", {
  set.seed(456)
  n_rows <- 40

  # Create test data with group variable
  test_data <- data.frame(
    y = rnorm(n_rows),
    x1 = rnorm(n_rows),
    control1 = rnorm(n_rows),
    group_var = rep(c("A", "B"), each = n_rows / 2),
    extra1 = rnorm(n_rows),
    extra2 = rnorm(n_rows),
    extra3 = rnorm(n_rows)
  )

  # Run with group_by directly in pipeline
  result <- breg(test_data) |>
    br_set_y("y") |>
    br_set_x("x1") |>
    br_set_x2("control1") |>
    br_set_model("gaussian") |>
    br_run(group_by = "group_var")

  # Check that models only contain necessary columns
  # Should have y, x1, control1 (3 columns) - group_var is used for splitting, not in model
  for (model in result@models) {
    expect_equal(ncol(model$model), 3)
    expect_setequal(colnames(model$model), c("y", "x1", "control1"))
  }
})
