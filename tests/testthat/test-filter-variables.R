test_that("Variable filtering works correctly", {
  # Create test data with different types of problematic variables
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    y = rnorm(n),
    good_var = rnorm(n, mean = 5, sd = 2), # Should pass all filters
    high_na_var = c(rep(NA, 85), rnorm(15)), # 85% NA - should be filtered with default threshold
    constant_var = rep(1, n), # Zero variance - should be filtered
    near_constant_var = c(rep(1, 99), 1.0000001), # Very low variance - should be filtered
    low_var = rnorm(n, mean = 0, sd = 1e-8), # Very low variance - should be filtered
    categorical_var = sample(letters[1:3], n, replace = TRUE) # Categorical - should not be filtered
  )

  # Test filtering function directly
  filter_result <- bregr:::filter_variables_x(
    test_data,
    c("good_var", "high_na_var", "constant_var", "near_constant_var", "low_var", "categorical_var"),
    filter_na_prop = 0.8,
    filter_sd_min = 1e-6,
    filter_var_min = 1e-6,
    filter_min_levels = 2
  )

  # Should keep good_var and categorical_var (non-numeric)
  expect_true("good_var" %in% filter_result$filtered_x)
  expect_true("categorical_var" %in% filter_result$filtered_x)

  # Should filter out problematic variables
  expect_true("high_na_var" %in% filter_result$filtered_out)
  expect_true("constant_var" %in% filter_result$filtered_out)
  expect_true("near_constant_var" %in% filter_result$filtered_out)
  expect_true("low_var" %in% filter_result$filtered_out)

  # Check summary
  expect_equal(filter_result$filter_summary$total, 6)
  expect_equal(filter_result$filter_summary$filtered, 4)
  expect_equal(filter_result$filter_summary$kept, 2)
})

test_that("Categorical variable filtering works correctly", {
  # Create test data with different types of categorical variables
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    y = rnorm(n),
    # Character variables
    good_char = sample(c("A", "B", "C"), n, replace = TRUE), # 3 levels - should pass
    constant_char = rep("A", n), # 1 level - should be filtered
    high_na_char = c(rep(NA, 85), sample(c("A", "B"), 15, replace = TRUE)), # High NA - should be filtered

    # Factor variables
    good_factor = factor(sample(c("X", "Y", "Z"), n, replace = TRUE)), # 3 levels - should pass
    constant_factor = factor(rep("X", n)), # 1 level - should be filtered
    high_na_factor = factor(c(rep(NA, 85), sample(c("X", "Y"), 15, replace = TRUE))), # High NA - should be filtered

    # Logical variables
    good_logical = sample(c(TRUE, FALSE), n, replace = TRUE), # 2 levels - should pass
    constant_logical = rep(TRUE, n), # 1 level - should be filtered

    # Mixed with numeric
    good_numeric = rnorm(n, sd = 2) # Should pass
  )

  all_vars <- c(
    "good_char", "constant_char", "high_na_char",
    "good_factor", "constant_factor", "high_na_factor",
    "good_logical", "constant_logical", "good_numeric"
  )

  # Test with default parameters
  filter_result <- bregr:::filter_variables_x(
    test_data, all_vars,
    filter_na_prop = 0.8,
    filter_sd_min = 1e-6,
    filter_var_min = 1e-6,
    filter_min_levels = 2
  )

  # Should keep variables with sufficient levels/variance
  expect_true("good_char" %in% filter_result$filtered_x)
  expect_true("good_factor" %in% filter_result$filtered_x)
  expect_true("good_logical" %in% filter_result$filtered_x)
  expect_true("good_numeric" %in% filter_result$filtered_x)

  # Should filter out problematic variables
  expect_true("constant_char" %in% filter_result$filtered_out)
  expect_true("high_na_char" %in% filter_result$filtered_out)
  expect_true("constant_factor" %in% filter_result$filtered_out)
  expect_true("high_na_factor" %in% filter_result$filtered_out)
  expect_true("constant_logical" %in% filter_result$filtered_out)

  # Check counts
  expect_equal(filter_result$filter_summary$kept, 4) # 4 good variables
  expect_equal(filter_result$filter_summary$filtered, 5) # 5 filtered variables
})

test_that("br_run works with filtering disabled", {
  # Test that existing functionality is not broken
  m <- breg(mtcars) |>
    br_set_y("mpg") |>
    br_set_x("qsec") |>
    br_set_model("gaussian") |>
    br_run(filter_x = FALSE)

  expect_true(inherits(m, "bregr::breg"))
  expect_true(nrow(m@results_tidy) > 0)
})

test_that("br_run works with filtering enabled", {
  # Create test data where some variables should be filtered
  set.seed(123)
  test_data <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50, sd = 2), # Good variable
    x2 = c(rep(NA, 45), rnorm(5)), # Too many NAs
    x3 = rep(1, 50) # Constant
  )

  # Expect message about filtering
  expect_message(
    m <- breg(test_data) |>
      br_set_y("y") |>
      br_set_x(c("x1", "x2", "x3")) |>
      br_set_model("gaussian") |>
      br_run(filter_x = TRUE),
    "Pre-filtering removed"
  )

  expect_true(inherits(m, "bregr::breg"))
  # Should only have results for x1
  expect_equal(length(unique(m@results_tidy$Focal_variable)), 1)
  expect_true("x1" %in% m@results_tidy$Focal_variable)
})

test_that("br_pipeline works with filtering", {
  # Create test data
  set.seed(123)
  test_data <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50, sd = 2), # Good variable
    x2 = rep(1, 50) # Constant - should be filtered
  )

  expect_message(
    m <- br_pipeline(
      test_data,
      y = "y",
      x = c("x1", "x2"),
      method = "gaussian",
      filter_x = TRUE
    ),
    "Pre-filtering removed"
  )

  expect_true(inherits(m, "bregr::breg"))
  expect_equal(length(unique(m@results_tidy$Focal_variable)), 1)
})

test_that("Error when all variables are filtered out", {
  # Create data where all focal variables should be filtered
  test_data <- data.frame(
    y = rnorm(50),
    x1 = rep(1, 50), # Constant
    x2 = rep(2, 50) # Constant
  )

  expect_error(
    breg(test_data) |>
      br_set_y("y") |>
      br_set_x(c("x1", "x2")) |>
      br_set_model("gaussian") |>
      br_run(filter_x = TRUE),
    "All focal variables were filtered out"
  )
})

test_that("Categorical filtering with custom min_levels parameter", {
  # Test custom min_levels parameter
  test_data <- data.frame(
    y = rnorm(50),
    x1 = sample(c("A", "B"), 50, replace = TRUE), # 2 levels
    x2 = sample(c("X", "Y", "Z"), 50, replace = TRUE) # 3 levels
  )

  # With min_levels = 3, x1 should be filtered out but x2 should pass
  filter_result <- bregr:::filter_variables_x(
    test_data, c("x1", "x2"),
    filter_min_levels = 3
  )

  expect_true("x2" %in% filter_result$filtered_x)
  expect_true("x1" %in% filter_result$filtered_out)
  expect_equal(filter_result$filter_summary$filtered, 1)
  expect_equal(filter_result$filter_summary$kept, 1)
})

test_that("Integration with br_run and br_pipeline for categorical variables", {
  # Create test data with categorical variables that should be filtered
  set.seed(123)
  test_data <- data.frame(
    y = rnorm(50),
    x1 = rnorm(50, sd = 2), # Good numeric variable
    x2 = rep("constant", 50), # Constant categorical - should be filtered
    x3 = sample(c("A", "B", "C"), 50, replace = TRUE) # Good categorical variable
  )

  # Test br_run
  expect_message(
    m1 <- breg(test_data) |>
      br_set_y("y") |>
      br_set_x(c("x1", "x2", "x3")) |>
      br_set_model("gaussian") |>
      br_run(filter_x = TRUE),
    "Pre-filtering removed"
  )

  expect_true(inherits(m1, "bregr::breg"))
  # Should only have results for x1 and x3 (x2 filtered out)
  expect_equal(length(unique(m1@results_tidy$Focal_variable)), 2)
  expect_true("x1" %in% m1@results_tidy$Focal_variable)
  expect_true("x3" %in% m1@results_tidy$Focal_variable)
  expect_false("x2" %in% m1@results_tidy$Focal_variable)

  # Test br_pipeline
  expect_message(
    m2 <- br_pipeline(
      test_data,
      y = "y",
      x = c("x1", "x2", "x3"),
      method = "gaussian",
      filter_x = TRUE
    ),
    "Pre-filtering removed"
  )

  expect_true(inherits(m2, "bregr::breg"))
  expect_equal(length(unique(m2@results_tidy$Focal_variable)), 2)
})
