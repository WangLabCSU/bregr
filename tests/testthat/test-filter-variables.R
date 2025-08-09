test_that("Variable filtering works correctly", {
  # Create test data with different types of problematic variables
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    y = rnorm(n),
    good_var = rnorm(n, mean = 5, sd = 2),  # Should pass all filters
    high_na_var = c(rep(NA, 85), rnorm(15)),  # 85% NA - should be filtered with default threshold
    constant_var = rep(1, n),  # Zero variance - should be filtered
    near_constant_var = c(rep(1, 99), 1.0000001),  # Very low variance - should be filtered
    low_var = rnorm(n, mean = 0, sd = 1e-8),  # Very low variance - should be filtered
    categorical_var = sample(letters[1:3], n, replace = TRUE)  # Categorical - should not be filtered
  )
  
  # Test filtering function directly
  filter_result <- bregr:::filter_variables_x(
    test_data, 
    c("good_var", "high_na_var", "constant_var", "near_constant_var", "low_var", "categorical_var"),
    filter_na_prop = 0.8,
    filter_sd_min = 1e-6,
    filter_var_min = 1e-6
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
    x1 = rnorm(50, sd = 2),  # Good variable
    x2 = c(rep(NA, 45), rnorm(5)),  # Too many NAs
    x3 = rep(1, 50)  # Constant
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
    x1 = rnorm(50, sd = 2),  # Good variable
    x2 = rep(1, 50)  # Constant - should be filtered
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
    x1 = rep(1, 50),  # Constant
    x2 = rep(2, 50)   # Constant
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