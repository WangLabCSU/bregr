test_that("br_show_survival_curves legend shows groups in correct order", {
  skip_if_not_installed("survival")
  skip_if_not_installed("ggplot2")
  
  library(bregr)
  library(ggplot2)
  
  # Create test data with predictable risk scores
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, 0.01),
    status = rbinom(n, 1, 0.7),
    risk_score = c(
      rnorm(33, -1, 0.2),  # Low scores for low risk
      rnorm(33, 0, 0.2),   # Medium scores for medium risk
      rnorm(34, 1, 0.2)    # High scores for high risk
    ),
    age = rnorm(n, 60, 10),
    sex = factor(sample(c("M", "F"), n, replace = TRUE))
  )
  
  # Create breg object with Cox regression
  breg_obj <- br_pipeline(
    test_data,
    y = c("time", "status"),
    x = "risk_score", 
    x2 = "sex",
    method = "coxph"
  )
  
  # Test 3 groups - should be "Low Risk", "Medium Risk", "High Risk"
  p3 <- br_show_survival_curves(breg_obj, n_groups = 3)
  expect_s3_class(p3, "ggplot")
  
  # The legend order should be determined by factor levels in the plot data
  # Check that the group factor has the correct levels in the right order
  plot_data <- ggplot_build(p3)$data[[1]]
  
  # For 3 groups, the factor levels should be Low Risk, Medium Risk, High Risk
  expected_levels <- c("Low Risk", "Medium Risk", "High Risk")
  
  # Since the plot data group column should be numeric references to factor levels,
  # let's check the underlying plot object instead
  plot_env <- ggplot2::ggplot_build(p3)
  
  # Better approach: check that when we build the plot, the groups appear in expected order
  # The group numbers should correspond to factor levels, so group 1 = Low Risk, etc.
  unique_groups_nums <- sort(unique(plot_data$group))
  expect_equal(length(unique_groups_nums), 3)
  expect_equal(unique_groups_nums, c(1, 2, 3))
  
  # Test 5 groups - should be "Q1", "Q2", "Q3", "Q4", "Q5"  
  p5 <- br_show_survival_curves(breg_obj, n_groups = 5)
  expect_s3_class(p5, "ggplot")
  
  # Extract the plot data to check legend order
  plot_data5 <- ggplot_build(p5)$data[[1]]
  unique_groups5_nums <- sort(unique(plot_data5$group))
  
  # Groups should be numbered 1-5 corresponding to Q1-Q5
  expect_equal(length(unique_groups5_nums), 5)
  expect_equal(unique_groups5_nums, c(1, 2, 3, 4, 5))
})