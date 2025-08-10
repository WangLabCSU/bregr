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
