test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("br_show_nomogram works with Cox regression models", {
  # Create a simple Cox regression model using survival data
  lung <- survival::lung
  if (!is.null(lung)) {
    lung <- lung[!is.na(lung$ph.ecog), ]
    lung <- lung[lung$ph.ecog != 3, ]
    lung$ph.ecog <- factor(lung$ph.ecog)
    
    # Create breg object with Cox regression
    mds <- br_pipeline(
      lung,
      y = c("time", "status"),
      x = c("age", "ph.ecog"),
      x2 = "sex",
      method = "coxph"
    )
    
    # Test that nomogram function works
    expect_no_error(
      plot <- br_show_nomogram(mds, idx = 1)
    )
    
    # Test that it returns a ggplot object
    plot <- br_show_nomogram(mds, idx = 1)
    expect_true(inherits(plot, "ggplot"))
  }
})

test_that("br_show_nomogram validates input parameters", {
  # Create a simple model for testing
  lung <- survival::lung
  if (!is.null(lung)) {
    lung <- lung[!is.na(lung$ph.ecog), ]
    lung <- lung[lung$ph.ecog != 3, ]
    lung$ph.ecog <- factor(lung$ph.ecog)
    
    mds <- br_pipeline(
      lung,
      y = c("time", "status"),
      x = c("age"),
      x2 = "sex",
      method = "coxph"
    )
    
    # Test that multiple indices are rejected
    expect_error(
      br_show_nomogram(mds, idx = c(1, 2)),
      "length-1.*idx.*is required"
    )
  }
})
