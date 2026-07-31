# Non-standard variable name support (#69)
# Tests that bregr systematically handles special characters in column names:
# :: hyphen, leading digits, spaces, R reserved words, backtick-quoted input

test_that("br_pipeline handles FGFR3::TACC3 as focal variable", {
  library(bregr)
  test_data <- data.frame(
    y = rnorm(30),
    age = rnorm(30),
    gene = rnorm(30)
  )
  colnames(test_data)[3] <- "FGFR3::TACC3"

  m <- br_pipeline(test_data, y = "y", x = "FGFR3::TACC3", x2 = "age",
                   method = "gaussian")
  expect_true(nrow(m@results_tidy) > 0)
})

test_that("br_pipeline handles hyphenated names like EGFR-AS1", {
  library(bregr)
  test_data <- data.frame(y = rnorm(30), g = rnorm(30))
  colnames(test_data)[2] <- "EGFR-AS1"

  m <- br_pipeline(test_data, y = "y", x = "EGFR-AS1", method = "gaussian")
  expect_true(nrow(m@results_tidy) > 0)
})

test_that("br_pipeline handles names starting with digits like 1p, 2q", {
  library(bregr)
  test_data <- data.frame(y = rnorm(30), g = rnorm(30))
  colnames(test_data)[2] <- "1p"

  m <- br_pipeline(test_data, y = "y", x = "1p", method = "gaussian")
  expect_true(nrow(m@results_tidy) > 0)
})

test_that("br_pipeline handles names with spaces", {
  library(bregr)
  test_data <- data.frame(y = rnorm(30), g = rnorm(30))
  colnames(test_data)[2] <- "gene name"

  m <- br_pipeline(test_data, y = "y", x = "gene name", method = "gaussian")
  expect_true(nrow(m@results_tidy) > 0)
})

test_that("br_pipeline handles R reserved words as column names", {
  library(bregr)
  test_data <- data.frame(y = rnorm(30), a = rnorm(30), b = rnorm(30))
  colnames(test_data)[2:3] <- c("if", "TRUE")

  m <- br_pipeline(test_data, y = "y", x = c("if", "TRUE"), method = "gaussian")
  expect_equal(length(m@models), 2)
  expect_true(nrow(m@results_tidy) > 0)
})

test_that("br_pipeline handles multiple non-standard names together", {
  library(bregr)
  test_data <- data.frame(
    y = rnorm(30),
    age = rnorm(30),
    a = rnorm(30), b = rnorm(30), c = rnorm(30)
  )
  colnames(test_data)[3:5] <- c("FGFR3::TACC3", "EGFR-AS1", "1p")

  m <- br_pipeline(test_data, y = "y",
                   x = c("FGFR3::TACC3", "EGFR-AS1", "1p"),
                   x2 = "age", method = "gaussian")
  expect_equal(length(m@models), 3)
})

test_that("br_pipeline handles non-standard names as control (x2) variables", {
  library(bregr)
  test_data <- data.frame(
    y = rnorm(30),
    a = rnorm(30), b = rnorm(30)
  )
  colnames(test_data)[2:3] <- c("FGFR3::TACC3", "EGFR-AS1")

  m <- br_pipeline(test_data, y = "y", x = "EGFR-AS1",
                   x2 = "FGFR3::TACC3", method = "gaussian")
  expect_true(nrow(m@results_tidy) > 0)
})

test_that("br_pipeline handles user-provided backtick-quoted names", {
  library(bregr)
  test_data <- data.frame(y = rnorm(30), age = rnorm(30), g = rnorm(30))
  colnames(test_data)[3] <- "FGFR3::TACC3"

  # User passes name already wrapped in backticks
  m <- br_pipeline(test_data, y = "y", x = "`FGFR3::TACC3`", method = "gaussian")
  expect_true(nrow(m@results_tidy) > 0)

  # Backtick-quoted name that doesn't need quoting
  test_data2 <- data.frame(y = rnorm(30), g = rnorm(30))
  colnames(test_data2)[2] <- "TP53"
  m2 <- br_pipeline(test_data2, y = "y", x = "`TP53`", method = "gaussian")
  expect_true(nrow(m2@results_tidy) > 0)
})

test_that("br_pipeline handles non-standard names as y variable", {
  library(bregr)
  test_data <- data.frame(a = rnorm(30), g = rnorm(30))
  colnames(test_data)[1] <- "response::score"

  m <- br_pipeline(test_data, y = "response::score", x = "g", method = "gaussian")
  expect_true(nrow(m@results_tidy) > 0)
})
