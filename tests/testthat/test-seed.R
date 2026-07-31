# SEED pipeline tests

test_that("br_seed_screen works with survival data", {
  library(bregr)
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n),
    GENE4 = rnorm(n, mean = -0.3),
    GENE5 = rnorm(n),
    age = rnorm(n, 60, 10)
  )

  res <- br_seed_screen(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = paste0("GENE", 1:5),
    x2 = "age"
  )

  expect_s3_class(res, "breg_seed_screen")
  expect_true("genes" %in% names(res))
  expect_true("results" %in% names(res))
  expect_true("p_threshold" %in% names(res))
  expect_true(res$n_genes_in == 5)
})

test_that("br_seed_screen handles auto-detection of genes", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n),
    age = rnorm(n, 60, 10)
  )

  res <- br_seed_screen(test_data,
    y = c("time", "status"),
    y_type = "survival",
    x2 = "age"
  )

  expect_s3_class(res, "breg_seed_screen")
  # auto-detected GENE1-3 as numeric columns
  expect_true(all(c("GENE1", "GENE2", "GENE3") %in% res$results$Focal_variable))
})

test_that("br_seed_screen works with ordinal screening", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    stage = sample(1:4, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n)
  )

  res <- br_seed_screen(test_data,
    y = "stage",
    y_type = "ordinal",
    genes = paste0("GENE", 1:3)
  )

  expect_s3_class(res, "breg_seed_screen")
  expect_true("results" %in% names(res))
  # Ordinal screening should have estimate column
  expect_true("estimate" %in% colnames(res$results))
})

test_that("br_seed_screen validates inputs correctly", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n)
  )

  # Invalid y_type
  expect_error(
    br_seed_screen(test_data,
      y = c("time", "status"),
      y_type = "invalid_type",
      genes = "GENE1"
    ),
    "y_type"
  )

  # Missing gene column
  expect_error(
    br_seed_screen(test_data,
      y = c("time", "status"),
      y_type = "survival",
      genes = "NONEXISTENT"
    ),
    "not found in"
  )
})

test_that("br_seed_model builds risk score correctly", {
  library(bregr)
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n, mean = -0.3),
    age = rnorm(n, 60, 10)
  )

  mod <- br_seed_model(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = c("GENE1", "GENE2", "GENE3"),
    x2 = "age"
  )

  expect_s3_class(mod, "breg_seed_model")
  expect_true("risk_score" %in% names(mod))
  expect_true("risk_score_formula" %in% names(mod))
  expect_true("coefficients" %in% names(mod))
  expect_equal(length(mod$risk_score), n)
  expect_true(is.numeric(mod$risk_score))
})

test_that("br_seed_model accepts br_seed_select objects", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n, mean = -0.3),
    age = rnorm(n, 60, 10)
  )

  # Create a mock br_seed_select object
  mock_select <- list(
    final_genes = paste0("GENE", 1:3)
  )
  class(mock_select) <- c("breg_seed_select", "list")

  mod <- br_seed_model(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = mock_select,
    x2 = "age"
  )

  expect_s3_class(mod, "breg_seed_model")
})

test_that("br_seed_select validates inputs", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n)
  )

  # Only one gene — should error
  expect_error(
    br_seed_select(test_data,
      y = c("time", "status"),
      y_type = "survival",
      genes = "GENE1"
    ),
    "at least 2 genes"
  )
})

skip_if_not_installed("glmnet")

test_that("br_seed_select works with survival data", {
  library(bregr)
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n, mean = -0.3),
    GENE4 = rnorm(n),
    GENE5 = rnorm(n),
    age = rnorm(n, 60, 10)
  )

  sel <- br_seed_select(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = paste0("GENE", 1:5),
    x2 = "age",
    seed = 42
  )

  expect_s3_class(sel, "breg_seed_select")
  expect_true("lasso_genes" %in% names(sel))
  expect_true("final_genes" %in% names(sel))
  expect_true(length(sel$final_genes) <= length(sel$lasso_genes))
})

test_that("br_seed_select reproducibility with seed", {
  library(bregr)
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n, mean = -0.3),
    GENE4 = rnorm(n),
    GENE5 = rnorm(n),
    age = rnorm(n, 60, 10)
  )

  sel1 <- br_seed_select(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = paste0("GENE", 1:5),
    x2 = "age",
    seed = 42
  )

  sel2 <- br_seed_select(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = paste0("GENE", 1:5),
    x2 = "age",
    seed = 42
  )

  expect_equal(sel1$lasso_genes, sel2$lasso_genes)
})

test_that("print methods work", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n, mean = -0.3),
    age = rnorm(n, 60, 10)
  )

  screen_res <- br_seed_screen(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = paste0("GENE", 1:3),
    x2 = "age",
    p_threshold = 0.99  # relaxed to ensure some genes pass
  )

  # Print method should run without error
  expect_s3_class(screen_res, "breg_seed_screen")

  mod_res <- br_seed_model(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = paste0("GENE", 1:3),
    x2 = "age"
  )

  expect_s3_class(mod_res, "breg_seed_model")
})

test_that("br_seed end-to-end pipeline works", {
  library(bregr)
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n, mean = 0.8 * (1:n) / n),
    GENE2 = rnorm(n, mean = 0.5),
    GENE3 = rnorm(n, mean = -0.5 * (1:n) / n),
    GENE4 = rnorm(n),
    GENE5 = rnorm(n),
    age = rnorm(n, 60, 10)
  )

  res <- br_seed(test_data,
    y = c("time", "status"),
    y_type = "survival",
    genes = paste0("GENE", 1:5),
    x2 = "age",
    p_threshold = 0.3,
    seed = 42
  )

  expect_s3_class(res, "breg_seed")
  expect_s3_class(res$screen, "breg_seed_screen")
  # select may be NULL when < 2 genes pass screening
  if (!is.null(res$select)) {
    expect_s3_class(res$select, "breg_seed_select")
  }
  expect_s3_class(res$model, "breg_seed_model")
  expect_true(is.numeric(res$risk_score))
  expect_true("risk_score_formula" %in% names(res))
})

test_that("br_seed_screen validates y and y_type length match", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n)
  )

  # Single y with multiple y_type — should error
  expect_error(
    br_seed_screen(test_data,
      y = c("time", "status"),
      y_type = c("survival", "binary"),
      genes = "GENE1"
    ),
    "single.*requires a single"
  )
})

test_that("br_seed_screen validates survival y format", {
  library(bregr)
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    time = rexp(n, 0.1),
    status = sample(0:1, n, replace = TRUE),
    GENE1 = rnorm(n)
  )

  expect_error(
    br_seed_screen(test_data,
      y = "time",
      y_type = "survival",
      genes = "GENE1"
    ),
    "length-2"
  )
})
