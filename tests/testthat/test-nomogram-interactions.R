test_that("br_show_nomogram handles interaction scenarios correctly", {
  skip_on_cran()
  
  # Test 1: Basic nomogram with factor variables (user's original scenario)
  mtcars$gear <- factor(mtcars$gear)
  mds <- bregr::br_pipeline(mtcars,
                           y = "mpg", 
                           x = c("hp", "gear"),
                           x2 = "vs",
                           method = "gaussian")
  
  # Both models should create nomograms without error
  expect_s3_class({
    p1 <- bregr::br_show_nomogram(mds, idx = 1, fun_at = c(15, 20, 25, 30))
    p1
  }, "ggplot")
  
  expect_s3_class({
    p2 <- bregr::br_show_nomogram(mds, idx = 2, fun_at = c(15, 20, 25, 30))
    p2
  }, "ggplot")
  
  # Test 2: Cox nomogram with factor variables
  lung <- survival::lung |> dplyr::filter(ph.ecog != 3, !is.na(ph.ecog))
  lung$ph.ecog <- factor(lung$ph.ecog)
  lung$sex <- factor(lung$sex)
  
  mds_cox <- bregr::br_pipeline(lung,
                               y = c("time", "status"),
                               x = c("age", "ph.ecog"),
                               x2 = "sex",
                               method = "coxph")
  
  expect_s3_class({
    p3 <- bregr::br_show_nomogram(mds_cox, idx = 1)
    p3
  }, "ggplot")
  
  expect_s3_class({
    p4 <- bregr::br_show_nomogram(mds_cox, idx = 2) 
    p4
  }, "ggplot")
  
  # Test 3: Nomogram with character factor levels
  mtcars$cyl_factor <- factor(mtcars$cyl, labels = c("Four", "Six", "Eight"))
  mds_char <- bregr::br_pipeline(mtcars,
                                y = "mpg",
                                x = c("hp", "cyl_factor"),
                                x2 = "vs",
                                method = "gaussian")
  
  expect_s3_class({
    p5 <- bregr::br_show_nomogram(mds_char, idx = 1, fun_at = c(15, 20, 25))
    p5
  }, "ggplot")
  
  expect_s3_class({
    p6 <- bregr::br_show_nomogram(mds_char, idx = 2, fun_at = c(15, 20, 25))
    p6
  }, "ggplot")
})
