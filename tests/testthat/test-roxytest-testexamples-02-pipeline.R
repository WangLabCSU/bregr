# Generated by roxytest: do not edit by hand!

# File R/"02-pipeline.R": @testexamples

test_that("[unknown alias] @ L104", {
  
  library(bregr)
  # 1. Pipeline -------------------------
  # 1.1. A single linear model ----------
  m <- breg(mtcars) |> # set model data
    br_set_y("mpg") |> # set dependent variable
    br_set_x("qsec") |> # set focal variables
    br_set_model("gaussian") |> # set model
    br_run() # run analysis
  
  # get model tidy result
  br_get_results(m, tidy = TRUE)
  # or m@results_tidy
  
  # compare with R's built-in function
  lm(mpg ~ qsec, data = mtcars) |> summary()
  # 1.2. Batch linear model -------------
  # control variables are injected in all constructed models
  # focal variables are injected in constructed models one by one
  m2 <- breg(mtcars) |>
    br_set_y("mpg") |>
    br_set_x(colnames(mtcars)[2:4]) |> # set focal variables
    br_set_x2("vs") |> # set control variables
    br_set_model("gaussian") |>
    br_run()
  # 1.3. Group by model -------------
  m3 <- breg(mtcars) |>
    br_set_y("mpg") |>
    br_set_x("cyl") |>
    br_set_x2("wt") |> # set control variables
    br_set_model("gaussian") |>
    br_run(group_by = "am")
  
  # 2. All-in-one pipeline wrapper ---
  m4 <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )
  
  # 3. Customized model -----------
  dt = data.frame(x = rnorm(100))
  dt$y = rpois(100, exp(1+dt$x))
  m5 <- breg(dt) |>
    br_set_y("y") |>
    br_set_x("x") |>
    br_set_model(method = 'quasi(variance = "mu", link = "log")') |>
    br_run()
  
  assert_breg_obj(m)
  assert_breg_obj(m2)
  assert_breg_obj(m3)
  assert_breg_obj(m4)
  assert_breg_obj(m5)
})

