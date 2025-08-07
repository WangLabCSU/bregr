pkgname <- "bregr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('bregr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("accessors")
### * accessors

flush(stderr()); flush(stdout())

### Name: accessors
### Title: Accessor functions for 'breg' objects
### Aliases: accessors br_get_data br_get_y br_get_x br_get_n_x br_get_x2
###   br_get_n_x2 br_get_group_by br_get_config br_get_models br_get_model
###   br_get_model_names br_rename_models br_get_results

### ** Examples

m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)
br_get_data(m)
br_get_y(m)
br_get_x(m)
br_get_n_x(m)
br_get_x2(m)
br_get_n_x2(m)
br_get_group_by(m)
br_get_config(m)
br_get_models(m)
br_get_model(m, 1)
br_get_n_x2(m)
br_get_results(m)
br_get_results(m, tidy = TRUE)
br_get_results(m, tidy = TRUE, term == "cyl")




cleanEx()
nameEx("br_show_fitted_line")
### * br_show_fitted_line

flush(stderr()); flush(stdout())

### Name: br_show_fitted_line
### Title: Show fitted regression line with 'visreg' interface
### Aliases: br_show_fitted_line

### ** Examples

if (rlang::is_installed("visreg")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )

  if (interactive()) {
    br_show_fitted_line(m)
  }
  br_show_fitted_line(m, xvar = "cyl")
}




cleanEx()
nameEx("br_show_fitted_line_2d")
### * br_show_fitted_line_2d

flush(stderr()); flush(stdout())

### Name: br_show_fitted_line_2d
### Title: Show 2d fitted regression line with 'visreg' interface
### Aliases: br_show_fitted_line_2d

### ** Examples

if (rlang::is_installed("visreg")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )

  br_show_fitted_line_2d(m, xvar = "cyl", yvar = "mpg")
}




cleanEx()
nameEx("br_show_forest")
### * br_show_forest

flush(stderr()); flush(stdout())

### Name: br_show_forest
### Title: Show a forest plot for regression results
### Aliases: br_show_forest

### ** Examples

m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)
br_show_forest(m)
br_show_forest(m, clean = TRUE, drop = 3)
br_show_forest(m, clean = FALSE)



cleanEx()
nameEx("br_show_forest_ggstats")
### * br_show_forest_ggstats

flush(stderr()); flush(stdout())

### Name: br_show_forest_ggstats
### Title: Show a forest plot with 'ggstats' interface
### Aliases: br_show_forest_ggstats

### ** Examples

if (rlang::is_installed("ggstats")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )
  br_show_forest_ggstats(m)
}




cleanEx()
nameEx("br_show_forest_ggstatsplot")
### * br_show_forest_ggstatsplot

flush(stderr()); flush(stdout())

### Name: br_show_forest_ggstatsplot
### Title: Show a forest plot with 'ggstatsplot' interface
### Aliases: br_show_forest_ggstatsplot

### ** Examples

if (rlang::is_installed("ggstats")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )
  br_show_forest_ggstatsplot(m)
}




cleanEx()
nameEx("br_show_risk_network")
### * br_show_risk_network

flush(stderr()); flush(stdout())

### Name: br_show_risk_network
### Title: Show connected risk network plot
### Aliases: br_show_risk_network

### ** Examples

lung <- survival::lung
# Cox-PH regression
mod_surv <- br_pipeline(
  data = lung,
  y = c("time", "status"),
  x = c("age", "ph.ecog", "ph.karno"),
  x2 = c("factor(sex)"),
  method = "coxph"
)
p <- br_show_risk_network(mod_surv)
p



cleanEx()
nameEx("br_show_table")
### * br_show_table

flush(stderr()); flush(stdout())

### Name: br_show_table
### Title: Show model tidy results in table format
### Aliases: br_show_table

### ** Examples

m <- br_pipeline(mtcars,
  y = "mpg",
  x = colnames(mtcars)[2:4],
  x2 = "vs",
  method = "gaussian"
)

br_show_table(m)
br_show_table(m, export = TRUE)
if (interactive()) {
  br_show_table(m, export = TRUE, args_table_export = list(format = "html"))
}



cleanEx()
nameEx("br_show_table_gt")
### * br_show_table_gt

flush(stderr()); flush(stdout())

### Name: br_show_table_gt
### Title: Show regression models with 'gtsummary' interface
### Aliases: br_show_table_gt

### ** Examples

if (rlang::is_installed("gtsummary")) {
  m <- br_pipeline(mtcars,
    y = "mpg",
    x = colnames(mtcars)[2:4],
    x2 = "vs",
    method = "gaussian"
  )
  br_show_table_gt(m)
}




cleanEx()
nameEx("breg")
### * breg

flush(stderr()); flush(stdout())

### Name: breg
### Title: Creates a new breg-class object
### Aliases: breg

### ** Examples

obj <- breg()
obj
print(obj, raw = TRUE)




cleanEx()
nameEx("pipeline")
### * pipeline

flush(stderr()); flush(stdout())

### Name: pipeline
### Title: Modeling and analysis pipeline
### Aliases: pipeline br_pipeline br_set_y br_set_x br_set_x2 br_set_model
###   br_run

### ** Examples

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
dt <- data.frame(x = rnorm(100))
dt$y <- rpois(100, exp(1 + dt$x))
m5 <- breg(dt) |>
  br_set_y("y") |>
  br_set_x("x") |>
  br_set_model(method = 'quasi(variance = "mu", link = "log")') |>
  br_run()




cleanEx()
nameEx("polar_init")
### * polar_init

flush(stderr()); flush(stdout())

### Name: polar_init
### Title: Init a dot plot in polar system
### Aliases: polar_init

### ** Examples

library(ggplot2)
# -------------------
#  Init a polar plot
# -------------------

data <- data.frame(x = LETTERS[1:7])

p1 <- polar_init(data, aes(x = x))
p1

# Set aes value
p2 <- polar_init(data, aes(x = x), size = 3, color = "red", alpha = 0.5)
p2

# Set aes mapping
set.seed(123L)
data1 <- data.frame(
  x = LETTERS[1:7],
  shape = c("r", "r", "r", "b", "b", "b", "b"),
  color = c("r", "r", "r", "b", "b", "b", "b"),
  size = abs(rnorm(7))
)
# Check https://ggplot2.tidyverse.org/reference/geom_point.html
# for how to use both stroke and color
p3 <- polar_init(data1, aes(x = x, size = size, color = color, shape = shape), alpha = 0.5)
p3

# --------------------
#  Connect polar dots
# --------------------
data2 <- data.frame(
  x1 = LETTERS[1:7],
  x2 = c("B", "C", "D", "E", "C", "A", "C"),
  color = c("r", "r", "r", "b", "b", "b", "b")
)
p4 <- p3 + polar_connect(data2, aes(x = x1, xend = x2))
p4

p5 <- p3 + polar_connect(data2, aes(x = x1, xend = x2, color = color), alpha = 0.8, linetype = 2)
p5

# Use two different color scales
if (requireNamespace("ggnewscale")) {
  library(ggnewscale)
  p6 <- p3 +
    new_scale("color") +
    polar_connect(data2, aes(x = x1, xend = x2, color = color), alpha = 0.8, linetype = 2)
  p6 + scale_color_brewer()
  p6 + scale_color_manual(values = c("darkgreen", "magenta"))
}



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
