#' Modeling and analysis pipeline
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Provides a set of functions for running batch regression analysis.
#' Combines data setup, model configuration, and execution steps into a single workflow.
#' Supports both GLM and Cox-PH models with options for focal/control terms and parallel processing.
#'
#' @details
#' Please note the difference between [variables](https://easystats.github.io/insight/#variables) and
#' [terms](https://easystats.github.io/insight/#terms),
#' e.g., `x + poly(x, 2)` has *one* variable `x`, but *two* terms `x` and `poly(x, 2)`.
#'
#' ### Global options
#'
#' **bregr** supported global options can be set with `options()`.
#' Currently they are used in `br_run()`.
#'
#' - `bregr.save_model`: If `TRUE`, save model to local disk.
#' - `bregr.path`: A path for save model, default uses a
#' temporary directory.
#'
#' @returns
#' An object of class `breg` with input values added to corresponding slot(s).
#' For `br_run()`, the returned object is a `breg` object with results added to
#' the slots `@results` and `@results_tidy`, note that `@models` is updated to a list
#' of constructed model object (See [accessors]).
#'
#' @name pipeline
#' @param data A `data.frame` containing all necessary variables for analysis.
#' Column names should follow R's naming conventions.
#' @param obj An object of class `breg`.
#' @param y Character vector specifying dependent variables (response variables).
#' For GLM models, this is typically a single character (e.g., `"outcome"`).
#' For Cox-PH models, it should be a length-2 vector in the format `c("time", "status")`.
#' @param x Character vector specifying focal independent terms (predictors).
#' @param x2 Character vector specifying control independent terms (predictor, optional).
#' @param method Method for model construction.
#' A name or a list specifying custom model setting.
#' A string representing a complex method setting is acceptable,
#' e.g., 'quasi(variance = "mu", link = "log")'.
#' Or a list with 4 elements, see [br_avail_method_config()]
#' for examples.
#' @param group_by A string specifying the group by column.
#' @param n_workers,run_parallel Integer, indicating integer number
#' of workers to launch, default is `1L`. When `>1`,
#' run modeling code in parallel in the background.
#' @param ... Additional arguments depending on the called function.
#' - `br_set_x()` for passing focal terms as characters.
#' - `br_set_x2()` for passing control terms as characters.
#' - `br_set_model()` for passing other configurations for modeling.
#' - `br_run()` for passing other configurations for obtaining modeling results with [broom.helpers::tidy_plus_plus()].
#' e.g., The default value for `exponentiate` is `FALSE` (coefficients are not exponentiated).
#' For logistic, and Cox-PH regressions models, `exponentiate` is set to `TRUE` at default.
#' @param model_args A list of arguments passed to `br_set_model()`.
#' @param run_args A list of arguments passed to `br_run()`.
#' @param filter_x Logical, whether to enable pre-filtering of focal variables. Default is `FALSE`.
#' @param filter_na_prop Numeric, maximum proportion of NA values allowed for a variable. Default is `0.8`.
#' @param filter_sd_min Numeric, minimum standard deviation required for a variable. Default is `1e-6`.
#' @param filter_var_min Numeric, minimum variance required for a variable. Default is `1e-6`.
#' @examples
#' library(bregr)
#' # 1. Pipeline -------------------------
#' # 1.1. A single linear model ----------
#' m <- breg(mtcars) |> # set model data
#'   br_set_y("mpg") |> # set dependent variable
#'   br_set_x("qsec") |> # set focal variables
#'   br_set_model("gaussian") |> # set model
#'   br_run() # run analysis
#'
#' # get model tidy result
#' br_get_results(m, tidy = TRUE)
#' # or m@results_tidy
#'
#' # compare with R's built-in function
#' lm(mpg ~ qsec, data = mtcars) |> summary()
#' # 1.2. Batch linear model -------------
#' # control variables are injected in all constructed models
#' # focal variables are injected in constructed models one by one
#' m2 <- breg(mtcars) |>
#'   br_set_y("mpg") |>
#'   br_set_x(colnames(mtcars)[2:4]) |> # set focal variables
#'   br_set_x2("vs") |> # set control variables
#'   br_set_model("gaussian") |>
#'   br_run()
#' # 1.3. Group by model -------------
#' m3 <- breg(mtcars) |>
#'   br_set_y("mpg") |>
#'   br_set_x("cyl") |>
#'   br_set_x2("wt") |> # set control variables
#'   br_set_model("gaussian") |>
#'   br_run(group_by = "am")
#'
#' # 2. All-in-one pipeline wrapper ---
#' m4 <- br_pipeline(mtcars,
#'   y = "mpg",
#'   x = colnames(mtcars)[2:4],
#'   x2 = "vs",
#'   method = "gaussian"
#' )
#'
#' # 3. Customized model -----------
#' \dontrun{
#' dt <- data.frame(x = rnorm(100))
#' dt$y <- rpois(100, exp(1 + dt$x))
#'
#' m5 <- breg(dt) |>
#'   br_set_y("y") |>
#'   br_set_x("x") |>
#'   br_set_model(method = 'quasi(variance = "mu", link = "log")') |>
#'   br_run()
#' }
#'
#' @testexamples
#' assert_breg_obj(m)
#' assert_breg_obj(m2)
#' assert_breg_obj(m3)
#' assert_breg_obj(m4)
#' @seealso [accessors] for accessing `breg` object properties.
NULL

#' @describeIn pipeline All-in-one end to end wrapper to run the regression analysis in batch.
#' Which could be splitted into the following steps
#' @export
br_pipeline <- function(
    data, y, x, method, x2 = NULL,
    group_by = NULL,
    n_workers = 1L, run_parallel = lifecycle::deprecated(),
    model_args = list(),
    run_args = list(),
    filter_x = FALSE,
    filter_na_prop = 0.8,
    filter_sd_min = 1e-6,
    filter_var_min = 1e-6,
    filter_min_levels = 2) {
  if (lifecycle::is_present(run_parallel)) {
    lifecycle::deprecate_warn("1.1.0", "bregr::br_run(run_parallel = )", "bregr::br_run(n_workers = )")
    n_workers <- run_parallel
  }

  breg(data) |>
    br_set_y(y) |>
    br_set_x(x,
      filter_x = filter_x, filter_na_prop = filter_na_prop,
      filter_sd_min = filter_sd_min, filter_var_min = filter_var_min,
      filter_min_levels = filter_min_levels
    ) |>
    br_set_x2(x2) |>
    br_set_model(method = method, !!!model_args) |>
    br_run(
      group_by = group_by, n_workers = n_workers,
      !!!run_args
    )
}

#' @describeIn pipeline Set dependent variables for model construction.
#' @export
br_set_y <- function(obj, y) {
  assert_breg_obj(obj)
  assert_character(y)

  data <- br_get_data(obj)
  if (nrow(data) == 0) {
    cli_abort("cannot set {.arg y} for {.arg obj} with void data")
  } else {
    .in <- y %in% colnames(data)
    if (!all(.in)) {
      cli_abort("column(s) {.val {y[!.in]}} specified in {.arg y} not in {.field data} (columns: {.val {colnames(data)}}) of {.arg obj}")
    }
  }

  obj@y <- y
  obj
}

#' @describeIn pipeline Set focal terms for model construction.
#' @param filter_x Logical, whether to enable pre-filtering of focal variables. Default is `FALSE`.
#' @param filter_na_prop Numeric, maximum proportion of NA values allowed for a variable. Default is `0.8`.
#' @param filter_sd_min Numeric, minimum standard deviation required for a variable. Default is `1e-6`.
#' @param filter_var_min Numeric, minimum variance required for a variable. Default is `1e-6`.
#' @param filter_min_levels Numeric, minimum number of unique levels required for categorical variables. Default is `2`.
#' @export
br_set_x <- function(obj, ...,
                     filter_x = FALSE,
                     filter_na_prop = 0.8,
                     filter_sd_min = 1e-6,
                     filter_var_min = 1e-6,
                     filter_min_levels = 2) {
  assert_breg_obj(obj)
  data <- br_get_data(obj)
  col_names <- colnames(data)
  if (nrow(data) == 0) {
    cli_abort("cannot set {.arg x} for {.arg obj} with void data")
  }

  x <- rlang::list2(...) |>
    unlist() |>
    as.character()
  assert_character(x, allow_na = FALSE)

  x <- repair_names(x, col_names)

  # Apply variable filtering if enabled
  if (filter_x) {
    assert_logical(filter_x, allow_na = FALSE)
    assert_number_decimal(filter_na_prop, min = 0, max = 1)
    assert_number_decimal(filter_sd_min, min = 0)
    assert_number_decimal(filter_var_min, min = 0)
    assert_number_whole(filter_min_levels, min = 1)

    filter_result <- filter_variables_x(
      data, x,
      filter_na_prop = filter_na_prop,
      filter_sd_min = filter_sd_min,
      filter_var_min = filter_var_min,
      filter_min_levels = filter_min_levels
    )
    x <- filter_result$filtered_x

    # Inform user about filtering results
    if (filter_result$filter_summary$filtered > 0) {
      cli::cli_inform(
        "Pre-filtering removed {filter_result$filter_summary$filtered} out of {filter_result$filter_summary$total} focal variables ({round(filter_result$filter_summary$prop_filtered * 100, 1)}%)"
      )
      if (length(filter_result$filtered_out) <= 10) {
        cli::cli_inform("Filtered variables: {.val {filter_result$filtered_out}}")
      } else {
        cli::cli_inform("Filtered variables (showing first 10): {.val {filter_result$filtered_out[1:10]}} ...")
      }
    } else {
      cli::cli_inform("Pre-filtering: no variables were filtered out")
    }
  }

  x_ <- get_vars(x)
  .in <- x_ %in% col_names
  if (!all(.in)) {
    cli_abort("column(s) {.val {x_[!.in]}} specified in {.arg x} not in {.field data} (columns: {.val col_names}}) of {.arg obj}")
  }

  obj@x <- x
  obj
}

#' @describeIn pipeline  Set control terms for model construction (Optional in pipeline).
#' @export
br_set_x2 <- function(obj, ...) {
  assert_breg_obj(obj)
  data <- br_get_data(obj)
  col_names <- colnames(data)
  if (nrow(data) == 0) {
    cli_abort("cannot set {.arg x} for {.arg obj} with void data")
  }

  x <- br_get_x(obj)
  if (is.null(x)) {
    cli_abort("{.fn br_set_x2()} should be called after {.fn br_set_x()}")
  }

  x2 <- rlang::list2(...) |>
    unlist() |>
    as.character()
  assert_character(x2, allow_na = FALSE, allow_null = TRUE)
  assert_not_overlap(x, x2)

  if (length(x2) > 0) x2 <- repair_names(x2, col_names)
  x2_ <- get_vars(x2)

  .in <- x2_ %in% col_names
  if (!all(.in)) {
    cli_abort("column(s) {.val {x2_[!.in]}} specified in {.arg x2} not in {.field data} (columns: {.val {col_names}}) of {.arg obj}")
  }

  obj@x2 <- x2
  obj
}


#' @describeIn pipeline Set model configurations.
#' @export
br_set_model <- function(obj, method, ...) {
  assert_breg_obj(obj)
  if (!rlang::is_list(method)) {
    assert_string(method, allow_empty = FALSE)

    # e.g., 'quasi(variance = "mu", link = "log")'
    if (!grepl("\\(", method)) {
      rlang::arg_match0(method, br_avail_methods())
    }

    # mapped to predefined model setting
    method2 <- br_avail_method_config(method)
  } else {
    if (!rlang::is_list(method, n = 4)) {
      cli::cli_abort("{.arg method} should be a list with 4 elements: {.field f_call}, {.field f_cnst_y}, {.field args_method}, and {.field args_data}, check {.fn br_avail_method_config} for examples")
    }
    assert_string(method$f_call, allow_empty = FALSE)
    if (grepl("::", method$f_call)) {
      pkg <- str_remove(method$f_call, "::.*")
      rlang::check_installed(pkg)
    }
    method2 <- method
  }

  config <- rlang::list2(...)
  config_text <- gsub(
    "^list\\(|\\)$", "",
    paste(deparse(config, width.cutoff = 500),
      collapse = ""
    )
  )

  models <- gen_template(
    obj@y, obj@x, obj@x2,
    method2$f_call,
    method2$f_cnst_y,
    method2$args_method,
    paste0(
      method2$args_data,
      ", ",
      config_text
    )
  ) |>
    as.list()

  names(models) <- obj@x
  obj@config <- list(method = method, extra = config_text)
  obj@models <- models
  obj
}

#' @describeIn pipeline Run the regression analysis in batch.
#' @export
br_run <- function(obj, ...,
                   group_by = NULL, n_workers = 1L,
                   run_parallel = lifecycle::deprecated()) {
  assert_breg_obj(obj)
  assert_character(group_by, allow_na = FALSE, allow_null = TRUE)
  if (br_get_n_x(obj) < 1) {
    cli::cli_abort("{.fn br_set_x} must run before {.fn br_run}")
  }
  if (length(br_get_y(obj)) < 1) {
    cli::cli_abort("{.fn br_set_y} must run before {.fn br_run}")
  }
  on.exit(invisible(gc()))

  if (lifecycle::is_present(run_parallel)) {
    # Signal the deprecation to the user
    lifecycle::deprecate_warn("1.1.0", "bregr::br_run(run_parallel = )", "bregr::br_run(n_workers = )")
    # Deal with the deprecated argument for compatibility
    n_workers <- run_parallel
  }
  assert_number_whole(n_workers, min = 1)
  if (n_workers > 1 && n_workers > parallel::detectCores() * 5) {
    cli::cli_abort("a large worker number {n_workers} set for {.arg n_workers} (>{.code parallel::detectCores()*5})")
  }

  if (n_workers > 1) {
    if (obj@n_x < 100) {
      cli::cli_warn("running in parallel is typically not recommended for small number (<100) of focal terms")
    }
  }

  if (!is.null(group_by)) {
    assert_not_overlap(group_by, obj@x,
      msg = "{.arg group_by} variables should not overlap with modeling (focal) variables"
    )
    assert_not_overlap(group_by, obj@x2,
      msg = "{.arg group_by} variables should not overlap with modeling (control) variables"
    )
  }

  ms <- br_get_models(obj)
  config <- br_get_config(obj)
  dots <- rlang::list2(...)

  # For br_avail_methods_use_exp(), `exponentiate` is typically set to `TRUE`.
  exponentiate <- FALSE
  if (!"exponentiate" %in% names(dots)) {
    if (rlang::is_string(config$method) && config$method %in% br_avail_methods_use_exp()) {
      dots[["exponentiate"]] <- TRUE
      cli_inform("exponentiate estimates of model(s) constructed from {.field {config$method}} method at default")
    } else {
      dots[["exponentiate"]] <- FALSE
    }
  }
  exponentiate <- dots[["exponentiate"]]

  if (is.null(group_by)) {
    res <- runner(ms, obj@data, dots, obj@y, obj@x, obj@x2, n_workers)
  } else {
    obj@group_by <- group_by
    data_split <- obj@data |>
      named_group_split(obj@data[, group_by, drop = FALSE])
    data_split[["All"]] <- obj@data
    res_list <- map(data_split, function(data) {
      runner(ms, data, dots, obj@y, obj@x, obj@x2, n_workers, group_by)
    })
    res <- list_transpose(res_list)
    res$models <- purrr::list_flatten(res$models)
    res$results <- vctrs::vec_rbind(!!!res$results, .names_to = "Group_variable")
    res$results_tidy <- vctrs::vec_rbind(!!!res$results_tidy, .names_to = "Group_variable")
  }

  obj@models <- res$models
  obj@results <- res$results
  obj@results_tidy <- res$results_tidy
  attr(obj, "exponentiate") <- exponentiate
  obj
}

runner <- function(ms, data, dots, y, x, x2, n_workers, group_by = NULL) {
  on.exit(invisible(gc()))
  # Subset data to only necessary columns before model construction
  necessary_cols <- get_necessary_columns(y, x, x2, group_by, colnames(data))
  data <- data[, necessary_cols, drop = FALSE]

  if (n_workers > 1) {
    res <- with(
      mirai::daemons(n_workers),
      {
        mp <- mirai::mirai_map(
          ms, runner_,
          .args = list(
            data = data, dots = dots,
            opts = options()
          )
        )
        mp[.progress]
      }
    )
  } else {
    res <- map(
      ms, runner_,
      data = data, dots = dots,
      opts = options()
    )
  }

  res <- list_transpose(res)
  models <- res$model
  results <- vctrs::vec_rbind(!!!res$result, .names_to = "Focal_variable")
  results_tidy <- vctrs::vec_rbind(!!!res$result_tidy, .names_to = "Focal_variable")

  list(
    models = models,
    results = results,
    results_tidy = results_tidy
  )
}

runner_ <- function(m, data, dots, opts = NULL) {
  on.exit(invisible(gc()))
  model_vars <- get_vars(m)
  necessary_cols <- model_vars[model_vars %in% colnames(data)]
  data <- data[, necessary_cols, drop = FALSE]
  options(opts)
  # m: model template
  # data: data frame for modeling
  # dots: arguments passing to parse model parameters
  # x: focal variables
  model <- rlang::try_fetch(
    rlang::eval_bare(rlang::parse_expr(m)),
    error = function(e) {
      cli::cli_inform("modeling failed for expression: {.code {m}}")
      cli::cli_warn(e$message)
      NULL
    }
  )
  if (is.null(model)) {
    return(list(model = NULL, result = NULL, result_tidy = NULL))
  }

  # NOTE:
  # broom.helpers::model_* funs
  # when weights were assigned to observations
  # the number of observations will be multiplied
  # see: https://github.com/larmarange/broom.helpers/blob/210cc945bd6a462148a358f8d4851e0d16d208e3/R/model_get_n.R#L96

  # Get comprehensive result for models
  result <- do.call(
    broom.helpers::tidy_plus_plus,
    args = vctrs::vec_c(
      list(model), dots,
      if ("interaction_sep" %in% names(dots)) {
        list(interaction_sep = dots[["interaction_sep"]])
      } else {
        list(interaction_sep = ":")
      }
    )
  )

  # Get tidy result for models
  # output conf.int while exponentiate depends on dots
  result_tidy <- do.call(
    broom::tidy,
    args = vctrs::vec_c(
      list(model), list(conf.int = TRUE),
      if ("exponentiate" %in% names(dots)) {
        list(exponentiate = dots[["exponentiate"]])
      } else {
        list()
      },
      if ("conf.level" %in% names(dots)) {
        list(conf.level = dots[["conf.level"]])
      } else {
        list()
      }
    )
  )

  if (!("intercept" %in% names(dots) && isTRUE(dots[["intercept"]]))) {
    result_tidy <- result_tidy |> dplyr::filter(!.data$term %in% "(Intercept)")
  }

  if (isTRUE(as.logical(getOption("bregr.save_model", default = FALSE)))) {
    rlang::check_installed(c("fs", "ids", "qs"))
    md_path <- getOption("bregr.path", default = "")
    if (md_path == "") {
      md_path <- fs::path_temp()
    }
    cli::cli_inform("model save is enabled with result path {md_path}",
      .frequency = "once", .frequency_id = md_path
    )
    fs::dir_create(md_path)
    dg <- suppressWarnings(ids::uuid(1, use_time = TRUE))
    if (!rlang::is_string(dg)) {
      cli::cli_abort(
        "failed to generate uuid for file, please check"
      )
    }
    md_file <- fs::path(md_path, dg, ext = "qs")
    qs::qsave(model, file = md_file)
    model <- md_file
  }

  list(model = model, result = result, result_tidy = result_tidy)
}
