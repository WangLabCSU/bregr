# Model diagnostics utilities
#
# Provides functions for model diagnostics including proportional hazards
# assumption testing for Cox models using Schoenfeld residuals, and other
# general model diagnostic capabilities.
# =====================


#' Test proportional hazards assumption for Cox models
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function tests the proportional hazards assumption for Cox regression
#' models using Schoenfeld residuals. This is a key diagnostic for Cox
#' proportional hazards models.
#'
#' @param breg A regression object with results (must pass `assert_breg_obj_with_results()`).
#' @param idx Index or name (focal variable) of the Cox model to test. If `NULL`, 
#' tests all Cox models in the breg object.
#' @param transform Character string specifying how to transform time for the test.
#' Options are "km" (Kaplan-Meier), "rank", "identity", or a function.
#' @param ... Additional arguments passed to [survival::cox.zph()].
#' @returns A list containing test results for each Cox model tested.
#' @noRd
#' @family br_diagnose
#' @examples
#' \dontrun{
#' # Create Cox models
#' mds <- br_pipeline(
#'   survival::lung,
#'   y = c("time", "status"),
#'   x = colnames(survival::lung)[6:10],
#'   x2 = c("age", "sex"),
#'   method = "coxph"
#' )
#' 
#' # Test proportional hazards assumption
#' ph_tests <- br_test_ph(mds)
#' print(ph_tests)
#' }
#' @testexamples
#' expect_true(TRUE)
br_test_ph <- function(breg, idx = NULL, transform = "km", ...) {
  assert_breg_obj_with_results(breg)
  
  # Get all models if idx is NULL
  if (is.null(idx)) {
    models <- br_get_models(breg)
  } else {
    models <- br_get_models(breg, idx)
    if (!is.list(models)) {
      # Single model case - convert to list with proper naming
      all_models <- br_get_models(breg)
      model_name <- if (is.character(idx)) {
        idx
      } else {
        names(all_models)[idx]
      }
      models <- list(models)
      names(models) <- model_name
    }
  }
  
  # Filter for Cox models only
  cox_models <- list()
  for (i in seq_along(models)) {
    model <- models[[i]]
    if (inherits(model, "coxph")) {
      cox_models[[names(models)[i]]] <- model
    }
  }
  
  if (length(cox_models) == 0) {
    warning("No Cox proportional hazards models found in the breg object.")
    return(NULL)
  }
  
  # Test proportional hazards assumption for each Cox model
  ph_results <- list()
  for (i in seq_along(cox_models)) {
    model_name <- names(cox_models)[i]
    model <- cox_models[[i]]
    
    tryCatch({
      ph_test <- survival::cox.zph(model, transform = transform, ...)
      ph_results[[model_name]] <- ph_test
    }, error = function(e) {
      warning(paste("Failed to test proportional hazards assumption for model", model_name, ":", e$message))
      ph_results[[model_name]] <- NULL
    })
  }
  
  # Add class for custom printing
  class(ph_results) <- c("br_ph_test", "list")
  ph_results
}


#' Diagnose regression models
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' General diagnostic function that performs appropriate diagnostics based on
#' the model type. For Cox models, tests proportional hazards assumption.
#' For other models, provides general diagnostic information.
#'
#' @param breg A regression object with results (must pass `assert_breg_obj_with_results()`).
#' @param idx Index or name (focal variable) of the model(s) to diagnose. If `NULL`, 
#' diagnoses all models in the breg object.
#' @param ... Additional arguments passed to specific diagnostic functions.
#' @returns A list containing diagnostic results for each model.
#' @noRd
#' @family br_diagnose
#' @examples
#' \dontrun{
#' # Create models
#' mds <- br_pipeline(
#'   survival::lung,
#'   y = c("time", "status"),
#'   x = colnames(survival::lung)[6:10],
#'   x2 = c("age", "sex"),
#'   method = "coxph"
#' )
#' 
#' # Diagnose models
#' diagnostics <- br_diagnose(mds)
#' print(diagnostics)
#' }
#' @testexamples
#' expect_true(TRUE)
br_diagnose <- function(breg, idx = NULL, ...) {
  assert_breg_obj_with_results(breg)
  
  # Get all models if idx is NULL
  if (is.null(idx)) {
    models <- br_get_models(breg)
  } else {
    models <- br_get_models(breg, idx)
    if (!is.list(models)) {
      # Single model case - convert to list with proper naming
      all_models <- br_get_models(breg)
      model_name <- if (is.character(idx)) {
        idx
      } else {
        names(all_models)[idx]
      }
      models <- list(models)
      names(models) <- model_name
    }
  }
  
  diagnostic_results <- list()
  
  for (i in seq_along(models)) {
    model_name <- names(models)[i]
    model <- models[[i]]
    
    # Cox models: test proportional hazards
    if (inherits(model, "coxph")) {
      # Test PH assumption directly on the model instead of going through br_test_ph
      tryCatch({
        ph_test <- survival::cox.zph(model)
        diagnostic_results[[model_name]] <- list(
          model_type = "coxph",
          ph_test = ph_test,
          summary = list(
            n = model$n,
            events = model$nevent,
            loglik = model$loglik
          )
        )
      }, error = function(e) {
        diagnostic_results[[model_name]] <- list(
          model_type = "coxph",
          ph_test = NULL,
          summary = list(
            n = model$n,
            events = model$nevent,
            loglik = model$loglik
          )
        )
      })
    } 
    # GLM models: general diagnostics
    else if (inherits(model, "glm")) {
      diagnostic_results[[model_name]] <- list(
        model_type = "glm",
        family = model$family$family,
        deviance = model$deviance,
        aic = model$aic,
        summary = list(
          n = nobs(model),
          df_residual = model$df.residual
        )
      )
    }
    # LM models: general diagnostics  
    else if (inherits(model, "lm")) {
      diagnostic_results[[model_name]] <- list(
        model_type = "lm",
        r_squared = summary(model)$r.squared,
        adj_r_squared = summary(model)$adj.r.squared,
        summary = list(
          n = nobs(model),
          df_residual = model$df.residual
        )
      )
    }
    # Other models: basic info
    else {
      diagnostic_results[[model_name]] <- list(
        model_type = class(model)[1],
        summary = list(
          available = "Basic diagnostics not yet implemented for this model type"
        )
      )
    }
  }
  
  # Add class for custom printing
  class(diagnostic_results) <- c("br_diagnostics", "list")
  diagnostic_results
}


#' Print method for proportional hazards test results
#'
#' @param x A `br_ph_test` object returned by [br_test_ph()].
#' @param ... Additional arguments (currently unused).
#' @noRd
print.br_ph_test <- function(x, ...) {
  cli::cli_h1("Proportional Hazards Assumption Tests")
  
  if (length(x) == 0) {
    cli::cli_alert_info("No test results available.")
    return(invisible(x))
  }
  
  for (i in seq_along(x)) {
    model_name <- names(x)[i]
    test_result <- x[[i]]
    
    if (is.null(test_result)) {
      cli::cli_alert_warning("Test failed for model: {.val {model_name}}")
      next
    }
    
    cli::cli_h2("Model: {.val {model_name}}")
    
    # Print the test table
    test_table <- test_result$table
    cli::cli_text("Schoenfeld Residuals Test:")
    
    # Format the table for display
    df_names <- rownames(test_table)
    for (j in seq_len(nrow(test_table))) {
      var_name <- df_names[j]
      chisq <- round(test_table[j, "chisq"], 3)
      df <- test_table[j, "df"]
      p_value <- format.pval(test_table[j, "p"], digits = 3)
      
      status_symbol <- if (test_table[j, "p"] < 0.05) "✗" else "✓"
      cli::cli_text("  {status_symbol} {var_name}: χ² = {chisq}, df = {df}, p = {p_value}")
    }
    
    # Global test
    global_p <- format.pval(test_result$table["GLOBAL", "p"], digits = 3)
    global_status <- if (test_result$table["GLOBAL", "p"] < 0.05) "✗ VIOLATED" else "✓ SATISFIED"
    cli::cli_text("  Global test: p = {global_p} - Assumption {global_status}")
    cli::cli_text("")
  }
  
  invisible(x)
}


#' Print method for general diagnostic results
#'
#' @param x A `br_diagnostics` object returned by [br_diagnose()].
#' @param ... Additional arguments (currently unused).
#' @noRd
print.br_diagnostics <- function(x, ...) {
  cli::cli_h1("Model Diagnostics Summary")
  
  if (length(x) == 0) {
    cli::cli_alert_info("No diagnostic results available.")
    return(invisible(x))
  }
  
  for (i in seq_along(x)) {
    model_name <- names(x)[i]
    diag_result <- x[[i]]
    
    cli::cli_h2("Model: {.val {model_name}} ({diag_result$model_type})")
    
    # Type-specific diagnostics
    if (diag_result$model_type == "coxph") {
      cli::cli_text("Sample size: {diag_result$summary$n}")
      cli::cli_text("Events: {diag_result$summary$events}")
      cli::cli_text("Log-likelihood: {round(diag_result$summary$loglik[2], 3)}")
      
      if (!is.null(diag_result$ph_test)) {
        global_p <- diag_result$ph_test$table["GLOBAL", "p"]
        ph_status <- if (global_p < 0.05) "✗ VIOLATED" else "✓ SATISFIED"
        cli::cli_text("Proportional Hazards: {ph_status} (p = {format.pval(global_p, digits = 3)})")
      }
    } else if (diag_result$model_type == "glm") {
      cli::cli_text("Family: {diag_result$family}")
      cli::cli_text("Sample size: {diag_result$summary$n}")
      cli::cli_text("Deviance: {round(diag_result$deviance, 3)}")
      cli::cli_text("AIC: {round(diag_result$aic, 3)}")
    } else if (diag_result$model_type == "lm") {
      cli::cli_text("Sample size: {diag_result$summary$n}")
      cli::cli_text("R-squared: {round(diag_result$r_squared, 3)}")
      cli::cli_text("Adjusted R-squared: {round(diag_result$adj_r_squared, 3)}")
    } else {
      cli::cli_text("Info: {diag_result$summary$available}")
    }
    
    cli::cli_text("")
  }
  
  invisible(x)
}