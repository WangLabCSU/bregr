# Model diagnostics utilities
#
# Provides functions for model diagnostics including proportional hazards
# assumption testing for Cox models using Schoenfeld residuals, and other
# general model diagnostic capabilities.
# =====================


# br_test_ph functionality merged into br_diagnose


#' Diagnose regression models
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Universal diagnostic function that performs appropriate diagnostics based on
#' the model type. For Cox models, tests proportional hazards assumption using
#' Schoenfeld residuals and provides comprehensive Cox diagnostics.
#' For other models, provides general diagnostic information.
#'
#' @param breg A regression object with results (must pass `assert_breg_obj_with_results()`).
#' @param idx Index or name (focal variable) of the model(s) to diagnose. If `NULL`,
#' diagnoses all models in the breg object.
#' @param transform Character string specifying how to transform time for Cox PH tests.
#' Options are "km" (Kaplan-Meier), "rank", "identity", or a function.
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
#' # Diagnose models (includes PH testing for Cox models)
#' diagnostics <- br_diagnose(mds)
#' print(diagnostics)
#' }
#' @testexamples
#' expect_true(TRUE)
br_diagnose <- function(breg, idx = NULL, transform = "km", ...) {
  assert_breg_obj_with_results(breg)

  # Get models based on idx
  if (is.null(idx)) {
    # Get all models
    models <- br_get_models(breg)
  } else {
    # Get specific model(s)
    all_models <- br_get_models(breg)
    if (is.character(idx)) {
      # Get by name
      if (idx %in% names(all_models)) {
        models <- list(all_models[[idx]])
        names(models) <- idx
      } else {
        cli::cli_warn("Model {idx} not found")
        return(NULL)
      }
    } else {
      # Get by index
      if (idx <= length(all_models)) {
        models <- list(all_models[[idx]])
        names(models) <- names(all_models)[idx]
      } else {
        cli::cli_warn("Index {idx} out of range")
        return(NULL)
      }
    }
  }

  diagnostic_results <- list()

  for (i in seq_along(models)) {
    model_name <- names(models)[i]
    model <- models[[i]]

    # Cox models: comprehensive diagnostics including PH testing
    if (inherits(model, "coxph")) {
      # Test PH assumption using Schoenfeld residuals
      ph_test <- NULL
      tryCatch(
        {
          ph_test <- survival::cox.zph(model, transform = transform, ...)
        },
        error = function(e) {
          cli::cli_warn("Failed to test proportional hazards assumption for model {model_name}: {e$message}")
        }
      )

      # Get concordance index if available
      concordance <- NULL
      tryCatch(
        {
          concordance <- model$concordance
        },
        error = function(e) {
          # Concordance might not be available in older survival versions
        }
      )

      diagnostic_results[[model_name]] <- list(
        model_type = "coxph",
        ph_test = ph_test,
        concordance = concordance,
        summary = list(
          n = model$n,
          events = model$nevent,
          loglik = model$loglik,
          lr_test = if (!is.null(model$score) && is.numeric(model$score)) {
            tryCatch(
              {
                list(
                  statistic = model$score,
                  p_value = 1 - pchisq(model$score, model$df)
                )
              },
              error = function(e) NULL
            )
          } else {
            NULL
          }
        )
      )
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


# Print method for br_ph_test removed - functionality merged into br_diagnose


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

      # Show likelihood ratio test if available
      if (!is.null(diag_result$summary$lr_test)) {
        lr_p <- format.pval(diag_result$summary$lr_test$p_value, digits = 3)
        cli::cli_text("LR test: χ² = {round(diag_result$summary$lr_test$statistic, 3)}, p = {lr_p}")
      }

      # Show concordance index if available
      if (!is.null(diag_result$concordance)) {
        if (is.list(diag_result$concordance)) {
          cli::cli_text("Concordance: {round(diag_result$concordance$concordance, 3)}")
        } else {
          cli::cli_text("Concordance: {round(diag_result$concordance, 3)}")
        }
      }

      # Proportional hazards test results
      if (!is.null(diag_result$ph_test)) {
        cli::cli_text("")
        cli::cli_text("Proportional Hazards Test (Schoenfeld Residuals):")

        test_table <- diag_result$ph_test$table
        df_names <- rownames(test_table)

        # Individual variable tests
        for (j in seq_len(nrow(test_table) - 1)) { # Exclude GLOBAL row
          var_name <- df_names[j]
          chisq <- round(test_table[j, "chisq"], 3)
          df <- test_table[j, "df"]
          p_value <- format.pval(test_table[j, "p"], digits = 3)

          status_symbol <- if (test_table[j, "p"] < 0.05) "x" else "+"
          cli::cli_text("  {status_symbol} {var_name}: χ² = {chisq}, df = {df}, p = {p_value}")
        }

        # Global test
        global_p <- format.pval(test_table["GLOBAL", "p"], digits = 3)
        global_status <- if (test_table["GLOBAL", "p"] < 0.05) "x VIOLATED" else "+ SATISFIED"
        cli::cli_text("  Global test: p = {global_p} - Assumption {global_status}")
      } else {
        cli::cli_text("Proportional Hazards: Test failed")
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
