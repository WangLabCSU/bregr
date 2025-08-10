# Helper functions for nomogram creation
#
# Internal functions to support the br_show_nomogram function

# Helper function to create Cox regression nomogram
.create_coxph_nomogram <- function(model, time_points, point_range, title, subtitle, model_name) {
  # Extract model coefficients and terms
  coefs <- stats::coef(model)

  # Handle NA coefficients (for singular fits) while preserving coefficient-term correspondence
  if (any(is.na(coefs))) {
    na_coefs <- names(coefs)[is.na(coefs)]
    cli::cli_inform("removing {length(na_coefs)} NA coefficient{?s} due to singular fit: {.val {na_coefs}}")
    coefs <- coefs[!is.na(coefs)]
  }

  if (length(coefs) == 0) {
    cli::cli_abort("no valid coefficients found in the model")
  }

  # Get model frame to understand variable ranges
  model_frame <- broom.helpers::model_get_model_frame(model)

  # Get baseline survival for more accurate survival probability calculations
  baseline_surv <- tryCatch(
    {
      survival::survfit(model)
    },
    error = function(e) NULL
  )

  # Scale coefficients to point range
  max_abs_coef <- max(abs(coefs))
  point_scale_factor <- diff(point_range) / (2 * max_abs_coef)

  # Create scales for each variable
  nom_data <- list()
  y_position <- length(coefs) + 1 # Start from top (removed redundant Points scale)

  # Variable scales - improved handling with proper line representations
  for (i in seq_along(coefs)) {
    var_name <- names(coefs)[i]
    coef_val <- coefs[i]

    # Try to match coefficient name to original variable
    # Handle factor variables with level suffixes
    base_var_name <- gsub("^(.+?)[0-9]+$", "\\1", var_name)

    # Find the base variable in model frame
    matching_vars <- names(model_frame)[grepl(paste0("^", base_var_name), names(model_frame))]
    if (length(matching_vars) == 0) {
      matching_vars <- names(model_frame)[grepl(var_name, names(model_frame), fixed = TRUE)]
    }

    if (length(matching_vars) > 0) {
      actual_var_name <- matching_vars[1]
      var_data <- model_frame[[actual_var_name]]

      if (is.numeric(var_data)) {
        # Continuous variable - create a proper scale with connecting line
        var_range <- range(var_data, na.rm = TRUE)
        # More points for a smooth connecting line
        n_line_points <- 21
        var_values_line <- seq(var_range[1], var_range[2], length.out = n_line_points)

        # Create evenly spaced x-positions for the variable scale (NOT scaled by coefficient)
        points_line <- seq(point_range[1] + diff(point_range) * 0.1,
          point_range[2] - diff(point_range) * 0.1,
          length.out = n_line_points
        )

        # Create labels and tick marks at meaningful intervals (every 5th point)
        tick_indices <- seq(1, n_line_points, by = 5)
        labels_line <- rep("", n_line_points)
        labels_line[tick_indices] <- round(var_values_line[tick_indices], 1)
        is_tick_line <- rep(FALSE, n_line_points)
        is_tick_line[tick_indices] <- TRUE

        nom_data[[i]] <- data.frame(
          y = y_position,
          x = points_line,
          label = labels_line,
          var_name = actual_var_name,
          type = "variable",
          is_tick = is_tick_line,
          stringsAsFactors = FALSE
        )
      } else if (is.factor(var_data)) {
        # Categorical variable - show line segment between reference and level
        levels_found <- levels(var_data)

        # For factor variables, we need to map coefficient to the right level
        if (grepl("[0-9]+$", var_name)) {
          # Extract level number from coefficient name
          level_num <- as.numeric(gsub(".*([0-9]+)$", "\\1", var_name))
          if (level_num <= length(levels_found)) {
            # Reference level gets baseline points
            ref_points <- point_range[1] + diff(point_range) * 0.2
            # Current level gets points based on coefficient
            level_points <- ref_points + coef_val * point_scale_factor

            # Create line segment between the two points
            n_line_points <- 11
            line_x <- seq(ref_points, level_points, length.out = n_line_points)
            line_labels <- rep("", n_line_points)
            line_labels[1] <- paste0(levels_found[1], " (ref)")
            line_labels[n_line_points] <- levels_found[level_num]
            line_ticks <- rep(FALSE, n_line_points)
            line_ticks[c(1, n_line_points)] <- TRUE

            nom_data[[i]] <- data.frame(
              y = y_position,
              x = line_x,
              label = line_labels,
              var_name = actual_var_name,
              type = "variable",
              is_tick = line_ticks,
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Simple two-level case
          ref_points <- point_range[1] + diff(point_range) * 0.2
          level_points <- ref_points + coef_val * point_scale_factor

          # Create line segment between the two points
          n_line_points <- 11
          line_x <- seq(ref_points, level_points, length.out = n_line_points)
          line_labels <- rep("", n_line_points)
          line_labels[1] <- paste0(levels_found[1], " (ref)")
          line_labels[n_line_points] <- if (length(levels_found) > 1) levels_found[2] else "Other"
          line_ticks <- rep(FALSE, n_line_points)
          line_ticks[c(1, n_line_points)] <- TRUE

          nom_data[[i]] <- data.frame(
            y = y_position,
            x = line_x,
            label = line_labels,
            var_name = actual_var_name,
            type = "variable",
            is_tick = line_ticks,
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      # If variable not found in model frame, create a generic scale
      n_line_points <- 11
      points_vals <- seq(point_range[1] + diff(point_range) * 0.1,
        point_range[2] - diff(point_range) * 0.1,
        length.out = n_line_points
      )
      labels_vals <- rep("", n_line_points)
      labels_vals[c(1, 6, 11)] <- c("Low", "Medium", "High")
      tick_vals <- rep(FALSE, n_line_points)
      tick_vals[c(1, 6, 11)] <- TRUE

      nom_data[[i]] <- data.frame(
        y = y_position,
        x = points_vals,
        label = labels_vals,
        var_name = var_name,
        type = "variable",
        is_tick = tick_vals,
        stringsAsFactors = FALSE
      )
    }

    y_position <- y_position - 1
  }

  # Total points scale with proper line
  y_position <- y_position - 0.5
  n_total_points <- 21 # More points for smoother line
  total_points <- seq(point_range[1], point_range[2], length.out = n_total_points)
  total_labels <- rep("", n_total_points)
  # Show labels every 4th point
  label_indices <- seq(1, n_total_points, by = 4)
  total_labels[label_indices] <- total_points[label_indices]
  total_ticks <- rep(FALSE, n_total_points)
  total_ticks[label_indices] <- TRUE

  nom_data[[length(nom_data) + 1]] <- data.frame(
    y = y_position,
    x = total_points,
    label = total_labels,
    var_name = "Total Points",
    type = "scale",
    is_tick = total_ticks,
    stringsAsFactors = FALSE
  )

  # Survival probability scales for each time point - improved calculation
  if (length(time_points) > 0) {
    for (j in seq_along(time_points)) {
      y_position <- y_position - 1

      # More accurate survival probability calculation
      if (!is.null(baseline_surv)) {
        # Convert months to days for proper time matching
        time_in_days <- time_points[j] * 30.44  # Average days per month
        time_idx <- which.min(abs(baseline_surv$time - time_in_days))

        if (length(time_idx) > 0 && time_idx <= length(baseline_surv$surv)) {
          baseline_surv_at_time <- baseline_surv$surv[time_idx]

          # Calculate survival probabilities based on linear predictor
          # Linear predictor range corresponding to the point range
          lp_range <- (total_points - mean(point_range)) / point_scale_factor
          survival_probs <- baseline_surv_at_time^exp(lp_range)
          survival_probs <- pmax(0.01, pmin(0.99, survival_probs))
        } else {
          # Fallback: use average baseline hazard estimation
          # Convert months to hazard time scale
          hazard_time <- time_points[j] / 12  # Convert to years for hazard calculation
          lp_range <- (total_points - mean(point_range)) / point_scale_factor
          survival_probs <- exp(-0.5 * hazard_time * exp(lp_range))  # More realistic baseline
          survival_probs <- pmax(0.01, pmin(0.99, survival_probs))
        }
      } else {
        # Fallback calculation with more realistic baseline hazard
        hazard_time <- time_points[j] / 12  # Convert to years
        lp_range <- (total_points - mean(point_range)) / point_scale_factor
        survival_probs <- exp(-0.5 * hazard_time * exp(lp_range))  # More realistic baseline
        survival_probs <- pmax(0.01, pmin(0.99, survival_probs))
      }

      # Create survival probability labels with fewer overlapping points
      surv_labels <- rep("", length(total_points))
      surv_labels[label_indices] <- paste0(round(survival_probs[label_indices] * 100, 1), "%")
      surv_ticks <- rep(FALSE, length(total_points))
      surv_ticks[label_indices] <- TRUE

      nom_data[[length(nom_data) + 1]] <- data.frame(
        y = y_position,
        x = total_points,
        label = surv_labels,
        var_name = paste0(time_points[j], "-month survival"),
        type = "survival",
        is_tick = surv_ticks,
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine all data
  plot_data <- do.call(rbind, nom_data)

  # Create the plot with improved styling
  if (is.null(title)) {
    title <- paste("Nomogram for", model_name, "Model")
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    # Add subtle grid lines for easier reading
    ggplot2::geom_vline(
      xintercept = seq(point_range[1], point_range[2], by = 10),
      color = "grey90", linewidth = 0.3
    ) +
    # Add connecting lines for ALL scales
    ggplot2::geom_line(ggplot2::aes(group = .data$y),
      linewidth = 0.6, color = "black"
    ) +
    # Add tick marks only for labeled points
    ggplot2::geom_point(
      data = plot_data[plot_data$is_tick, ],
      size = 1.8, color = "black"
    ) +
    # Add labels only for tick marks, with improved positioning to prevent overlap
    ggplot2::geom_text(
      data = plot_data[plot_data$is_tick & plot_data$label != "", ],
      ggplot2::aes(label = .data$label),
      vjust = -1.2, hjust = 0.5, size = 3, color = "black"
    ) +
    ggplot2::scale_y_continuous(
      breaks = unique(plot_data$y),
      labels = unique(plot_data$var_name)[order(unique(plot_data$y), decreasing = TRUE)],
      limits = c(min(plot_data$y) - 0.5, max(plot_data$y) + 0.5)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(point_range[1] - 5, point_range[2] + 5)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      panel.background = ggplot2::element_rect(fill = "white", color = "black")
    )

  return(p)
}

# Helper function to create linear/GLM nomogram
.create_lm_nomogram <- function(model, fun_at, point_range, title, subtitle, model_name) {
  # Extract model coefficients and terms
  coefs <- stats::coef(model)

  # Check if model has intercept using proper method
  model_terms <- stats::terms(model)
  has_intercept <- attr(model_terms, "intercept") == 1

  # Handle intercept removal if present
  if (has_intercept) {
    intercept <- coefs[1]
    coefs <- coefs[-1] # Remove intercept
  } else {
    cli::cli_inform("model fitted without intercept")
  }

  # Handle NA coefficients (for singular fits) while preserving coefficient-term correspondence
  if (any(is.na(coefs))) {
    na_coefs <- names(coefs)[is.na(coefs)]
    cli::cli_inform("removing {length(na_coefs)} NA coefficient{?s} due to singular fit: {.val {na_coefs}}")
    coefs <- coefs[!is.na(coefs)]
  }

  if (length(coefs) == 0) {
    cli::cli_abort("no valid coefficients found in the model")
  }

  # Get model frame to understand variable ranges
  model_frame <- broom.helpers::model_get_model_frame(model)

  # Set default prediction values if not provided
  if (is.null(fun_at)) {
    pred_range <- range(stats::predict(model), na.rm = TRUE)
    fun_at <- seq(pred_range[1], pred_range[2], length.out = 5)
    fun_at <- round(fun_at, 2)
  }

  # Scale coefficients to point range
  max_abs_coef <- max(abs(coefs))
  point_scale_factor <- diff(point_range) / (2 * max_abs_coef)

  # Create scales for each variable
  nom_data <- list()
  y_position <- length(coefs) + 1 # Start from top (removed redundant Points scale)

  # Variable scales - improved handling similar to Cox model
  for (i in seq_along(coefs)) {
    var_name <- names(coefs)[i]
    coef_val <- coefs[i]

    # Get variable data from model frame
    if (var_name %in% colnames(model_frame)) {
      var_data <- model_frame[[var_name]]

      if (is.numeric(var_data)) {
        # Continuous variable - create proper scale with connecting line
        var_range <- range(var_data, na.rm = TRUE)
        n_line_points <- 21
        var_values <- seq(var_range[1], var_range[2], length.out = n_line_points)

        # Create evenly spaced x-positions for the variable scale (NOT scaled by coefficient)
        points <- seq(point_range[1] + diff(point_range) * 0.1,
          point_range[2] - diff(point_range) * 0.1,
          length.out = n_line_points
        )

        # Create labels at meaningful intervals
        tick_indices <- seq(1, n_line_points, by = 5)
        labels_line <- rep("", n_line_points)
        labels_line[tick_indices] <- round(var_values[tick_indices], 1)
        is_tick_line <- rep(FALSE, n_line_points)
        is_tick_line[tick_indices] <- TRUE

        nom_data[[i]] <- data.frame(
          y = y_position,
          x = points,
          label = labels_line,
          var_name = var_name,
          type = "variable",
          is_tick = is_tick_line,
          stringsAsFactors = FALSE
        )
      } else {
        # Categorical variable - improved handling
        if (is.factor(var_data)) {
          levels_found <- levels(var_data)
          if (length(levels_found) > 1) {
            # Reference level gets baseline points
            ref_level <- levels_found[1]
            ref_points <- point_range[1] + diff(point_range) * 0.2
            level_points <- ref_points + coef_val * point_scale_factor

            # Create line segment
            n_line_points <- 11
            line_x <- seq(ref_points, level_points, length.out = n_line_points)
            line_labels <- rep("", n_line_points)
            line_labels[1] <- paste0(ref_level, " (ref)")
            line_labels[n_line_points] <- if (length(levels_found) > 1) levels_found[2] else "Other"
            line_ticks <- rep(FALSE, n_line_points)
            line_ticks[c(1, n_line_points)] <- TRUE

            nom_data[[i]] <- data.frame(
              y = y_position,
              x = line_x,
              label = line_labels,
              var_name = var_name,
              type = "variable",
              is_tick = line_ticks,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    } else {
      # Generic scale for unknown variables
      n_line_points <- 11
      points_vals <- seq(point_range[1] + diff(point_range) * 0.1,
        point_range[2] - diff(point_range) * 0.1,
        length.out = n_line_points
      )
      labels_vals <- rep("", n_line_points)
      labels_vals[c(1, 6, 11)] <- c("Low", "Medium", "High")
      tick_vals <- rep(FALSE, n_line_points)
      tick_vals[c(1, 6, 11)] <- TRUE

      nom_data[[i]] <- data.frame(
        y = y_position,
        x = points_vals,
        label = labels_vals,
        var_name = var_name,
        type = "variable",
        is_tick = tick_vals,
        stringsAsFactors = FALSE
      )
    }

    y_position <- y_position - 1
  }

  # Total points scale with proper line
  y_position <- y_position - 0.5
  n_total_points <- 21
  total_points <- seq(point_range[1], point_range[2], length.out = n_total_points)
  total_labels <- rep("", n_total_points)
  label_indices <- seq(1, n_total_points, by = 4)
  total_labels[label_indices] <- total_points[label_indices]
  total_ticks <- rep(FALSE, n_total_points)
  total_ticks[label_indices] <- TRUE

  nom_data[[length(nom_data) + 1]] <- data.frame(
    y = y_position,
    x = total_points,
    label = total_labels,
    var_name = "Total Points",
    type = "scale",
    is_tick = total_ticks,
    stringsAsFactors = FALSE
  )

  # Prediction scale - improved mapping
  y_position <- y_position - 1

  # Map prediction values to points more accurately
  n_pred_points <- 21
  pred_points <- seq(point_range[1], point_range[2], length.out = n_pred_points)

  # Create prediction labels at the specified fun_at values
  pred_labels <- rep("", n_pred_points)
  pred_ticks <- rep(FALSE, n_pred_points)

  # Place fun_at values evenly across the prediction scale
  if (length(fun_at) > 0) {
    pred_indices <- round(seq(1, n_pred_points, length.out = length(fun_at)))
    pred_labels[pred_indices] <- fun_at
    pred_ticks[pred_indices] <- TRUE
  }

  nom_data[[length(nom_data) + 1]] <- data.frame(
    y = y_position,
    x = pred_points,
    label = pred_labels,
    var_name = "Predicted Value",
    type = "prediction",
    is_tick = pred_ticks,
    stringsAsFactors = FALSE
  )

  # Combine all data
  plot_data <- do.call(rbind, nom_data)

  # Create the plot with improved styling
  if (is.null(title)) {
    title <- paste("Nomogram for", model_name, "Model")
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
    # Add subtle grid lines for easier reading
    ggplot2::geom_vline(
      xintercept = seq(point_range[1], point_range[2], by = 10),
      color = "grey90", linewidth = 0.3
    ) +
    # Add connecting lines for ALL scales
    ggplot2::geom_line(ggplot2::aes(group = .data$y),
      linewidth = 0.6, color = "black"
    ) +
    # Add tick marks only for labeled points
    ggplot2::geom_point(
      data = plot_data[plot_data$is_tick, ],
      size = 1.8, color = "black"
    ) +
    # Add labels only for tick marks, with improved positioning to prevent overlap
    ggplot2::geom_text(
      data = plot_data[plot_data$is_tick & plot_data$label != "", ],
      ggplot2::aes(label = .data$label),
      vjust = -1.2, hjust = 0.5, size = 3, color = "black"
    ) +
    ggplot2::scale_y_continuous(
      breaks = unique(plot_data$y),
      labels = unique(plot_data$var_name)[order(unique(plot_data$y), decreasing = TRUE)],
      limits = c(min(plot_data$y) - 0.5, max(plot_data$y) + 0.5)
    ) +
    ggplot2::scale_x_continuous(
      limits = c(point_range[1] - 5, point_range[2] + 5)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      panel.background = ggplot2::element_rect(fill = "white", color = "black")
    )

  return(p)
}
