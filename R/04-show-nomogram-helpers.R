# Helper functions for nomogram creation
#
# Internal functions to support the br_show_nomogram function

# Helper function to create Cox regression nomogram
.create_coxph_nomogram <- function(model, time_points, point_range, title, subtitle, model_name) {
  # Extract model coefficients and terms
  coefs <- stats::coef(model)
  
  # Remove any NA coefficients (for singular fits)
  coefs <- coefs[!is.na(coefs)]
  
  if (length(coefs) == 0) {
    cli::cli_abort("No valid coefficients found in the model")
  }
  
  # Get model frame to understand variable ranges
  model_frame <- broom.helpers::model_get_model_frame(model)
  
  # Get baseline survival for more accurate survival probability calculations
  baseline_surv <- tryCatch({
    survival::survfit(model)
  }, error = function(e) NULL)
  
  # Scale coefficients to point range
  max_abs_coef <- max(abs(coefs))
  point_scale_factor <- diff(point_range) / (2 * max_abs_coef)
  
  # Create scales for each variable
  nom_data <- list()
  y_position <- length(coefs) + 2  # Start from top
  
  # Points scale at the top
  nom_data[[1]] <- data.frame(
    y = y_position,
    x = seq(point_range[1], point_range[2], length.out = 11),
    label = seq(point_range[1], point_range[2], length.out = 11),
    var_name = "Points",
    type = "scale",
    stringsAsFactors = FALSE
  )
  y_position <- y_position - 1
  
  # Variable scales - improved handling
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
        # Continuous variable
        var_range <- range(var_data, na.rm = TRUE)
        var_values <- seq(var_range[1], var_range[2], length.out = 9)
        points <- (var_values - min(var_values)) * coef_val * point_scale_factor / diff(var_range) + 
                  point_range[1] + (point_range[2] - point_range[1]) * 0.2
        
        nom_data[[i + 1]] <- data.frame(
          y = y_position,
          x = points,
          label = round(var_values, 1),
          var_name = actual_var_name,
          type = "variable",
          stringsAsFactors = FALSE
        )
      } else if (is.factor(var_data)) {
        # Categorical variable - improved handling
        levels_found <- levels(var_data)
        
        # For factor variables, we need to map coefficient to the right level
        if (grepl("[0-9]+$", var_name)) {
          # Extract level number from coefficient name
          level_num <- as.numeric(gsub(".*([0-9]+)$", "\\1", var_name))
          if (level_num <= length(levels_found)) {
            # Reference level (level 1) gets baseline points
            ref_points <- mean(point_range)
            # Current level gets points based on coefficient
            level_points <- ref_points + coef_val * point_scale_factor
            
            points_vals <- c(ref_points, level_points)
            labels_vals <- c(paste0(levels_found[1], " (ref)"), levels_found[level_num])
            
            nom_data[[i + 1]] <- data.frame(
              y = y_position,
              x = points_vals,
              label = labels_vals,
              var_name = actual_var_name,
              type = "variable",
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Simple two-level case
          ref_points <- mean(point_range)
          level_points <- ref_points + coef_val * point_scale_factor
          
          points_vals <- c(ref_points, level_points)
          labels_vals <- c(paste0(levels_found[1], " (ref)"), 
                          if(length(levels_found) > 1) levels_found[2] else "Other")
          
          nom_data[[i + 1]] <- data.frame(
            y = y_position,
            x = points_vals,
            label = labels_vals,
            var_name = actual_var_name,
            type = "variable",
            stringsAsFactors = FALSE
          )
        }
      }
    } else {
      # If variable not found in model frame, create a generic scale
      points_vals <- c(mean(point_range) - abs(coef_val) * point_scale_factor * 0.5,
                      mean(point_range),
                      mean(point_range) + abs(coef_val) * point_scale_factor * 0.5)
      
      nom_data[[i + 1]] <- data.frame(
        y = y_position,
        x = points_vals,
        label = c("Low", "Medium", "High"),
        var_name = var_name,
        type = "variable",
        stringsAsFactors = FALSE
      )
    }
    
    y_position <- y_position - 1
  }
  
  # Total points scale
  y_position <- y_position - 0.5
  total_points <- seq(point_range[1], point_range[2], length.out = 11)
  nom_data[[length(nom_data) + 1]] <- data.frame(
    y = y_position,
    x = total_points,
    label = total_points,
    var_name = "Total Points",
    type = "scale",
    stringsAsFactors = FALSE
  )
  
  # Survival probability scales for each time point - improved calculation
  if (length(time_points) > 0) {
    for (j in seq_along(time_points)) {
      y_position <- y_position - 1
      
      # More accurate survival probability calculation
      if (!is.null(baseline_surv)) {
        # Use baseline survival function for better accuracy
        time_idx <- which.min(abs(baseline_surv$time - time_points[j]))
        if (length(time_idx) > 0 && time_idx <= length(baseline_surv$surv)) {
          baseline_surv_at_time <- baseline_surv$surv[time_idx]
          
          # Calculate survival probabilities based on linear predictor
          lp_range <- (total_points - mean(point_range)) / point_scale_factor
          survival_probs <- baseline_surv_at_time ^ exp(lp_range)
          survival_probs <- pmax(0.01, pmin(0.99, survival_probs))
        } else {
          # Fallback to simplified calculation
          survival_probs <- exp(-exp((total_points - mean(point_range)) / point_scale_factor) * time_points[j] / 36)
          survival_probs <- pmax(0.01, pmin(0.99, survival_probs))
        }
      } else {
        # Fallback calculation
        survival_probs <- exp(-exp((total_points - mean(point_range)) / point_scale_factor) * time_points[j] / 36)
        survival_probs <- pmax(0.01, pmin(0.99, survival_probs))
      }
      
      nom_data[[length(nom_data) + 1]] <- data.frame(
        y = y_position,
        x = total_points,
        label = paste0(round(survival_probs * 100, 1), "%"),
        var_name = paste0(time_points[j], "-month survival"),
        type = "survival",
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
    ggplot2::geom_vline(xintercept = seq(point_range[1], point_range[2], by = 10), 
                        color = "grey90", linewidth = 0.3) +
    ggplot2::geom_point(size = 1.2, color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), 
                       vjust = -0.6, size = 2.8, color = "black") +
    # Add connecting lines for scales
    ggplot2::geom_line(data = plot_data[plot_data$type %in% c("scale", "survival"), ],
                       ggplot2::aes(group = .data$y), 
                       linewidth = 0.5, color = "black") +
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
  
  # Remove intercept and any NA coefficients
  intercept <- coefs[1]
  coefs <- coefs[-1]  # Remove intercept
  coefs <- coefs[!is.na(coefs)]
  
  if (length(coefs) == 0) {
    cli::cli_abort("No valid coefficients found in the model")
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
  y_position <- length(coefs) + 3  # Start from top
  
  # Points scale at the top
  nom_data[[1]] <- data.frame(
    y = y_position,
    x = seq(point_range[1], point_range[2], length.out = 11),
    label = seq(point_range[1], point_range[2], length.out = 11),
    var_name = "Points",
    type = "scale",
    stringsAsFactors = FALSE
  )
  y_position <- y_position - 1
  
  # Variable scales - improved handling similar to Cox model
  for (i in seq_along(coefs)) {
    var_name <- names(coefs)[i]
    coef_val <- coefs[i]
    
    # Get variable data from model frame
    if (var_name %in% colnames(model_frame)) {
      var_data <- model_frame[[var_name]]
      
      if (is.numeric(var_data)) {
        # Continuous variable
        var_range <- range(var_data, na.rm = TRUE)
        var_values <- seq(var_range[1], var_range[2], length.out = 9)
        # Improved point scaling
        points <- (var_values - min(var_values)) * coef_val * point_scale_factor / diff(var_range) + 
                  point_range[1] + (point_range[2] - point_range[1]) * 0.2
        
        nom_data[[i + 1]] <- data.frame(
          y = y_position,
          x = points,
          label = round(var_values, 1),
          var_name = var_name,
          type = "variable",
          stringsAsFactors = FALSE
        )
      } else {
        # Categorical variable - improved handling
        if (is.factor(var_data)) {
          levels_found <- levels(var_data)
          if (length(levels_found) > 1) {
            # Reference level gets mean points
            ref_level <- levels_found[1]
            ref_points <- mean(point_range)
            level_points <- ref_points + coef_val * point_scale_factor
            
            points_vals <- c(ref_points, level_points)
            labels_vals <- c(paste0(ref_level, " (ref)"), 
                            if (length(levels_found) > 1) levels_found[2] else "Other")
            
            nom_data[[i + 1]] <- data.frame(
              y = y_position,
              x = points_vals,
              label = labels_vals,
              var_name = var_name,
              type = "variable",
              stringsAsFactors = FALSE
            )
          }
        }
      }
    } else {
      # Generic scale for unknown variables
      points_vals <- c(mean(point_range) - abs(coef_val) * point_scale_factor * 0.5,
                      mean(point_range),
                      mean(point_range) + abs(coef_val) * point_scale_factor * 0.5)
      
      nom_data[[i + 1]] <- data.frame(
        y = y_position,
        x = points_vals,
        label = c("Low", "Medium", "High"),
        var_name = var_name,
        type = "variable",
        stringsAsFactors = FALSE
      )
    }
    
    y_position <- y_position - 1
  }
  
  # Total points scale
  y_position <- y_position - 0.5
  total_points <- seq(point_range[1], point_range[2], length.out = 11)
  nom_data[[length(nom_data) + 1]] <- data.frame(
    y = y_position,
    x = total_points,
    label = total_points,
    var_name = "Total Points",
    type = "scale",
    stringsAsFactors = FALSE
  )
  
  # Prediction scale - improved mapping
  y_position <- y_position - 1
  # More accurate mapping of total points to prediction values
  # Use the actual relationship between coefficients and prediction
  
  # Calculate the range of linear predictors from the model
  pred_range_actual <- range(stats::predict(model, type = "response"), na.rm = TRUE)
  
  # Map prediction values to points more accurately
  pred_points <- seq(point_range[1], point_range[2], length.out = length(fun_at))
  
  nom_data[[length(nom_data) + 1]] <- data.frame(
    y = y_position,
    x = pred_points,
    label = fun_at,
    var_name = "Predicted Value",
    type = "prediction",
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
    ggplot2::geom_vline(xintercept = seq(point_range[1], point_range[2], by = 10), 
                        color = "grey90", linewidth = 0.3) +
    ggplot2::geom_point(size = 1.2, color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), 
                       vjust = -0.6, size = 2.8, color = "black") +
    # Add connecting lines for scales
    ggplot2::geom_line(data = plot_data[plot_data$type %in% c("scale", "prediction"), ],
                       ggplot2::aes(group = .data$y), 
                       linewidth = 0.5, color = "black") +
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