#' Show connected risk network plot
#'
#' @description
#' `r lifecycle::badge('stable')`
#' @inheritParams br_show_forest
#' @param ... Arguments passing to [br_get_results()] for subsetting data table.
#' @export
#' @returns A plot
#' @family br_show
#' @family risk_network
#' @examples
#' lung <- survival::lung
#' # Cox-PH regression
#' mod_surv <- br_pipeline(
#'   data = lung,
#'   y = c("time", "status"),
#'   x = c("age", "ph.ecog", "ph.karno"),
#'   x2 = c("factor(sex)"),
#'   method = "coxph"
#' )
#' p <- br_show_risk_network(mod_surv)
#' p
#' @testexamples
#' assert_s3_class(p, "ggplot")
br_show_risk_network <- function(breg, ...) {
  assert_breg_obj_with_results(breg)
  rlang::check_installed("ggnewscale")

  data <- br_get_data(breg)
  x <- br_get_x(breg)
  method <- br_get_config(breg)$method
  exponentiate <- attr(breg, "exponentiate")
  if (insight::model_name(br_get_models(breg, 1)) != "coxph") {
    cli_abort("this function is designed for Cox-PH model analysis")
  }
  cli_inform("please note only continuous focal terms analyzed and visualized")

  # 1. Obtain regression results
  result <- br_get_results(breg, tidy = FALSE, ...)
  data_reg <- result |>
    dplyr::filter(
      .data$Focal_variable == .data$variable,
      .data$var_type == "continuous"
    )
  if ("Group_variable" %in% result && length(unique(data_reg$Group_variable)) > 1) {
    cli_abort("this function cannot be used for analyzing data with multiple group variable after filtering")
  }
  if (isFALSE(exponentiate)) {
    # when user set exponentiate=FALSE in br_run()
    data_reg$estimate <- exp(data_reg$estimate)
  }

  data_reg$role <- dplyr::case_when(
    data_reg$p.value > 0.05 ~ "non-signf",
    data_reg$estimate < 1 ~ "protector",
    data_reg$estimate > 1 ~ "risker"
  )
  data_reg$`-log10(p)` <- -log10(data_reg$p.value)
  data_reg2 <- data_reg[, c("Focal_variable", "n_obs", "estimate", "role", "-log10(p)")]

  # TODO: support factors with model matrix? broom.helpers::model_get_model_matrix(breg@models[[1]])?

  # 2. Correlation analysis
  vars_comb <- combn(x |> get_vars(), 2, simplify = FALSE)
  cor_value <- rlang::try_fetch(
    sapply(vars_comb, function(x) {
      cor(data[[x[1]]], data[[x[2]]], use = "pairwise")
    }),
    error = function(e) {
      cli_abort("failed due to error: {e$message}, please check your data format")
    }
  )


  data_cor <- cbind(as.data.frame(t(sapply(vars_comb, function(x) x))), cor_value)
  colnames(data_cor) <- c("var1", "var2", "correlation")
  data_cor$linewidth <- abs(data_cor$correlation)
  data_cor$way <- ifelse(data_cor$correlation > 0, "positive", "negative")
  data_cor

  # 3. Visualization
  p <- polar_init(
    data_reg2,
    ggplot2::aes(
      x = .data$Focal_variable,
      color = .data$role, size = .data$`-log10(p)`
    )
  ) + ggplot2::scale_color_manual(values = c("grey", "blue", "red")) +
    labs(size = "-log10(p)", color = "risk type") +
    ggnewscale::new_scale("color") +
    ggnewscale::new_scale("linewidth") +
    polar_connect(data_cor,
      aes(
        x = .data$var1, xend = .data$var2,
        linewidth = .data$linewidth, color = .data$way
      ),
      alpha = 0.5
    ) +
    ggplot2::scale_color_manual(values = c("cyan", "orange")) +
    ggplot2::labs(color = "correlation type", linewidth = "correlation size") +
    ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8)
    )
  p
}


#' Init a dot plot in polar system
#'
#' @description
#' `r lifecycle::badge('stable')`
#' @param data A `data.frame` contains all events, e.g., genes.
#' @param mapping Set of aesthetic mappings to [ggplot2::geom_point()].
#' You should not set mapping for `y`.
#' @param ... Other arguments passing to [ggplot2::geom_point()].
#'
#' @importFrom ggplot2 .pt aes aes_string alpha coord_polar element_blank
#' expand_limits geom_point geom_segment ggplot ggproto labs theme
#' zeroGrob element_text
#'
#' @returns A `ggplot` object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' # -------------------
#' #  Init a polar plot
#' # -------------------
#'
#' data <- data.frame(x = LETTERS[1:7])
#'
#' p1 <- polar_init(data, aes(x = x))
#' p1
#'
#' # Set aes value
#' p2 <- polar_init(data, aes(x = x), size = 3, color = "red", alpha = 0.5)
#' p2
#'
#' # Set aes mapping
#' set.seed(123L)
#' data1 <- data.frame(
#'   x = LETTERS[1:7],
#'   shape = c("r", "r", "r", "b", "b", "b", "b"),
#'   color = c("r", "r", "r", "b", "b", "b", "b"),
#'   size = abs(rnorm(7))
#' )
#' # Check https://ggplot2.tidyverse.org/reference/geom_point.html
#' # for how to use both stroke and color
#' p3 <- polar_init(data1, aes(x = x, size = size, color = color, shape = shape), alpha = 0.5)
#' p3
#'
#' # --------------------
#' #  Connect polar dots
#' # --------------------
#' data2 <- data.frame(
#'   x1 = LETTERS[1:7],
#'   x2 = c("B", "C", "D", "E", "C", "A", "C"),
#'   color = c("r", "r", "r", "b", "b", "b", "b")
#' )
#' p4 <- p3 + polar_connect(data2, aes(x = x1, xend = x2))
#' p4
#'
#' p5 <- p3 + polar_connect(data2, aes(x = x1, xend = x2, color = color), alpha = 0.8, linetype = 2)
#' p5
#'
#' # Use two different color scales
#' if (requireNamespace("ggnewscale")) {
#'   library(ggnewscale)
#'   p6 <- p3 +
#'     new_scale("color") +
#'     polar_connect(data2, aes(x = x1, xend = x2, color = color), alpha = 0.8, linetype = 2)
#'   p6 + scale_color_brewer()
#'   p6 + scale_color_manual(values = c("darkgreen", "magenta"))
#' }
#' @testexamples
#' expect_s3_class(p5, "ggplot")
#' if (requireNamespace("ggnewscale")) {
#'   expect_s3_class(p6, "ggplot")
#' }
#' @family risk_network
polar_init <- function(data, mapping, ...) {
  stopifnot(is.data.frame(data))
  data$y <- 1

  mapping2 <- aes(
    y = .data$y
  )
  mapping <- merge_mapping(mapping, mapping2)

  ggplot(data) +
    geom_point(mapping = mapping, ...) +
    coord_polar() +
    expand_limits(y = c(0, 1.3)) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(hjust = -Inf, vjust = -Inf),
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    labs(y = NULL, x = NULL)
}


#' Connects dots
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Check [polar_init()] for examples.
#'
#' @param data A `data.frame` contains connections of all events.
#' @param mapping Set of aesthetic mappings to [ggplot2::geom_segment()].
#' Set mappings for `x` and `xend` are required.
#' @param ... Other arguments passing to [ggplot2::geom_segment()].
#' @returns A `ggplot` object.
#' @export
#' @family risk_network
polar_connect <- function(data, mapping, ...) {
  stopifnot(is.data.frame(data))
  # TODO: 完美连接球和线?
  mapping2 <- aes(
    y = 0.98,
    yend = 0.98
  )
  mapping <- merge_mapping(mapping2, mapping)
  geom_segment_straight(
    mapping = mapping,
    data = data,
    ...
  )
}

merge_mapping <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  for (i in names(y)) {
    x[[i]] <- .subset2(y, i)
  }
  x
}
