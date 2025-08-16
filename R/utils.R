#' Continuous Variable Configuration for Theseus Plot
#'
#' @description
#' The `continuous_config()` function creates a configuration object for handling
#' continuous variables in Theseus Plots. It controls how continuous data is binned
#' into discrete categories for contribution calculations and visualization.
#'
#' @param n integer. Number of bins to create for a continuous variable.
#' @param pretty logical. If TRUE, use pretty breaks for bin edges.
#' @param split string. Method for binning continuous variables. Options are:
#'   - "count": divide the variable into bins with roughly equal number of observations.
#'   - "width": divide the range of the variable into equal-width bins.
#'   - "rate": divide based on differences in outcome rates between bins.
#' @param breaks numeric vector specifying custom break points.
#'
#' @return A list containing binning parameters (`n`, `pretty`, `split`, `breaks`)
#'   to be used in plotting or contribution calculations for continuous variables.
#'
#' @examples
#' continuous_config(n = 5, pretty = FALSE, split = "rate")
#'
#' @export
continuous_config <- function(n = 10L, pretty = TRUE,
                              split = c("count", "width", "rate"),
                              breaks = NULL) {
  split <- match.arg(split)
  list(n = n, pretty = pretty, split = split, breaks = breaks)
}

compute_breaks <- function(values, break_num = 10L) {
  if (any(is.na(values))) {
    break_num <- break_num - 1L
  }
  breaks <- quantile(values, probs = seq(0, 1, length.out = break_num + 1), na.rm = TRUE)
  breaks <- unname(breaks)
  breaks
}

pretty_breaks <- function(breaks) {
  digits <- ifelse(breaks == 0, 0, floor(log10(abs(breaks))) + 1L)
  base <- 10 ^ (digits - 2)
  ifelse(sign(breaks) < 0, floor(breaks / base), ceiling(breaks / base)) * base
}
