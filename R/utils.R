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
  # if(break_num == 2 && is.integer(values)) {
  #   min_value <- min(values)
  #   max_value <- max(values)
  #   for(i in min_value:(max_value-1)) {
  #     breaks <- c(min_value-1, i, max_value)
  #     tbl <- table(cut(values, breaks, include.lowest = TRUE))
  #     if(tbl[1] > tbl[2]) break
  #   }
  #   return(breaks)
  # }
  breaks <- unname(breaks)
  # if(length(breaks) == length(unique(breaks))) return(breaks)
  # min_value <- min(values)
  # next_values <- Filter(function(x) x != min_value, values)
  # next_breaks <- compute_breaks(next_values, break_num = break_num - 1)
  # min_value <- max(1, min_value)
  # unname(c(min_value - 1, next_breaks))
  breaks
}

pretty_breaks <- function(breaks) {
  digits <- ifelse(breaks == 0, 0, floor(log10(abs(breaks))) + 1L)
  base <- 10 ^ (digits - 2)
  ifelse(sign(breaks) < 0, floor(breaks / base), ceiling(breaks / base)) * base
}
