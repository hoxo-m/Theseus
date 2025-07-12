#' @export
config_continuous_set <- function(n = 10L) {
  list(n = n)
}

compute_breaks <- function(values, break_num = 10L) {
  if (any(is.na(values))) {
    break_num <- break_num - 1L
  }
  breaks <- quantile(values, probs = seq(0, 1, length.out = break_num + 1), na.rm = TRUE)
  if(break_num == 2 && is.integer(values)) {
    min_value <- min(values)
    max_value <- max(values)
    for(i in min_value:(max_value-1)) {
      breaks <- c(min_value-1, i, max_value)
      tbl <- table(cut(values, breaks, include.lowest = TRUE))
      if(tbl[1] > tbl[2]) break
    }
    return(breaks)
  }
  breaks <- unname(breaks)
  if(length(breaks) == length(unique(breaks))) return(breaks)
  min_value <- min(values)
  next_values <- Filter(function(x) x != min_value, values)
  next_breaks <- compute_breaks(next_values, break_num = break_num - 1)
  min_value <- max(1, min_value)
  unname(c(min_value - 1, next_breaks))
}

pretty_breaks <- function(breaks) {
  digits <- floor(log10(breaks))
  head <- ifelse(digits[1] <= 0, floor(breaks[1]), {
    base <- 10 ^ (digits[1] - 1)
    floor(breaks[1] / base) * base
  })
  tail <- Map(function(break_, digit) {
    if(digit <= 0) {
      ceiling(break_)
    } else {
      base <- 10 ^ (digit - 1)
      ceiling(break_ / base) * base
    }
  }, breaks[-1], digits[-1]) %>% unlist
  c(head, tail)
}
