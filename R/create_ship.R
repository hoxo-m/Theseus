#' Creates a Ship Object for Generating Theseus Plots
#'
#' @description
#' Creates a ship object, which serves as a container for data and methods to
#' generate Theseus plots for decomposing differences in rate metrics.
#'
#' @param data1 data frame representing the first group (e.g., the baseline or
#'   "original" data).
#' @param data2 data frame representing the second group (e.g., the comparison
#'   or "refitted" data).
#' @param y column name specifying the outcome variable used to compute the rate
#'   metric (default is "y"). Typically, this is a binary indicator (e.g., 0/1)
#'   that is aggregated to form rates.
#' @param labels character vector of length 2 giving the labels for the two
#'   groups. The first corresponds to `data1`, the second to `data2`. Default is
#'   c("Original", "Refitted").
#' @param ylab string specifying the y-axis label for plots. If NULL (default),
#'   no label is displayed.
#' @param digits integer indicating the number of decimal places to use for
#'   displaying numeric values (default is 3).
#' @param text_size numeric value specifying the relative size of text elements
#'   in plots (default is 1).
#'
#' @return A \link{ShipOfTheseus} object, which can be used with \code{plot()}
#'   to create Theseus plots.
#'
#' @examples
#' library(dplyr)
#'
#' data <- nycflights13::flights |>
#'   filter(!is.na(arr_delay)) |>
#'   mutate(on_time = arr_delay <= 0)
#'
#' data1 <- data |> filter(month == 1)
#' data2 <- data |> filter(month == 2)
#'
#' create_ship(data1, data2, y = on_time)
#'
#' @export
create_ship <- function(data1, data2, y = "y", labels = c("Original", "Refitted"),
                        ylab = NULL, digits = 3L, text_size = 1) {
  ShipOfTheseus$new(data1, data2, rlang::enquo(y), labels, ylab = ylab,
                    digits = digits, text_size = text_size)
}
