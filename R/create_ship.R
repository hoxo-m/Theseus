#' @export
create_ship <- function(data1, data2, outcome = "y",
                        labels = c("Original", "Refitted")) {
  ShipOfTheseus$new(data1, data2, rlang::enquo(outcome), labels)
}
