#' @export
create_ship <- function(data1, data2, y = "y", labels = c("Original", "Refitted")) {
  ShipOfTheseus$new(data1, data2, rlang::enquo(y), labels)
}
