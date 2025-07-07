#' @export
create_ship <- function(data1, data2, labels = c("Original", "Refitted")) {
  ShipOfTheseus$new(data1, data2, labels)
}
