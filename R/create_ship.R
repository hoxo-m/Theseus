#' @export
create_ship <- function(data1, data2, labels = c("Original", "Refitted")) {
  ship <- ShipOfTheseus$new(data1, data2, labels)
  memoise_R6_method(ship, "compute_table")
  ship
}
