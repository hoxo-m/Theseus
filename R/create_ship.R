#' @export
create_ship <- function(data1, data2, y = "y", labels = c("Original", "Refitted"),
                        ylab = NULL, digits = 3L, text_size = 1) {
  ShipOfTheseus$new(data1, data2, rlang::enquo(y), labels, ylab = ylab,
                    digits = digits, text_size = text_size)
}
