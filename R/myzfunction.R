#' My z function
#'
#' @param x a quantitative vector of data
#'
#' @return A list containing the z value
#' @export
#'
#' @examples
#' myzfunction(x = 1:10)
myzfunction <- function(x) {
  z = (x - mean(x)) / sd(x)
  z
}
