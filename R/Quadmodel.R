#' Lab 4 Quadratic equation
#'
#' @param x A quantitative vector
#'
#' @return A vector of quadratic model y hat value
#' @export
#'
#' @examples
#' quad(1:10)
quad <- function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}
