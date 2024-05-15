#' @export
addSeven <- function(x) {x + 7}

#' @export
addThree <- function(x) {x + 3}

#' @export
squareNumber <- function(x){x^2}

#' @export
visualizeTransformations <- function(vector_with_numbers){
  
  df <- data.frame(orig        = rep(vector_with_numbers, 3), 
                   transformed = c(addSeven(vector_with_numbers),
                                   addThree(vector_with_numbers),
                                   squareNumber(vector_with_numbers)),
                   group       = rep(1:3, each = length(vector_with_numbers)))

  ggplot2::ggplot(data=df, ggplot2::aes(x = orig, y = transformed, color=group)) + ggplot2::geom_point()
  
}
