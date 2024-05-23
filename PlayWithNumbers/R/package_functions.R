#' @title Add seven
#' 
#' @description Adds seven to any number or vector given to the function.
#' 
#' @param x numeric. A single number or a vector with data.
#' @return A data.frame containing the following elements:
#' \describe{
#'  \item{old_numbers}{numeric. A vector of numbers specified by the user.}
#'  \item{new_numbers}{numeric. A vector of numbers specified by the user with 7 added to each of its elements.}
#' }
#' 
#' @examples 
#' # add 7 to a vector of numbers
#' x <- c(3, 4, 5, 6, 7)
#' addSeven(x)
#' # add 7 to a single number
#' addSeven(3)
#' @references 
#' * Sarafoglou, A. (2024). How to add 7 to a number. Science, 10, 206-208.
#' * Sarafoglou, A. (2023). What is addition, when is it useful? Science, 2, 100-168.
#' @export
addSeven <- function(x) {
  dat <- data.frame(old_numbers = x,
                    new_numbers = x + 7)
  return(dat)
  }

#' @export
addThree <- function(x) {x + 3}

#' @export
squareNumber <- function(x){x^2}

#' @export
visualizeTransformations <- function(vector_with_numbers){
  
  df <- data.frame(orig        = rep(vector_with_numbers, 3), 
                   transformed = c(addSeven(vector_with_numbers)$new_numbers,
                                   addThree(vector_with_numbers),
                                   squareNumber(vector_with_numbers)),
                   group       = rep(1:3, each = length(vector_with_numbers)))

  ggplot2::ggplot(data=df, ggplot2::aes(x = orig, y = transformed, color=group)) + ggplot2::geom_point()
  
}
