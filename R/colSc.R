    ##' Convert moult score to grey scale
    ##'
    ##' Convert a moult score between 0 and 1 to a grey colour
    ##' suitable for plotting feathers.
    ##'
    ##' @param x Numeric moult score between 0 and 1.
    ##'
    ##' @return A character string containing a grey colour.
    ##'
    ##' @details
    ##' Values of 0 produce white feathers and values of 1
    ##' produce dark grey feathers.
    ##'
    ##' @keywords internal
    colSc <- function(x) gray(1 - .7 * x)
