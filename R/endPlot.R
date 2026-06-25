##' Close plotting viewport
##'
##' Remove the current grid viewport.
##'
##' @return Invisibly returns NULL.
##'
##' @keywords internal
endPlot <- function() {

    grid::popViewport()

    invisible(NULL)

}
