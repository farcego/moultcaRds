##' Initialise plotting viewport
##'
##' Open a new grid page and create a viewport with
##' user-defined x and y scales.
##'
##' @param xlim Numeric vector of length 2 giving x limits.
##' @param ylim Numeric vector of length 2 giving y limits.
##'
##' @return Invisibly returns NULL.
##'
##' @keywords internal
initPlot <- function(
    xlim = c(0, 100),
    ylim = c(0, 70)
) {

    grid::grid.newpage()

    grid::pushViewport(
        grid::viewport(
            xscale = xlim,
            yscale = ylim
        )
    )

    invisible(NULL)

}
