##' Draw text in native coordinates
##'
##' Wrapper around \code{grid::grid.text()} using native units
##(*coordinate system needs to be taken out of the plotting
##function).
##'
##' @param z Character string to plot.
##' @param x Numeric x position.
##' @param y Numeric y position.
##' @param cex Numeric text size.
##'
##' @return Invisibly returns NULL.
##'
##' @keywords internal
txt <- function(z, x, y, cex = .55) {
    grid::grid.text(z, x, y, default.units = "native",
                    gp = grid::gpar(cex = cex))
}
