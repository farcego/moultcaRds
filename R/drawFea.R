    ##' Draw a schematic feather
    ##'
    ##' Draw a simplified feather polygon using grid graphics.
    ##' Optionally adds hatching and a label.
    ##'
    ##' @param x Numeric. X position of the feather base.
    ##' @param y Numeric. Y position of the feather base.
    ##' @param w Numeric. Feather width.
    ##' @param h Numeric. Feather length.
    ##' @param v Numeric. Moult value between 0 and 1 used for shading.
    ##' @param lab Optional label plotted below the feather.
    ##' @param hatch Logical. Draw diagonal hatching.
    ##' @param tip Numeric. Length of the feather tip.
    ##'
    ##' @return Invisibly returns NULL.
    ##'
    ##' @details
    ##' The feather is drawn as a five-vertex polygon.
    ##' Shading is controlled by \code{v} through \code{colSc()}.
    ##' When \code{hatch = TRUE}, diagonal lines are clipped to the
    ##' feather area and the border is redrawn on top.
    ##'
    ##' @keywords internal
    fea <- function(x, y, w = 3.2, h = 10, v = 0,
                    lab = NULL, hatch = FALSE, tip = 1.5) {
        xx <- c(x, x + w, x + w, x + w / 2, x)
        yy <- c(y + h, y + h, y + tip, y, y + tip)

        grid::grid.polygon(
            xx, yy,
            default.units = "native",
            gp = grid::gpar(fill = colSc(v), col = "grey35", lwd = .45)
        )

        if (hatch) {
            grid::pushViewport(grid::viewport(
                x = grid::unit(x + w / 2, "native"),
                y = grid::unit(y + h / 2, "native"),
                width = grid::unit(w, "native"),
                height = grid::unit(h, "native"),
                clip = "on"
            ))

            for (j in seq(-h, w, by = 1.4)) {
                grid::grid.lines(c(j, j + h), c(0, h),
                                 default.units = "native",
                                 gp = grid::gpar(col = "grey45", lwd = .45))
            }

            grid::popViewport()

            grid::grid.polygon(
                xx, yy,
                default.units = "native",
                gp = grid::gpar(fill = NA, col = "grey35", lwd = .45)
            )
        }

        if (!is.null(lab)) {
            txt(lab, x + w / 2, y - 2.1, .5)
        }
    }
