##' Plot a wing card
##'
##' Make the appropriate calls to the small plotting helpers in this package
##' and draw a schematic wing-card using grid graphics.
##'
##' @param mac Numeric scalar. Median/marginal coverts moult score.
##' @param mec Numeric scalar. Median/lesser coverts moult score.
##' @param gc Numeric vector of length 10. Greater-covert scores, ordered
##'   from GC10 to GC1.
##' @param ts Numeric vector of length 9. Tertial/secondary scores, ordered
##'   from S9/T to S1.
##' @param cc Numeric scalar. Carpal covert score.
##' @param al Numeric vector of length 3. Alula scores.
##' @param pc Numeric vector of length 9. Primary-covert scores, ordered
##'   from inner to outer coverts.
##' @param p Numeric vector of length 10. Primary scores, P1 to P10.
##' @param r Numeric vector of length 6. Rectrix scores.
##' @param hatchPc Logical vector of length 9, or one logical recycled to
##'   length 9. Indicates which primary coverts should be hatched.
##' @param title Optional plot title.
##' @param xlim Numeric vector of length 2 giving x limits.
##' @param ylim Numeric vector of length 2 giving y limits.
##'
##' @return Invisibly returns NULL.
##'
##' @details
##' This function assumes that the package helper functions \\code{initPlot()},
##' \\code{endPlot()}, \\code{txt()}, \\code{fea()}, and \\code{colSc()} are
##' available. For interactive script use, source those helper files before
##' calling this function.
##'
##' @export
plotWingCard <- function(
    mac = .6,
    mec = .6,
    gc = rep(.6, 10),
    ts = c(.6, .6, .6, .6, .6, .45, .25, 0, 0),
    cc = .6,
    al = c(.6, .5, .35),
    pc = rep(.6, 9),
    p = rep(.6, 10),
    r = rep(.6, 6),
    hatchPc = rep(FALSE, 9),
    title = NULL,
    xlim = c(0, 100),
    ylim = c(0, 70)
) {

    ##' Check moult score vector
    ##'
    ##' Validate that a moult score vector is numeric, has the expected
    ##' length, contains no missing values, and that all values lie
    ##' between 0 and 1.
    ##'
    ##' @param x Numeric vector of moult scores.
    ##' @param n Integer. Expected length of \code{x}.
    ##' @param nm Character string used in the error message to identify
    ##'   the argument being checked.
    ##'
    ##' @return Invisibly returns \code{x} if all checks pass.
    ##'
    ##' @keywords internal
    chkScore <- function(x, n, nm) {
        if (!is.numeric(x) || length(x) != n || anyNA(x) ||
            any(x < 0 | x > 1)) {
            stop(nm, " must be a numeric vector of length ", n,
                 " with values between 0 and 1.", call. = FALSE)
        }
        x
    }

    mac <- chkScore(mac, 1, "mac")
    mec <- chkScore(mec, 1, "mec")
    gc <- chkScore(gc, 10, "gc")
    ts <- chkScore(ts, 9, "ts")
    cc <- chkScore(cc, 1, "cc")
    al <- chkScore(al, 3, "al")
    pc <- chkScore(pc, 9, "pc")
    p <- chkScore(p, 10, "p")
    r <- chkScore(r, 6, "r")

    if (!is.logical(hatchPc) || length(hatchPc) == 0 || anyNA(hatchPc)) {
        stop("hatchPc must be a logical vector.", call. = FALSE)
    }
    hatchPc <- rep(hatchPc, length.out = 9)

    initPlot(xlim = xlim, ylim = ylim)
    on.exit(endPlot(), add = TRUE)

    ## main sizes
    macX <- 38
    macW <- 35
    macH <- 4.2
    gcH <- 5.5
    nS <- 9
    sW <- macW / nS
    sx <- macX - macW / 2 + (0:(nS - 1)) * sW

    ## labels
    if (!is.null(title)) txt(title, 50, 67, .75)
    txt("MaC", 13, 61)
    txt("MeC", 13, 56.8)
    txt("GC", 13, 51.5)
    txt("T/S", 13, 38.5)

    ## marginal and median/lesser coverts
    grid::grid.rect(
        macX, 61, macW, macH,
        default.units = "native",
        gp = grid::gpar(fill = colSc(mac), col = "grey35", lwd = .45)
    )
    grid::grid.rect(
        macX, 56.8, macW, macH,
        default.units = "native",
        gp = grid::gpar(fill = colSc(mec), col = "grey35", lwd = .45)
    )

    ## greater coverts, GC10 followed by GC9-GC1
    gcY <- 56.8 - macH / 2 - gcH
    fea(sx[1] - sW, gcY, w = sW, h = gcH, v = gc[1], lab = 10)
    for (i in seq_len(nS)) {
        fea(sx[i], gcY, w = sW, h = gcH, v = gc[i + 1], lab = 10 - i)
    }

    ## tertials / secondaries
    for (i in seq_len(nS)) {
        fea(sx[i], 29.5, w = sW, h = 16, v = ts[i], lab = 10 - i)
    }

    ## carpal covert
    topMac <- 61 + macH / 2
    ccX <- 56
    ccW <- 3
    ccH <- 5.5
    ccY <- topMac - ccH
    fea(ccX, ccY, w = ccW, h = ccH, v = cc)
    txt("CC", ccX + 5, ccY + ccH / 2)

    ## alula
    alW <- 3.1
    alH <- c(6.0, 6.4, 6.8)
    alX <- c(66.0, 68.1, 70.2)
    for (i in seq_along(al)) {
        fea(alX[i], topMac - alH[i] - (i - 1) * .7,
            w = alW, h = alH[i], v = al[i])
    }
    txt("Al", 75, 60)

    ## primary coverts and primaries
    ccMid <- ccX + ccW / 2
    px <- ccMid + seq(0, by = 3.2, length.out = 9)

    for (i in seq_along(px)) {
        fea(px[i], 43, w = 3.2, h = 6.5, v = pc[i], hatch = hatchPc[i])
    }
    txt("PC", max(px) + 5, 48)

    for (i in seq_along(px)) {
        fea(px[i], 29, w = 3.2, h = 14, v = p[i], lab = i)
    }
    fea(max(px) + 3.5, 35, w = 3.2, h = 7, v = p[10], lab = 10)
    txt("P", max(px) + 9, 41)

    ## rectrices
    rx <- seq(55, by = 3.2, length.out = 6)
    for (i in seq_along(rx)) {
        fea(rx[i], 14, w = 3.2, h = 9, v = r[i])
    }
    txt("R", 77, 20)

    ## legend
    vals <- c(0, .1, .3, .6, .9, 1)
    lx <- 19
    for (i in seq_along(vals)) {
        grid::grid.rect(
            lx + i * 3.2, 7, 3.2, 2,
            default.units = "native",
            gp = grid::gpar(fill = colSc(vals[i]), col = "grey50", lwd = .4)
        )
        txt(vals[i] * 100, lx + i * 3.2, 4.8, .48)
    }
    txt("%", lx + 22, 4.8, .48)

    invisible(NULL)
}

##' Save a wing card to file
##'
##' Convenience wrapper around \\code{plotWingCard()}.
##'
##' @param file Output file path. Supported extensions are png, pdf, and svg.
##' @param width Plot width in inches.
##' @param height Plot height in inches.
##' @param res PNG resolution.
##' @param ... Arguments passed to \\code{plotWingCard()}.
##'
##' @return Invisibly returns \\code{file}.
##'
##' @export
saveWingCard <- function(file, width = 9, height = 6.3, res = 300, ...) {

    ext <- tolower(tools::file_ext(file))

    if (ext == "png") {
        grDevices::png(file, width = width, height = height,
                       units = "in", res = res)
    } else if (ext == "pdf") {
        grDevices::pdf(file, width = width, height = height)
    } else if (ext == "svg") {
        grDevices::svg(file, width = width, height = height)
    } else {
        stop("file must end in .png, .pdf, or .svg.", call. = FALSE)
    }

    on.exit(grDevices::dev.off(), add = TRUE)
    plotWingCard(...)

    invisible(file)
}
