plotExtGrid <- function() {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(xscale = c(0, 100), yscale = c(0, 70)))


    ## main sizes
    macX <- 38
    macW <- 35
    macH <- 4.2
    gcH <- 5.5
    nS <- 9
    sW <- macW / nS
    sx <- macX - macW / 2 + (0:(nS - 1)) * sW

    
    ## labels
    txt("MaC", 13, 61)gg
    txt("MeC", 13, 56.8)
    txt("GC", 13, 51.5)
    txt("T/S", 13, 38.5)

    ## MaC and MeC: shorter than GC
    grid::grid.rect(macX, 61, macW, macH, default.units = "native",
                    gp = grid::gpar(fill = colSc(.6), col = "grey35", lwd = .45))

    grid::grid.rect(macX, 56.8, macW, macH, default.units = "native",
                    gp = grid::gpar(fill = colSc(.6), col = "grey35", lwd = .45))

    ## GC: attached below MeC
    gcY <- 56.8 - macH / 2 - gcH

    ## GC10, no overlap
    fea(sx[1] - sW, gcY, w = sW, h = gcH, v = .6, lab = 10)

    ## GC1-GC9
    for (i in seq_len(nS)) {
        fea(sx[i], gcY, w = sW, h = gcH, v = .6, lab = 10 - i)
    }

    ## tertials / secondaries
    sVal <- c(.6, .6, .6, .6, .6, .45, .25, 0, 0)

    for (i in seq_len(nS)) {
        fea(sx[i], 29.5, w = sW, h = 16, v = sVal[i],
            lab = 10 - i)
    }

    ## CC
    topMac <- 61 + macH / 2
    ccX <- 56
    ccW <- 3
    ccH <- 5.5
    ccY <- topMac - ccH

    fea(ccX, ccY, w = ccW, h = ccH, v = .6)
    txt("CC", ccX + 5, ccY + ccH / 2)

    ## alulae
    alW <- 3.1
    alH <- c(6.0, 6.4, 6.8)
    alX <- c(66.0, 68.1, 70.2)
    alV <- c(.6, .5, .35)

    for (i in 1:3) {
        fea(alX[i], topMac - alH[i] - (i - 1) * .7,
            w = alW, h = alH[i], v = alV[i])
    }

    txt("Al", 75, 60)

    ## primaries and primary coverts start at CC midpoint
    ccMid <- ccX + ccW / 2
    px <- ccMid + seq(0, by = 3.2, length.out = 9)

    ## primary coverts
    for (i in seq_along(px)) {
        fea(px[i], 43, w = 3.2, h = 6.5, v = .6,
            hatch = i %% 2 == 0)
    }

    txt("PC", max(px) + 5, 48)

    ## primaries
    for (i in seq_along(px)) {
        fea(px[i], 29, w = 3.2, h = 14, v = .6,
            lab = i)
    }

    ## P10
    fea(max(px) + 3.5, 35, w = 3.2, h = 7, v = .6, lab = 10)
    txt("P", max(px) + 9, 41)

    ## tail
    rx <- seq(55, by = 3.2, length.out = 6)

    for (i in seq_along(rx)) {
        fea(rx[i], 14, w = 3.2, h = 9, v = .6)
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

    grid::popViewport()
}
