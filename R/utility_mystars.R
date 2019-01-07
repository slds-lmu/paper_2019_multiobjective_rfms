mystars = function (x, key.cex, key.labelsize, full = TRUE, scale = TRUE, radius = TRUE, labels = dimnames(x)[[1L]], 
    locations = NULL, nrow = NULL, ncol = NULL, len = 1, key.loc = NULL, 
    key.labels = dimnames(x)[[2L]], key.xpd = TRUE, xlim = NULL, 
    ylim = NULL, flip.labels = NULL, draw.segments = FALSE, col.segments = 1L:n.seg, 
    col.stars = NA, col.lines = NA, axes = FALSE, frame.plot = axes, 
    main = NULL, sub = NULL, xlab = "", ylab = "", cex = 0.8, 
    lwd = 0.25, lty = par("lty"), xpd = FALSE, mar = pmin(par("mar"), 
        1.1 + c(2 * axes + (xlab != ""), 2 * axes + (ylab != 
            ""), 1, 0)), add = FALSE, plot = TRUE, ...) {
    if (is.data.frame(x)) 
        x <- data.matrix(x)
    else if (!is.matrix(x)) 
        stop("'x' must be a matrix or a data frame")
    if (!is.numeric(x)) 
        stop("data in 'x' must be numeric")
    n.loc <- nrow(x)
    n.seg <- ncol(x)
    if (is.null(locations)) {
        if (is.null(nrow)) 
            nrow <- ceiling(if (!is.numeric(ncol)) sqrt(n.loc) else n.loc/ncol)
        if (is.null(ncol)) 
            ncol <- ceiling(n.loc/nrow)
        if (nrow * ncol < n.loc) 
            stop("'nrow * ncol' is less than the number of observations")
        ff <- if (!is.null(labels)) 
            2.3
        else 2.1
        locations <- expand.grid(ff * 1L:ncol, ff * nrow:1)[1L:n.loc, 
            ]
        if (!is.null(labels) && (missing(flip.labels) || !is.logical(flip.labels))) 
            flip.labels <- ncol * mean(nchar(labels, type = "c")) > 
                30
    }
    else {
        if (is.numeric(locations) && length(locations) == 2) {
            locations <- cbind(rep.int(locations[1L], n.loc), 
                rep.int(locations[2L], n.loc))
            if (!missing(labels) && n.loc > 1) 
                warning("labels do not make sense for a single location")
            else labels <- NULL
        }
        else {
            if (is.data.frame(locations)) 
                locations <- data.matrix(locations)
            if (!is.matrix(locations) || ncol(locations) != 2) 
                stop("'locations' must be a 2-column matrix.")
            if (n.loc != nrow(locations)) 
                stop("number of rows of 'locations' and 'x' must be equal.")
        }
        if (missing(flip.labels) || !is.logical(flip.labels)) 
            flip.labels <- FALSE
    }
    xloc <- locations[, 1]
    yloc <- locations[, 2]
    angles <- if (full) 
        seq.int(0, 2 * pi, length.out = n.seg + 1)[-(n.seg + 
            1)]
    else if (draw.segments) 
        seq.int(0, pi, length.out = n.seg + 1)[-(n.seg + 1)]
    else seq.int(0, pi, length.out = n.seg)
    if (length(angles) != n.seg) 
        stop("length of 'angles' must equal 'ncol(x)'")
    if (scale) {
        x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/diff(range(x, 
            na.rm = TRUE)))
    }
    x[is.na(x)] <- 0
    mx <- max(x <- x * len)
    if (is.null(xlim)) 
        xlim <- range(xloc) + c(-mx, mx)
    if (is.null(ylim)) 
        ylim <- range(yloc) + c(-mx, mx)
    deg <- pi/180
    op <- par(mar = mar, xpd = xpd)
    on.exit(par(op))
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    if (plot && !add) 
        plot(0, type = "n", ..., xlim = xlim, ylim = ylim, main = main, 
            sub = sub, xlab = xlab, ylab = ylab, asp = 1, axes = axes)
    if (!plot) 
        return(locations)
    s.x <- xloc + x * rep.int(cos(angles), rep.int(n.loc, n.seg))
    s.y <- yloc + x * rep.int(sin(angles), rep.int(n.loc, n.seg))
    if (draw.segments) {
        aangl <- c(angles, if (full) 2 * pi else pi)
        for (i in 1L:n.loc) {
            px <- py <- numeric()
            for (j in 1L:n.seg) {
                k <- seq.int(from = aangl[j], to = aangl[j + 
                  1], by = 1 * deg)
                px <- c(px, xloc[i], s.x[i, j], x[i, j] * cos(k) + 
                  xloc[i], NA)
                py <- c(py, yloc[i], s.y[i, j], x[i, j] * sin(k) + 
                  yloc[i], NA)
            }
            polygon(px, py, col = col.segments, lwd = lwd, lty = lty)
        }
    }
    else {
        for (i in 1L:n.loc) {
            polygon(s.x[i, ], s.y[i, ], lwd = lwd, lty = lty, 
                col = col.stars[i])
            polygon(s.x[i, ], s.y[i, ], lwd = lwd, lty = lty, 
                border = col.lines[i], col = col.stars[i])
            if (radius) 
                segments(rep.int(xloc[i], n.seg), rep.int(yloc[i], 
                  n.seg), s.x[i, ], s.y[i, ], lwd = lwd, lty = lty)
        }
    }
    if (!is.null(labels)) {
        y.off <- mx * (if (full) 
            1
        else 0.1)
        if (flip.labels) 
            y.off <- y.off + cex * par("cxy")[2L] * ((1L:n.loc)%%2 - 
                if (full) 
                  0.4
                else 0)
        text(xloc, yloc - y.off, labels, cex = cex, adj = c(0.5, 
            1))
    }
    if (!is.null(key.loc)) {
        par(xpd = key.xpd)
        key.x <- key.cex * len * cos(angles) + key.loc[1L]
        key.y <- key.cex * len * sin(angles) + key.loc[2L]
        if (draw.segments) {
            px <- py <- numeric()
            for (j in 1L:n.seg) {
                k <- seq.int(from = aangl[j], to = aangl[j + 
                  1], by = 1 * deg)
                px <- c(px, key.loc[1L], key.x[j], len * cos(k) + 
                  key.loc[1L], NA)
                py <- c(py, key.loc[2L], key.y[j], len * sin(k) + 
                  key.loc[2L], NA)
            }
            polygon(px, py, col = col.segments, lwd = lwd, lty = lty)
        }
        else {
            polygon(key.x, key.y, lwd = lwd, lty = lty)
            if (radius) 
                segments(rep.int(key.loc[1L], n.seg), rep.int(key.loc[2L], 
                  n.seg), key.x, key.y, lwd = lwd, lty = lty)
        }
        lab.angl <- angles + if (draw.segments) 
            (angles[2L] - angles[1L])/2
        else 0
        label.x <- key.cex *1.1 * len * cos(lab.angl) + key.loc[1L]
        label.y <- key.cex *1.1 * len * sin(lab.angl) + key.loc[2L]
        for (k in 1L:n.seg) {
            text.adj <- c(if (lab.angl[k] < 90 * deg || lab.angl[k] > 
                270 * deg) 0 else if (lab.angl[k] > 90 * deg && 
                lab.angl[k] < 270 * deg) 1 else 0.5, if (lab.angl[k] <= 
                90 * deg) (1 - lab.angl[k]/(90 * deg))/2 else if (lab.angl[k] <= 
                270 * deg) (lab.angl[k] - 90 * deg)/(180 * deg) else 1 - 
                (lab.angl[k] - 270 * deg)/(180 * deg))
            text(label.x[k], label.y[k], labels = key.labels[k], 
                cex = key.labelsize *cex, adj = text.adj)
        }
    }
    if (frame.plot) 
        box(...)
    invisible(locations)
}
