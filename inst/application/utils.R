
#Funciones tomadas del paquete CARET

prettySeq <- function (x) {
  paste("Resample", gsub(" ", "0", format(seq(along = x))), sep = "")
}

createDataPartition <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(5,
                                                           length(y))) {
  if (class(y)[1] == "Surv")
    y <- y[, "time"]
  out <- vector(mode = "list", times)
  if (length(y) < 2)
    stop("y must have at least 2 data points")
  if (groups < 2)
    groups <- 2
  if (is.numeric(y)) {
    y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))),
             include.lowest = TRUE)
  }
  else {
    xtab <- table(y)
    if (any(xtab == 0)) {
      warning(paste("Some classes have no records (", paste(names(xtab)[xtab == 0], sep = "", collapse = ", "), ") and these will be ignored"))
      y <- factor(as.character(y))
    }
    if (any(xtab == 1)) {
      warning(paste("Some classes have a single record (",
                    paste(names(xtab)[xtab == 1], sep = "", collapse = ", "),
                    ") and these will be selected for the sample"))
    }
  }
  subsample <- function(dat, p) {
    if (nrow(dat) == 1) {
      out <- dat$index
    }
    else {
      num <- ceiling(nrow(dat) * p)
      out <- sample(dat$index, size = num)
    }
    out
  }
  for (j in 1:times) {
    tmp <- plyr:::dlply(data.frame(y = y, index = seq(along = y)),plyr:::.(y), subsample, p = p)
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }
  if (!list) {
    out <- matrix(unlist(out), ncol = times)
    colnames(out) <- prettySeq(1:ncol(out))
  }
  else {
    names(out) <- prettySeq(out)
  }
  out
}

#Funciones tomadas del paquete PSYCH

pairs.panels <- function (x, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE,
          digits = 2, method = "pearson", pch = 20, lm = FALSE, cor = TRUE,
          jiggle = FALSE, factor = 2, hist.col = "cyan", show.points = TRUE,
          rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, smoother = FALSE,
          stars = FALSE, ci = FALSE, alpha = 0.05, ...){
  "panel.hist.density" <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1], usr[2], 0, 1.5))
    tax <- table(x)
    if (length(tax) < 11) {
      breaks <- as.numeric(names(tax))
      y <- tax/max(tax)
      interbreak <- min(diff(breaks)) * (length(tax) -
                                           1)/41
      rect(breaks - interbreak, 0, breaks + interbreak,
           y, col = hist.col)
    }
    else {
      h <- hist(x, breaks = breaks, plot = FALSE)
      breaks <- h$breaks
      nB <- length(breaks)
      y <- h$counts
      y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = hist.col)
    }
    if (density) {
      tryd <- try(d <- density(x, na.rm = TRUE, bw = "nrd",
                               adjust = 1.2), silent = TRUE)
      if (class(tryd) != "try-error") {
        d$y <- d$y/max(d$y)
        lines(d)
      }
    }
    if (rug)
      rug(x)
  }
  "panel.cor" <- function(x, y, prefix = "", ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    if (is.null(wt)) {
      r <- cor(x, y, use = "pairwise", method = method)
    }
    else {
      r <- cor.wt(data.frame(x, y), w = wt[, c(1:2)])$r[1,
                                                        2]
    }
    txt <- format(c(round(r, digits), 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (stars) {
      pval <- r.test(sum(!is.na(x * y)), r)$p
      symp <- symnum(pval, corr = FALSE, cutpoints = c(0,
                                                       0.001, 0.01, 0.05, 1), symbols = c("***", "**",
                                                                                          "*", " "), legend = FALSE)
      txt <- paste0(txt, symp)
    }
    cex <- cex.cor * 0.8/(max(strwidth("0.12***"), strwidth(txt)))
    if (scale) {
      cex1 <- cex * abs(r)
      if (cex1 < 0.25)
        cex1 <- 0.25
      text(0.5, 0.5, txt, cex = cex1)
    }
    else {
      text(0.5, 0.5, txt, cex = cex)
    }
  }
  "panel.smoother" <- function(x, y, pch = par("pch"), col.smooth = "red",
                               span = 2/3, iter = 3, ...) {
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points)
        points(x, y, pch = pch, ...)
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      if (smooth & ci) {
        lml <- loess(y ~ x, degree = 1, family = "symmetric")
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred <- predict(lml, newdata = tempx, se = TRUE)
        if (ci) {
          upperci <- pred$fit + confid * pred$se.fit
          lowerci <- pred$fit - confid * pred$se.fit
          polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                              rev(upperci)), col = adjustcolor("light grey",
                                                                               alpha.f = 0.8), border = NA)
        }
        lines(tempx$x, pred$fit, col = col.smooth, ...)
      }
      else {
        if (smooth)
          lines(stats::lowess(x[ok], y[ok], f = span,
                              iter = iter), col = col.smooth)
      }
    }
    if (ellipses)
      draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.smooth,
                   ...)
  }
  "panel.lm" <- function(x, y, pch = par("pch"), col.lm = "red",
                         ...) {
    ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin, xmin), max(ymax, xmax))
    xlim <- ylim
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ylim = ylim, xlim = xlim,
               ...)
      }
    }
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lml <- lm(y ~ x)
      if (ci) {
        tempx <- data.frame(x = seq(min(x, na.rm = TRUE),
                                    max(x, na.rm = TRUE), length.out = 47))
        pred <- predict.lm(lml, newdata = tempx, se.fit = TRUE)
        upperci <- pred$fit + confid * pred$se.fit
        lowerci <- pred$fit - confid * pred$se.fit
        polygon(c(tempx$x, rev(tempx$x)), c(lowerci,
                                            rev(upperci)), col = adjustcolor("light grey",
                                                                             alpha.f = 0.8), border = NA)
      }
      if (ellipses) {
        xm <- mean(x, na.rm = TRUE)
        ym <- mean(y, na.rm = TRUE)
        xs <- sd(x, na.rm = TRUE)
        ys <- sd(y, na.rm = TRUE)
        r = cor(x, y, use = "pairwise", method = method)
        draw.ellipse(xm, ym, xs, ys, r, col.smooth = col.lm,
                     ...)
      }
      abline(lml, col = col.lm, ...)
    }
  }
  "draw.ellipse" <- function(x = 0, y = 0, xs = 1, ys = 1,
                             r = 0, col.smooth, add = TRUE, segments = 51, ...) {
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + x
      ellipse[, 2] <- ellipse[, 2] * ys + y
      if (show.points)
        points(x, y, pch = 19, col = col.smooth, cex = 1.5)
      lines(ellipse, ...)
    }
  }
  "panel.ellipse" <- function(x, y, pch = par("pch"), col.smooth = "red",
                              ...) {
    segments = 51
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1] - abs(0.05 * usr[1]), usr[2] + abs(0.05 *
                                                            usr[2]), 0, 1.5))
    xm <- mean(x, na.rm = TRUE)
    ym <- mean(y, na.rm = TRUE)
    xs <- sd(x, na.rm = TRUE)
    ys <- sd(y, na.rm = TRUE)
    r = cor(x, y, use = "pairwise", method = method)
    if (jiggle) {
      x <- jitter(x, factor = factor)
      y <- jitter(y, factor = factor)
    }
    if (smoother) {
      smoothScatter(x, y, add = TRUE, nrpoints = 0)
    }
    else {
      if (show.points) {
        points(x, y, pch = pch, ...)
      }
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    if (!is.na(r)) {
      if (abs(r) > 0)
        theta <- sign(r)/sqrt(2)
      else theta = 1/sqrt(2)
      shape <- diag(c(sqrt(1 + r), sqrt(1 - r))) %*% matrix(c(theta,
                                                              theta, -theta, theta), ncol = 2, byrow = TRUE)
      ellipse <- unit.circle %*% shape
      ellipse[, 1] <- ellipse[, 1] * xs + xm
      ellipse[, 2] <- ellipse[, 2] * ys + ym
      points(xm, ym, pch = 19, col = col.smooth, cex = 1.5)
      if (ellipses)
        lines(ellipse, ...)
    }
  }
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (missing(cex.cor))
    cex.cor <- 1
  for (i in 1:ncol(x)) {
    if (is.character(x[[i]])) {
      x[[i]] <- as.numeric(as.factor(x[[i]]))
      colnames(x)[i] <- paste(colnames(x)[i], "*", sep = "")
    }
  }
  n.obs <- nrow(x)
  confid <- qt(1 - alpha/2, n.obs - 2)
  if (!lm) {
    if (cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.smoother, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.smoother,
            lower.panel = panel.smoother, pch = pch, ...)
    }
  }
  else {
    if (!cor) {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.lm,
            lower.panel = panel.lm, pch = pch, ...)
    }
    else {
      pairs(x, diag.panel = panel.hist.density, upper.panel = panel.cor,
            lower.panel = panel.lm, pch = pch, ...)
    }
  }
}

#Funciones tomadas del paquete DUMMIES

dummy <- function (x, data = NULL, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE) {
  if (is.null(data)) {
    name <- as.character(sys.call(1))[2]
    name <- sub("^(.*\\$)", "", name)
    name <- sub("\\[.*\\]$", "", name)
  }
  else {
    if (length(x) > 1)
      stop("More than one variable provided to produce dummy variable.")
    name <- x
    x <- data[, name]
  }
  if (drop == FALSE && class(x) == "factor") {
    x <- factor(x, levels = levels(x), exclude = NULL)
  }
  else {
    x <- factor(x, exclude = NULL)
  }
  if (length(levels(x)) < 2) {
    if (verbose)
      warning(name, " has only 1 level. Producing dummy variable anyway.")
    return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x),
                                                               c(paste(name, sep, x[[1]], sep = "")))))
  }
  mm <- model.matrix(~x - 1, model.frame(~x - 1), contrasts = FALSE)
  colnames.mm <- colnames(mm)
  if (verbose)
    cat(" ", name, ":", ncol(mm), "dummy varibles created\n")
  mm <- matrix(fun(mm), nrow = nrow(mm), ncol = ncol(mm), dimnames = list(NULL,
                                                                          colnames.mm))
  colnames(mm) <- sub("^x", paste(name, sep, sep = ""), colnames(mm))
  if (!is.null(row.names(data)))
    rownames(mm) <- rownames(data)
  return(mm)
}

dummy.data.frame <- function (data, names = NULL, omit.constants = TRUE, dummy.classes = c("factor" ,"character"), all = TRUE, ...) {
  df <- data.frame(row.names = row.names(data))
  new.attr <- list()
  for (nm in names(data)) {
    old.attr <- attr(df, "dummies")
    if (nm %in% names || (is.null(names) && (dummy.classes == "ALL" || class(data[, nm]) %in% dummy.classes))) {
      dummies <- dummy(nm, data, ...)
      if (ncol(dummies) == 1 & omit.constants) {
        dummies <- matrix(nrow = nrow(data), ncol = 0)
      }
      if (ncol(dummies) > 0)
        new.attr[[nm]] <- (ncol(df) + 1):(ncol(df) + ncol(dummies))
    }
    else {
      if (!all)
        (next)()
      dummies <- data[, nm, drop = FALSE]
    }
    df <- cbind(df, dummies)
  }
  attr(df, "dummies") <- new.attr
  return(df)
}
