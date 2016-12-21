eff.cont <- function(yp, sdyp, from, to){
    ## 'fit' is a result from a glm.

    x <- seq(from, to, length = 1000)
    y <- yp * x
    y.upp <- y + 1.96 * sdyp * abs(x)
    y.low <- y - 1.96 * sdyp * abs(x)
    plot(x, exp(y), type = "l", ylim = c(0, max(exp(y.upp))), col = "blue", lwd = 2)
    lines(x, exp(y.upp), lty = 2, col = "blue")
    lines(x, exp(y.low), lty = 2, col = "blue")
    abline(h = 1)
    abline(v = 0)
    invisible(exp(y.low))
}

eff.fact <- function(yp, sdyp, labs, main = "", las = 1, xlab = "Levels",
                     ci = FALSE){
    ## 'yp' is a vector of level estimates, sdyp their se's.
    y <- exp(c(0, yp))
    n <- length(y)
    y.upp <- exp(c(0, yp + 1.96 * sdyp))
    y.low <- exp(c(0, yp - 1.96 * sdyp))
    if (ci){
        ylims <- c(min(y.low), max(y.upp))
    }else{
        ylims <- c(min(y), max(y))
    }
    plot(1:n, y, type = "b", col = "blue", ylim = ylims, axes = FALSE,
         xlab = xlab, ylab = "Rel. Risk", main = main)
    axis(1, at = 1:n, labels = labs, las = las)
    axis(2)
    box()
    if (ci){
        lines(1:n, y.upp, type = "b", lty = 2, pch = "-")
        lines(1:n, y.low, type = "l", lty = 2)

        for (i in 2:n){
            lines(c(i, i), c(y.low[i], y.upp[i]), lty = 2)
        }
    }
   abline(h = 1)
}
