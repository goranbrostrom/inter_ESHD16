xt <-
function (x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
    display = NULL, auto = FALSE, ...) 
{
    cox <- x
    beta <- cox$coef
    se <- sqrt(diag(cox$var))
    if (is.null(cox$naive.var)) {
        tmp <- cbind(beta, exp(beta), se, beta/se, 1 - pchisq((beta/se)^2, 
            1))
        dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)", 
            "se(coef)", "z", "p"))
    }
    else {
        tmp <- cbind(beta, exp(beta), se, beta/se, signif(1 - 
            pchisq((beta/se)^2, 1), digits - 1))
        dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)", 
            "robust se", "z", "p"))
    }
    return(xtable(tmp, caption = caption, label = label, align = align, 
        digits = digits, display = display, auto = auto, ...))
}
