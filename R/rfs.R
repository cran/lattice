

rfs <-
    function(model, layout = c(2,1), xlab = "f-value", ylab = NULL,
             panel = function(...) {panel.grid(); panel.qqmath(...)},
             ...)
{
    fitval <- fitted.values(model) - mean(fitted.values(model))
    resids <- residuals(model)
    
    nf <- length(fitval)
    nr <- length(resids)
    
    data <- list(y = c( fitval, resids),
                 f = c( rep("Fitted Values minus Mean", nf),
                 rep("Residuals", nr)))

    qqmath(~y|f, data = data, layout = layout, xlab = xlab, ylab = ylab,
           distribution = qunif, panel = panel, ...)
}
