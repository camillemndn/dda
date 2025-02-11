#' @export
aov.dd <- function(ddobj, group, a, b) {
  rangeval <- ddobj$basis$rangeval
  xval <- seq(a, b, length.out = 1401)
  ddlog <- log(eval(ddobj, xval))
  ddint <- 1 / (b - a) * apply(ddlog, 2, function(logd) integrate(approxfun(xval, logd), a, b)$value)
  ddata <- data.frame(ddint = ddint, group = group)
  fit <- aov(ddint ~ group, data = ddata)
  list(aov = fit, test = anova(fit), data = ddata)
}
