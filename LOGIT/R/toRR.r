# coef to IRR following glm-poisson or glm.nb  Joseph M Hilbe 7Feb2015
toRR <- function(object, ...) {
     coef <- object$coef
       se <- sqrt(diag(vcov(object)))
       zscore <- coef / se
     rr <- exp(coef)
       delta <- rr * se
     pvalue <- 2*pnorm(abs(zscore),lower.tail=FALSE)
     loci <- coef - qnorm(.975) * se
     upci <- coef + qnorm(.975) * se
     rrtab <- data.frame(rr, delta, zscore, pvalue, exp(loci), exp(upci))
     round(rrtab, 4)
}