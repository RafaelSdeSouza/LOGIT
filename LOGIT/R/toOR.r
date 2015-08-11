# Hilbe, J.M., Practical Guide to Logistic Regression
# From coef table to OR after glm JM Hilbe 7Feb2015
toOR <- function(object, ...) {
     coef <- object$coef
       se <- sqrt(diag(vcov(object)))
       zscore <- coef / se
     or <- exp(coef)
       delta <- or * se
     pvalue <- 2*pnorm(abs(zscore),lower.tail=FALSE)
     loci <- coef - qnorm(.975) * se
     upci <- coef + qnorm(.975) * se
     ortab <- data.frame(or, delta, zscore, pvalue, exp(loci), exp(upci))
     round(ortab, 4)
}
