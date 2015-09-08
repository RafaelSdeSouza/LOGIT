# Hilbe, J.M., Modeling Count Data, Cambridge Univ Press
# Function to calculate Pearson and Pearson dispersion
#   following glm and glm.nb:  source(P_disp.r)
# x=modelname: ex: P_disp(mymodel)  30Jan,2012 J. Hilbe
#' @title   Display Pearson Chi2 and associated dispersion statistic
#' following following use of glm.
#' @description Following the glm() function with a grouped binomial or poisson family, or glm.nb(),
#' P_disp() displays the Pearson Chi2 statistic and related dispersion statistic.
#' Values of the dispersion greater than 1.0 indicate possible overdispersion; values
#' under 1.0 indicate possible underdispersion.
#' @aliases P_disp
#' @import MASS
#' @usage P_disp(x)
#'
#' @format   \describe{
#' \item{x}{
#' The only argument is the name of the fitted glm or glm.nb function model}
#' }
#' @param x  glm object
#' @return list
#' @note P_disp must be loaded into memory in order to be effectve. As a function in LOGIT,
#' it is immediately available to a user.
#' @details P_disp is a post-estimation function, following the use of glm() or glm.nb().
#' Appropriate with grouped binomial or Poisson glm families.
#' @seealso \code{\link{glm}}
#' @author Joseph M. Hilbe, Arizona State University, and
#' Jet Propulsion Laboratory, California Institute of technology
#'
#' @references Hilbe, Joseph M. (2016), Practical Guide to Logistic Regression, Chapman & Hall/CRC.
#' Hilbe, Joseph M. (2014), Modeling Count Data, Cambridge University Press
#'@examples
#'library(MASS)
#'library(LOGIT)
#'data(titanicgrp)
#'class03 <- factor(titanicgrp$class, levels=c("3rd class", "2nd class", "1st class"))
#'died <- titanicgrp$cases - titanicgrp$survive
#'grptit <- glm( cbind(survive, died) ~ age+sex+class03, family=binomial,
#'data=titanicgrp)
#'summary(grptit)
#'P_disp(grptit)
#'
#' @keywords models
#' @export
#'
P_disp <- function(x) {
   pr <- sum(residuals(x, type = "pearson")^2)
   dispersion <- pr/x$df.residual
   cat("\n Pearson Chi2 = ", pr ,
       "\n Dispersion   = ", dispersion, "\n")
}


