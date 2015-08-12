# Hilbe, J.M., Practical Guide to Logistic Regression 2015
# Rafael de Souza, Eotvos Lorand Univ.
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License version 3 as published by
#the Free Software Foundation.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#' @title  Display confusion or classification matrix following logistic regression using glm with binomial family.
#' @description Provides a confusion matrix of classification statistics following logistic regression.
#' @aliases confusion_stat
#' @usage confusion_stat(<predicted statistic>, <observed response>)
#'
#' @format \arguments{
#' \item{x}{
#' The function has two arguments: predicted values, response values}
#' }
#' @param pred
#' @param obs
#' @return confusion matrix
#' @import caret
#' @note confusion_stat() must be loaded into memory in order to be effectve. As a function in LOGIT,
#' it is immediately available to a user.
#'@examples
#'  library(MASS)
#'  library(LOGIT)
#'  data(medpar)
#'  mylogit <- glm( died ~  los + white + hmo, family=binomial, data=medpar)
#'  mu <- predict(mylogit, type="response")
#'  confusion_stat(mu, medpar$died)
#'
#' @seealso \code{\link{glm}}
#' @author Rafael de Souza, ELTE  University,  and Joseph M. Hilbe, Arizona State University
#'
#' @references Hilbe, Joseph M. (2016), Practical Guide to Logistic Regression, Chapman & Hall/CRC.
#' Hilbe, Joseph M. (2009), Logistic Regression Models, Chapman & Hall/CRC.
#' @keywords models
#' @export
#'
confusion_stat<-function(pred=pred,obs=obs)
{
  CM<-table(pred,obs)
  table=ftable(addmargins(CM))
  stat<-confusionMatrix(pred,obs)
  Sensitivity<-stat
  return(list(matrix=table,statistics=c(stat$overall[1],stat$byClass[1],stat$byClass[2] )))
}
