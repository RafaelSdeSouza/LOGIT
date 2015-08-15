\name{edrelig}
\alias{edrelig}
\docType{data}
\title{edrelig}
\description{
A study of 601 subjects aimed at determing predictors for religiousity.
Subjects self-identify as being religious or not. Predictrors include gender,
age, having children, and education level.
}
\usage{data(edrelig)}
\format{
  A data frame with 601 observations of grouped data with 5 variables.
  \describe{
    \item{\code{male}}{0=female; 1=male}
    \item{\code{age}}{ages from 17.5 to 57}
    \item{\code{kids}}{0=not have children;1=has children}
    \item{\code{educlevel}}{1=AA degree; 2=BA; 3=MA/PhD}
    \item{\code{religious}}{0=no; 1=yes}
    }
}
\details{
edrelig is saved as a data frame.
}
\source{
Hilbe, Practical Guide to Logistic Regression, Chapman & Hall/CRC
}
\references{
Hilbe, Joseph M (2016), Practical Guide to Logistic Regression, Chapman & Hall/CRC
}
\examples{
library(LOGIT)
data(edrelig)
head(edrelig)
edrelig$cage <- edrelig$age - mean(edrelig$age)
summary(isrelig<- glm(religious ~  male + cage + kids + factor(educlevel), family=binomial, data=edrelig))
toOR(isrelig)
}
\keyword{datasets}




