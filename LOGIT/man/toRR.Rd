\name{toRR}
\alias{toRR}

\title{
 Display count model coefficient table as incidence rate ratios and associated statistics
}
\description{
Following the glm or glm.nb commands, toRR() displays a table of incidence rate ratios and related statistics
including exponentiated model confidence intervals. 
}
\usage{
toRR(x)
}

\arguments{
  \item{x}{
The only argument is the name of the fitted glm or glm.nb function model}
}

\details{
toRR is a post-estimation function, following the use of glm() or glm.nb(). 
}
\value{

\item{or}{incidence rate ratio of predictor}
\item{delta}{Model standard error using delta method}
\item{zscore}{z-statistic}
\item{pvalue}{probability-value based on normal distribution}
\item{exp.loci}{Exponentialed lower model confidence interval}
\item{exp.upci}{Expontiated upper model confidence interval}
%% ...
}
\references{
Hilbe, Joseph M. (2016), Practical Guide to Logistic Regression, Chapman & Hall/CRC. 
Hilbe, Joseph M. (2014), Modeling Count Data, Cambridge University Press

}
\author{
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of technology 
    hilbe@asu.edu
}
\note{
toRR must be loaded into memory in order to be effectve. As a function in LOGIT, 
it is immediately available to a user. 
}

\seealso{
\code{\link{glm}}, \code{\link{glm.nb}}
}
\examples{
library(MASS)
library(LOGIT)
data(medpar)
mypoi <- glm(los ~ white + hmo + factor(edlevel), family=poisson, data=medpar)
summary(mypoi)
toRR(mypoi)

}

\keyword{ models }
