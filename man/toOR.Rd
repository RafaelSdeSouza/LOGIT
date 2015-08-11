\name{toOR}
\alias{toOR}

\title{
 Display logistic coefficient table as odds ratios and associated statistics
}
\description{
Following the glm command, toOR() displays a table of odds ratios and related statistics
including exponentiated model confidence intervals. 
}
\usage{
toOR(x)
}

\arguments{
  \item{x}{
The only argument is the name of the fitted glm function model}
}

\details{
toOR is a post-estimation function, following the use of glm(). 
}
\value{

\item{or}{odds ratio of predictor}
\item{delta}{Model standard error using delta method}
\item{zscore}{z-statistic}
\item{pvalue}{probability-value based on normal distribution}
\item{exp.loci}{Exponentialed lower model confidence interval}
\item{exp.upci}{Expontiated upper model confidence interval}
%% ...
}
\references{
Hilbe, Joseph M. (2016), Practical Guide to Logistic Regression, Chapman & Hall/CRC. 

}
\author{
Joseph M. Hilbe, Arizona State University, and 
    Jet Propulsion Laboratory, California Institute of technology 
    hilbe@asu.edu
}
\note{
toOR must be loaded into memory in order to be effectve. As a function in LOGIT, 
it is immediately available to a user. 
}

\seealso{
\code{\link{glm}}
}
\examples{
library(MASS)
library(LOGIT)
data(medpar)
mylogit <- glm(died ~ los + white + hmo, family=binomial, data=medpar)
summary(mylogit)
toOR(mylogit)

}

\keyword{ models }
