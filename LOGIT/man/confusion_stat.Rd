\name{confusion_stat}
\alias{confusion_stat}

\title{
 Display confusion or classification matrix following logistic regression using glm 
with binomial family.
}

\description{
Provides a confusion matrix of classification statistics following logistic regression. 
}

\usage{
confision_stat(<predicted statistic>, <observed response>)
}

\arguments{
  \item{x}{
The function has two arguments: predicted values, response values}
}

\details{
confusion_stat is a post-estimation function for logistic regression, following the use 
of glm(). Predicted values must be calculated prior to use, and become an argument of the 
function. 
}


\references{
Hilbe, Joseph M. (2016), Practical Guide to Logistic Regression, Chapman & Hall/CRC. 
Hilbe, Joseph M. (2009), Logistic Regression Models, Chapman & Hall/CRC. 

}
\author{
Rafael de Souza, Eötvös Loránd University, Hungary, rafael2706@gmail.com
Joseph M. Hilbe, Arizona State University, hilbe@asu.edu
}
\note{
confusion_stat() must be loaded into memory in order to be effectve. As a function in LOGIT, 
it is immediately available to a user. 
}

\seealso{
\code{\link{glm}}
}
\examples{
library(MASS)
library(LOGIT)
data(medpar)
mylogit <- glm( died ~  los + white + hmo, family=binomial, data=medpar)
mu <- predict(mylogit, type="response")
confusion_stat(mu, medpar$died)

}

\keyword{ models }
