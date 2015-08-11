\name{hlGOF.test}
\alias{hlGOF.test}

\title{
Display Hosmer-Lemeshow statistic and table of probabilities following logistic 
regression using glm with binomial family.
}

\description{
Provides a Hosmer-Lemeshow statistic and table following logistic regression. 
}

\usage{
hlGOF.test(response, predicted statistic, # breaks or groups)
See example 
}

\arguments{
  \item{x}{
The function has three arguments: resppnse term, predicted values, # groups}
}

\details{
hlGOF.test is a post-estimation function for logistic regression, following the use 
of glm(). Usage displays a table of observed vs predicted groups and an overall 
H-L goodness-of-fit statistic. The test is originally from Hilbe (2009). 
}


\references{
Hilbe, J. M. (2016), Practical Guide to Logistic Regression, Chapman & Hall/CRC. 
Hilbe, J. M. (2009), Logistic Regression Models, Chapman & Hall/CRC. 
}

\author{
Joseph M. Hilbe, Arizona State University, Hilbe@asu.edu. Robert LaBudde, 
Institute for Statisical Education (Statistics.com), provided initial code 
for this function for Hilbe, Logistic Regression Models, text. 
}

\note{
hlGOF.test must be loaded into memory in order to be effectve. As a function in LOGIT, 
it is immediately available to a user. My thanks to Prof. Robert LaBudde for the initial 
version of this function. 
}

\seealso{
\code{\link{glm}}
}
\examples{
library(MASS)
library(LOGIT)
data(medpar)
mylogit <- glm( died ~  los + white + hmo, family=binomial, data=medpar)
summary(mylogit)
medpar2 <- na.omit(medpar)
hiGOF.test(medpar$died, predict(medpar2, mylogit, type="response"), breaks=12) 

}

\keyword{ models }
