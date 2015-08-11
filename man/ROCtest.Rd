\name{ROCtest}
\alias{ROCtest}

\title{
 Display ROC curve and related AUC statistic, or sensitivity-specificity plot of glm 
with binomial family
}

\description{
Provides two options following the glm() function with binomial family. 
1: Senstivity-specificity plot with optimal cut point statistic 
2: ROC plot with Area Under Curve (AUC) statistic
}

\usage{
ROCtest(modelname, # folds, type="Sensitivity" | "ROC")
}

\arguments{
  \item{x}{
The function has three arguments: modelname, folds, type of plot}
}

\details{
ROCtest is a post-estimation function for logistic regression, following the use 
of glm(). Options to display a sensitivity-specificity plot or ROC curve are 
available. 
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
ROCtest() must be loaded into memory in order to be effectve. As a function in LOGIT, 
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
summary(mylogit)
ROCtest(mylogit, fold=10, type="Sensitivity")
ROCtest(mylogit, fold=10, type="ROC")
}

\keyword{ models }
