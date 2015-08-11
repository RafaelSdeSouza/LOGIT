\name{P__disp}
\alias{P__disp}

\title{
 Display Pearson Chi2 and associated dispersion statistic following following use of glm 
}

\description{
Following the glm() function with a grouped binomial or poisson family, or glm.nb(), 
P__disp() displays the Pearson Chi2 statistic and related dispersion statistic. 
Values of the dispersion greater than 1.0 indicate possible overdispersion; values 
under 1.0 indicate possible underdispersion. 
}

\usage{
P__disp(x)
}

\arguments{
  \item{x}{
The only argument is the name of the fitted glm or glm.nb function model}
}

\details{
P__disp is a post-estimation function, following the use of glm() or glm.nb(). 
Appropriate with grouped binomial or Poisson glm families.
}

\value{

\item{Pearson Chi2}{Pearson Chi2 statistic}
\item{Dispersion}{Pearson dispersion: Chi2/dof}
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
P__disp must be loaded into memory in order to be effectve. As a function in LOGIT, 
it is immediately available to a user. 
}

\seealso{
\code{\link{glm}}
}
\examples{
library(MASS)
library(LOGIT)
data(titanicgrp)
class03 <- factor(titanicgrp$class, levels=c("3rd class", "2nd class", "1st class"))
died <- titanicgrp$cases - titanicgrp$survive
grptit <- glm( cbind(survive, died) ~ age+sex+class03, family=binomial, 
   data=titanicgrp)
summary(grptit)
P__disp(grptit)
}

\keyword{ models }
