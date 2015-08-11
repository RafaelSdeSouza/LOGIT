# Hilbe, J.M., Modeling Count Data, Cambridge Univ Press
# Function to calculate Pearson and Pearson dispersion 
#   following glm and glm.nb:  source(P__disp.r)
# x=modelname: ex: P__disp(mymodel)  30Jan,2012 J. Hilbe
P__disp <- function(x) {
   pr <- sum(residuals(x, type="pearson")^2)
   dispersion <- pr/x$df.residual
   cat("\n Pearson Chi2 = ", pr , 
       "\n Dispersion   = ", dispersion, "\n")
}


