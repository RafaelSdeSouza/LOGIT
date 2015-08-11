# Hilbe, J.M., Practical Guide to Logistic Regression
# Rafael de Souza, Eötvös Loránd Univ. 2015
require(caret)
confusion_stat<-function(pred=pred,obs=obs)
{
  CM<-table(pred,obs)
  table=ftable(addmargins(CM))
  stat<-confusionMatrix(pred,obs)
  Sensitivity<-stat
  return(list(matrix=table,statistics=c(stat$overall[1],stat$byClass[1],stat$byClass[2] ))) 
}  