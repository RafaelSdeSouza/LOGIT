# Hilbe, J.M., Practical Guide to Logistic Regression 2015
# Rafael de Souza, Eotvos Lorand Univ.
require(caret)
confusion_stat<-function(pred=pred,obs=obs)
{
  CM<-table(pred,obs)
  table=ftable(addmargins(CM))
  stat<-confusionMatrix(pred,obs)
  Sensitivity<-stat
  return(list(matrix=table,statistics=c(stat$overall[1],stat$byClass[1],stat$byClass[2] )))
}
