waldci<- function(x,n,level){
  phat<-sum(x)/n
  results<-phat + c(-1,1)*qnorm(1-level/2)*sqrt(phat*(1-phat)/n)
  print(results)
}