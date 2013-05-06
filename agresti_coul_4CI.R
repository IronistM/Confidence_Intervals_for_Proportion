# Computes the Agresti-Coull `add 4' CI for x successes out of n trials
# with confidence coefficient conflev. Adds 2 successes and
# 4 trials.
add4ci <- function(x,n,level){
  ptilde = (x+2)/(n+4)
  z = abs(qnorm((1-conflev)/2))
  stderr = sqrt(ptilde * (1-ptilde)/(n+4))
  ul = ptilde + z * stderr
  ll = ptilde - z * stderr
  if(ll < 0) ll = 0
  if(ul > 1) ul = 1
  ll
}