CoeficienteVariacion <-
function(x){
  n = length(x)
  varianza = ((n-1)/n)*var(x)
  cv = sqrt(varianza)/abs(mean(x))
  return(cv)
}
