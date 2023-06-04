CoeficienteVariacion <- function(x){
  n = length(x)
  varianza = ((n-1)/n)*var(x)
  cv = sqrt(varianza)/abs(mean(x))
  return(cv)
}

##########################################################

data.cv = c(0,1,9,8,7,-8,-5,2,-3,-4,0,-2,-4,4,2,3,4,-6,-2)

##########################################################

# CoeficienteVariacion(data.cv)