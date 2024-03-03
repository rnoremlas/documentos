rm(list=ls())

#####

momentos = function(x, r, mp = 0) {
  m = sum((x-mp)^r)/length(x)
  return(m)
}

TODO = function(x, alfa = 0.95, graf = F) {
  n = length(x)
  media = momentos(x, 1)
  mediana = median(x)
  tabla.estadistica = table(x)
  posicion = which.max(table(x))
  moda = as.double(rownames(tabla.estadistica)[posicion])
  varianza = momentos(x, 2, mean(x))
  desv.tip = sqrt(varianza)
  Q1 = quantile(x, probs = 0.25)
  Q3 = quantile(x, probs = 0.75)
  percentil.alfa = quantile(x, probs = alfa)
  minimo = min(x)
  maximo = max(x)
  rango = maximo - minimo
  rango.int = as.numeric(Q3) - as.numeric(Q1)
  ca = maximo/minimo
  rango.re = rango/abs(media)
  cv = desv.tip/abs(media)
  pearson = (media-moda)/desv.tip
  m3 = momentos(x, 3, mean(x))
  simetria = m3/(desv.tip^3)
  m4 = momentos(x, 4, mean(x))
  kurtosis = m4/(varianza^2) - 3
  library(ineq)
  gini = ineq(x, type="Gini")
  if(graf == T) {plot(Lc(x), col="red", lwd=2)}
  salida = data.frame(n, media, mediana, moda, varianza, desv.tip, Q1, Q3, percentil.alfa, minimo, maximo, rango, rango.int, ca, rango.re, cv, pearson, simetria, kurtosis, gini)
  rownames(salida) = c("")
  return(salida)
}

##### 

set.seed(2024)
n=1000
datos = rnorm(n, 4, 4)

TODO(datos)
TODO(datos, alfa = 0.99, graf = T)
