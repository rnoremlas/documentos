install.packages('remotes') ## solo primera vez
library(remotes)

x = 1:15
CoeficienteVariacion(x)

## https://github.com/rnoremlas/CoefVar
install_github("rnoremlas/CoefVar")

library(CoefVar)
CoeficienteVariacion(x)
