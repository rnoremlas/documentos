\encoding{latin1}
\name{CoeficienteVariacion}
\alias{CoeficienteVariacion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coeficiente de Variación
}
\description{
La presente función calcula el coeficiente de variación de un conjunto de datos usando la varianza muestral.
}
\usage{
CoeficienteVariacion(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Es un vector de longitud no nula de números reales.
}
}
\author{
Román Salmerón Gómez
}
\examples{
## Ejemplo 1

data(data.cv)
CoeficienteVariacion(data.cv)

## Ejemplo 2

x = rnorm(50)
CoeficienteVariacion(x)

## Ejemplo 3

x = c(1,2,3,4,5,6,7,8,9)
CoeficienteVariacion(x)

## The function is currently defined as
CoeficienteVariacion <- function(x) 
{
    n = length(x)
    varianza = ((n - 1)/n) * var(x)
    cv = sqrt(varianza)/abs(mean(x))
    return(cv)
  }
}
\keyword{Coeficiente de Variación}
\keyword{Varianza}

