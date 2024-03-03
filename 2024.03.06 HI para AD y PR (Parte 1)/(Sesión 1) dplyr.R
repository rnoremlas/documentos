rm(list=ls())
library(dplyr)

###################################################################################################################
# genero unos datos de forma aleatoria

set.seed(1)

mascotas = c("perro", "gato", "loro", "perro", "perro", "gato", "conejo", "gato", "perro", "perro", 
             "perro", "gato", "loro", "perro", "perro", "gato", "conejo", "gato", "perro", "perro",
             "perro", "gato", "loro", "perro", "perro", "gato", "conejo", "gato", "perro", "perro",
             "perro", "gato", "loro", "perro", "perro", "gato", "conejo", "gato", "perro", "perro")
n = length(mascotas)
edad = round(rnorm(n, 5, 2))
ingresos = round(rnorm(n, 15, 4))*100
binomial = rbinom(n, 1, 0.3) # binomial con 10 repeticiones y probabilidad de éxito 0.2
vive = ifelse(binomial==1, "norte", "sur")

datos = data.frame(mascotas, edad, ingresos, vive)
datos

###################################################################################################################
################## opción FILTER (selecciono filas)

  # selecciono los datos de los perros
  perros = filter(datos, mascotas=="perro") 
  perros

  # selecciono los datos de las mascotas que no son perros
  no_perros = filter(datos, mascotas!="perro") 
  no_perros
  
  # ¿cuáles son los ingresos medios de las familias del sur que tienen a un gato como mascota?
  gatos_sur = filter(datos, mascotas=="gato" & vive=="sur") 
  gatos_sur
  mean(gatos_sur$ingresos) 
  
  # selecciono los datos de las mascotas que son conejos o loros
  conejos_loros = filter(datos, mascotas=="conejo" | mascotas=="loro") 
  conejos_loros
  
  # ¿hay algún gato que tenga 6 años o más?
  gatos_6 = filter(datos, mascotas=="gato" & edad >= 6) 
  gatos_6
  
  # ¿los ingresos medios del sur son mayores que los del norte?
  sur = filter(datos, vive=="sur")
  mean(sur$ingresos)
  norte = filter(datos, vive=="norte")
  mean(norte$ingresos)

################## opción SELECT (selecciono columnas)

  # ¿los ingresos medios del sur son mayores que los del norte?
  ingresos_sur = select(sur, ingresos)
  mean(ingresos_sur) # pequeño inconveniente
  is.array(ingresos_sur) 
  mean(as.matrix(ingresos_sur))
  ingresos_norte = select(norte, ingresos)
  mean(as.matrix(ingresos_norte))
  
  # puedo seleccionar unas columnas concretas, ya sean seleccionandolas directamente
  datos_edad_vive = select(datos, edad, vive)
  datos_edad_vive
  # o quitando lo que no quiero
  datos_edad_vive = select(datos, -mascotas, -ingresos)
  datos_edad_vive
  datos_sinINGRESOS = select(datos, -ingresos)
  datos_sinINGRESOS
  
  # puedo seleccionar aquellas columnas cuyo nombre contiene una 'e'
  datos_e = select(datos, contains('e'))
  datos_e

################## opción ARRANGE (ordeno filas)
  
  # ordeno los datos según la edad de menor a mayor
  datos_ordenados_edad = arrange(datos, edad)
  datos_ordenados_edad
  
  # ordeno los datos según la edad de menor a mayor y como segundo criterio de mayor a menor en ingresos
  datos_ordenados_edad_ingresos = arrange(datos, edad, -ingresos)
  datos_ordenados_edad_ingresos

  # si la variable es de tipo cadena, usa orden alfabético
  arrange(datos, mascotas, ingresos)
  
################## sintaxis en cadena
  
  # ¿cúantos perros de más de cinco o más años pertenecen a familias con ingresos inferiores a 1500 euros que viven en el sur?
  datos %>% # tomo los datos a usar
    filter(mascotas=="perro") %>% # selecciono a los perros
    filter(edad > 5) %>% # selecciono a los que tienen más de cinco años
    filter(ingresos < 1500) %>% # selecciono los que ingresan menos de 1500 euros
    filter(vive=="sur")
  
  # ¿qué hace el siguiente código?
  datos %>%
    filter(mascotas=="perro") %>% 
    filter(edad > 5) %>% 
    arrange(edad, ingresos, vive) %>% 
    select(-mascotas)
  
###################################################################################################################  