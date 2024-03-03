SieteMEDIA <- function(){
  cat("\n")
  cat("¿Jugamos a las siete y media? \n")
  cat("Ganarás si sacas la misma o mayor puntuación que la banca y ésta sea menos que 7.5. \n")
  cat("Escribe 's' (sí) o 'n' (no) y pulsa enter. \n")
  jugar = scan(n=1, quiet=T, what=character())
  if (jugar == "n"){
    cat("\n")
    cat("Tú te lo pierdes. Si cambias de opinión escribe SieteMEDIA(). \n")
  } else {
    if (jugar == "s"){
      banca = sample(c(6, 6.5, 7, 7.5), 1)
      puntos = sample(3:5, 1)
      cat("Tu primera carta es un", puntos, ". La banca tiene un ", banca,". ¿Quieres otra? Escribe 's' (sí) o 'n' (no) y pulsa enter. \n")
      otra = scan(n=1, quiet=T, what=character())
      #if (otra != "s"){cat("¡Cobarde!. ")}
      while (otra == "s"){
        siguiente = sample(c(1,2,0.5), 1)
        puntos = puntos + siguiente
        if (puntos> 7.5){
          cat("Tu nueva carta es un", siguiente, ". Lo siento, has perdido. Escribe SieteMEDIA() si quieres probar de nuevo. \n")
          otra = "n"
        } else {
          cat("Tu nueva carta es un", siguiente, ". ¿Quieres otra? Escribe 's' (sí) o 'n' (no) y pulsa enter. \n")  
          otra = scan(n=1, quiet=T, what=character())
        }
      }
      if ((puntos >= banca) && (puntos <= 7.5)){cat("¡Enhorabuena, has ganado! Escribe SieteMEDIA() si quieres probar de nuevo. \n")}
      if ((puntos < banca) && (puntos <= 7.5)){cat("Parece que no sabes sumar (o escribir). Escribe SieteMEDIA() si quieres probar de nuevo. \n")}
    } else {
      cat("Era muy fácil, simplemente tenías que escribir 's' (sí) o 'n' (no). Anda, escribe SieteMEDIA() de nuevo... \n")
    }
  }
  cat("\n")
}

#######################

SieteMEDIA()
