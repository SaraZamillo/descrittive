#' @title Rappresentazione grafica dei quantili di una distribuzione
#' @description Confronta la distribuzione cumulata della variabile osservata con la distribuzione cumulata della normale.
#' Se la variabile osservata presenta una distribuzione normale, i punti di questa distribuzione congiunta si addensano sulla diagonale che va dal basso verso l'alto e da sinistra verso destra.
#' Aggiunge anche una retta tra la distribuzione e i quantili dei dati.
#' @param x vettore numerico di cui vogliamo verificare la normalita'.
#' @return Un grafico dove vengono rappresentitati i punti del vettore e una retta che approssima la funzione normale.
#' Se i punti seguono la retta allora la distribuzione del vettore e' normale.
#' @export
#' @examples
#' x=rnorm(100,mean=165,10)
#' confrontoNormale(x)
#'
#' y=rchisq(20, 2, ncp = 0)
#' confrontoNormale(y)
confrontoNormale <- function(x){
  qqnorm(x, pch=15, col="mediumblue", main="Confronto tra x e y~N(0,1)")
  qqline(x, col=3, lwd=3, lty=2)
}
