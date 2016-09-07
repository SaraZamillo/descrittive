#' @title Confronto tra due distribuzioni
#' @description Sovrappone i plot di sue distribuzioni per vedere se hanno andamenti diversi.
#' @param x vettore numerico
#' @param y vettore numerico
#' @param labelx nome del primo vettore
#' @param labely nome del primo vettore.
#' @return Un grafico che rappresenta i due vettori consimboli diversi
#' @export
#' @examples
#' x=rnorm(100,mean=165,10)
#' x2=rnorm(90,mean=170,sd = 20)
#' confrontoDistribuzioni(x,x2,"Popolazione1","Popolazione2")
confrontoDistribuzioni <- function(x,y,labelx,labely){
  plot(x, ylim=range(min(x,y),max(x,y)),ylab="Valori", pch=19, col="red", main="Confronto distribuzioni", col.main="blue3",
       font.main=18, cex.main=1.7, font.lab=18)
  points(y, col="green3", pch=8)
  leg.txt=c(labelx,labely)
  legend("topright", leg.txt, pch=c(19,8), col=c("red","green3"))
}
