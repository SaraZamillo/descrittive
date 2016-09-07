#' @title Grafico univariato
#' @description Grafico che rappresenta i punti con il loro indice.
#' Valido per dati quantitativi, sia per vettori che per matrici.
#' @param x vettore o una matrice
#' @param labelx etichetta del nome di x
#' @return Un grafico univariato (il tipo di grafico dipende dal vettore o matrice passati in input)
#' @export
#' @examples
#' x=c(167,189,167,156,188,190,200,150,200,177)
#' graficoSingolo(x,"Altezze")
#'
#' m=matrix(rnorm(n = 50,mean = 165,sd = 10),ncol=5)
#' graficoSingolo(m,"Altezze")
graficoSingolo <- function(x,labelx){
  # 'lwd' è lo spessore dei punti
  # 'cex.axis' è la dimensione dei numeri lungo gli assi
  #pch mi fa i pallini pieni
  grafico=plot(x, ylab=labelx, col=4, pch=19, cex.axis = 0.9, font=11,
               fg="grey")
  #mtext aggiugne il testo al grafico
  # 'side' (da 1 a 4) ci dice lungo quale asse verrà posizionato
  #il numero 1 è l'asse x, e poi procede in senso orario
  # 'line' è l'interlinea dal grafico (maggiore è il numero maggiore è l'interlinea)
  mtext(labelx, side=3, line=1, col=2, font=15, cex=1.5)
}

#' @title Grafico multivariato
#' @description Grafico che rappresenta i punti con il rispettivo indice.
#' ATTENZIONE: I VETTORI DEVONO AVERE LE STESSE DIMENSIONI.
#' Se utilizzo due vettori ottengo uno scatterplot normale.
#' Se utilizzo un fattore ed un vettore ottengo dei boxplot.
#' Il comando locator(1) puo' essere utilizzato per guardare le coordinate di un punto sul grafico.
#' @param x un vettore o un fattore
#' @param y un vettore
#' @param labelx etichetta che verra' associata ad x
#' @param labely etichetta che verra' associata ad y
#' @return Uno scatterplot o dei boxplot divisi per i vari livelli del fattore
#' @export
#' @examples
#' x=c(167,189,167,156,188,190,200,150,200,177)
#' v=c(20,23,20,12,13,17,19,16,15,10)
#' graficoMultiplo(x,v,"altezze","peso")
#'
#' y=c(rep("giallo",2), rep("verde",3),rep("blu",4), rep("rosso",5))
#' f=as.factor(y)
#' punti=c(3,4,5,7,8,5,4,3,2,4,6,7,9,0)
#' graficoMultiplo(f,punti,"Squadre","Punti")
graficoMultiplo <- function(x,y,labelx,labely){
  plot(x,y, xlab=labelx, ylab=labely, col="darkmagenta", pch=17, asp=1,
       font=12, fg="grey")
# asp mi fa ottenere un sistema monometrico
# cioè dove il rapporto tra le unità sui due assi è 1
#  abline(v=axTicks(1), h=axTicks(2), col="grey",lty=2)
  mtext("Scatterplot", side=3, line=1, col="hotpink", font=2, cex=1.5)
}
