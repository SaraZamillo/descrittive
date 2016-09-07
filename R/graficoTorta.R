#' @title Diagramma a torta
#' @description Costruisce il diagramma a torta di un vettore.
#' @param x vettore numerico
#' @param lbls vettore di etichette di x (questi due vettori devono avere la stessa lunghezza).
#' @return Il grafico a torta del vettore numerico con le etichette associate.
#' @export
#' @examples
#' value=c(160,150,100,190,200)
#' label=c("Germania","Italia","Francia","Inghilterra","Austria")
#' graficoTorta(value,label)
graficoTorta <- function(x,lbls){
  pct=round(x/sum(x)*100) #calcolo delle percentuali
  lbls=paste(lbls, pct) # aggiungo il numero percentuale alle etichette
  lbls=paste(lbls,"%",sep="") # aggiungo il simbolo % alle etichette
  pie(x, labels = lbls, main="Grafico a torta", col=rainbow(length(lbls)),
      col.main="red", font.main=8, cex.main=2)
}
