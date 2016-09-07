#variabile quantitativa (con suddivisione in classi)
#' @title Distribuzioni di frequenza assoluta per variabili quantitative continue
#' @description Costruisce la distribuzione di frequenza assoluta del vettore passato in input.
#' @param vettore vettore numerico
#' @param n numero di classi nelle quali suddividere il vettore.
#' @return Una tabella contenente la distribuzione assoluta ripartita in classi.
#' @export
#' @examples
#' valori = rnorm(100, mean=165,sd=10)
#' freqQuantSemplice(valori,5)
freqQuantSemplice <- function(vettore,n){
  r = range(vettore)
    ampiezzaClassi = (r[2]-r[1])/n

  classi=1:(n+1)
  classi[1]=r[1]
  for (i in 2:(n+1)){
    classi[i]=classi[i-1]+ampiezzaClassi
  }
  classi[1]=classi[1]-0.001
  classi[n+1]=classi[n+1]+0.001
   return(table(cut(vettore, breaks = classi)))
}

freqQuantCumulata<-function(valore,n){
  table<-freqQuantSemplice(valore,n)
    for(i in 2:length(table)) {
      table[i]=table[i-1]+table[i]
    }
  return(table)
}

#' @title Distribuzioni di frequenza relativa per variabili quantitative continue
#' @description Costruisce la distribuzione di frequenza relativa del vettore passato in input.
#' @param vettore vettore numerico
#' @param n numero di classi nelle quali suddividere il vettore.
#' @return Una tabella contenente la distribuzione relativa ripartita in classi.
#' @export
#' @examples
#' valori = rnorm(100, mean=165,sd=10)
#' freqQuantRelativa(valori,5)
freqQuantRelativa <- function(vettore,n){
  return(freqQuantSemplice(vettore,n)/length(vettore))
}
