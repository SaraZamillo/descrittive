#' @title Istogramma di un vettore numerico
#' @description Costruisce l'istogramma di un vettore, specificando il numero di classi desiderate.
#' @param x vettore numerico
#' @param xlabel etichetta specificante il nome di x
#' @param n numero di classi in cui suddividere l'istogramma.
#' @return Un istogramma che rappresenta il vettore.
#' @export
#' @examples
#' x=rnorm(100,mean=165,10)
#' istogramma(x,"Altezze",5)
istogramma <- function(x,xlabel,n){
  #breaks=b esplicita i punti di break degli intervalli dei valori di x che delimitano le classi
  #probability=TRUE le colonne rappresentano le frequenze relative e non assolute
  #las=2 mette i valori delle frequenze relative in verticale
  r = range(x)
  ampiezzaClassi = (r[2]-r[1])/n

  classi=1:(n+1)
  classi[1]=r[1]
  for (i in 2:(n+1)){
    classi[i]=classi[i-1]+ampiezzaClassi
  }
  classi[1]=classi[1]-0.001
  classi[n+1]=classi[n+1]+0.001
  hist(x, breaks=classi, xlab=xlabel, ylab="Frequenze relative", probability = TRUE,
       include.lowest = TRUE, col="slategray1", lty=4, las=1,
       col.main="midnightblue", col.axis="midnightblue", fg="midnightblue", font.main=15,
       font.lab=14, col.lab="midnightblue", border="midnightblue")
}
