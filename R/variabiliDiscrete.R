#' @title Distribuzioni di frequenza assoluta per variabili discrete

#' @description Costruisce la distribuzione di frequenza assoluta del vettore passato in input.
#' Se la variabile in questione e' ordinale per avere l'ordine desiderato si consiglia
#' di eseguire la seguente istruzione:
#' vettore = factor(vettore, levels=c("primo valore","secondo valore",...))
#' @param vettore vettore numerico (ordinale o discreto) oppure un vettore nominale
#' @return Una tabella contenente la distribuzione assoluta
#' @export
#' @examples
#' altezze = c(rep(160,5),rep(175,10),rep(185,3),rep(190,2),200,rep(178,8),rep(165,19),150)
#' freqSemplice(altezze)
#'
#' colori=c(rep("verde",10),rep("viola",4),rep("indaco",3),rep("giallo",5), rep("blu", 8),rep("rosso",8))
#' freqSemplice(colori)
#'
freqSemplice <- function(vettore){
  return(table(vettore))
}

#' @title Distribuzioni di frequenza relativa per variabili discrete

#' @description Costruisce la distribuzione di frequenza relativa del vettore passato in input.
#' Se la variabile in questione e' ordinale per avere l'ordine desiderato si consiglia
#' di eseguire la seguente istruzione:
#' vettore = factor(vettore, levels=c("primo valore","secondo valore",...))
#' @param vettore vettore numerico (ordinale o discreto) oppure un vettore nominale
#' @return Una tabella contenente la distribuzione relativa
#' @export
#' @examples
#' altezze = c(rep(160,5),rep(175,10),rep(185,3),rep(190,2),200,rep(178,8),rep(165,19),150)
#' freqRelativa(altezze)
#'
#' colori=c(rep("verde",10),rep("viola",4),rep("indaco",3),rep("giallo",5), rep("blu", 8),rep("rosso",8))
#' freqRelativa(colori)
#'
freqRelativa <- function(vettore){
  return(freqSemplice(vettore)/length(vettore))
}
