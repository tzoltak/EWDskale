#' @title Łączenie kryteriów przy wykorzystniu funkcji mirt.
#' @description
#' Funkcja łączy kryteria korzystając z wyników skalowania funkcją mirt
#' @param dane dane w formie tabeli do obliczeń skalowania.
#' @param mirtSummary rezultat wywołania summary dla obiektu mirt.
#' @param wiazki_pyt_kryt tablica zawierająca informacje o wiązkach, pytaniach, kryteriach.
#' Parametr może być efektem działania funkcji \code{\link{pobierz_wiazki_pytania_kryteria}}.
#' @param prog liczba określająca próg, powyżej którego ma dochodzić do połączeń.
#' @param h2 zmienna logiczna, która określa czy wykorzystywać miarę h2
#' czy wartość drugiego konstruktu do łączeń kryteriów.
#' @details
#' Jeżeli parametr prog wynosi NULL to łączone są dwa kryteria o najwyższej mierze określanej przez parametr h2.
#' Jeżeli parametr wiazki_pyt_kryt równaja się NULL to podział na wiązki nie jest wykorzystywany przy łączeniu.
#' @return
#' Funkcja zwraca listę zawierającą:
#' \itemize{
#' \item \code{dane} - wynik połączenia;
#' \item \code{polaczenie} - informacja o wykonanym połączeniu;
#' \item \code{wyniki} - tablica zawirejąca statystyki kryteriów wykorzystywane przy wyborze kryteriów do połączenia;
#' }
#' @export
polacz_kryteria_mirt <- function(dane, mirtSummary, wiazki_pyt_kryt = NULL, prog = NULL, h2 = FALSE){

  if(!is.null(prog) & !is.numeric(prog)){
    stop("Parametr prog powinien być liczbą.")
  }

  if( is.null(wiazki_pyt_kryt)){
    warning("Informacja o wiązkach nie będzie uwzględniana przy łączeniu kryteriów.")
  }

  if( h2 & ! "h2" %in% names(mirtSummary) ){
    stop("Parametr mirtSummary nie zawiera elementu opisującego h2.")
  }

  if( !h2 &  ! "F_2" %in% colnames(mirtSummary$rotF) ){
    stop("Parametr mirtSummary nie zawiera danych skalowanie dla drugiego konstruktu.")
  }

  df = data.frame(kryterium = colnames(dane), mirtSummary$rotF, mirtSummary$h2)

  if( is.null(prog) ){

    if(h2 == FALSE){
      df = df[order(abs(df$F_2), decreasing = TRUE), ]
    } else {
      df = df[order(df$h2, decreasing = TRUE), ]
    }

    polaczenie = data.frame(kr1 = as.character(df$kryterium[1]),
                            kr2 = as.character(df$kryterium[2]),
                            F_2_1 = df$F_2[1],
                            F_2_2 = df$F_2[2],
                            h2_1 = df$h2[1],
                            h2_2 = df$h2[2])

    kryt1 = df$kryterium[1]
    kryt2 = df$kryterium[2]

    if( !is.null(wiazki_pyt_kryt)){
      wiazka1 = wiazki_pyt_kryt$id_wiazki[wiazki_pyt_kryt$id_kryterium == kryteria_z_nazw(kryt1)]
      wiazka2 = wiazki_pyt_kryt$id_wiazki[wiazki_pyt_kryt$id_kryterium == kryteria_z_nazw(kryt2)]

      if(wiazka1 != wiazka2){
        warning("Nie wykonano obliczeń ze względu na różne wiązki dla kryteriów: ", kryt1, ", ", kryt2, ".")
        return(list(dane = dane, polaczenie = NULL, wyniki = df))
      }
    }
    dane[, grepl( paste0("^", kryt1, "$"), colnames(dane))] = dane[, grepl( paste0("^", kryt1, "$"), colnames(dane))] +
                                                              dane[, grepl(paste0("^", kryt2, "$"), colnames(dane))]
    dane = dane[, !grepl( paste0("^", kryt2, "$"), colnames(dane))]
  } else{
    wektorKryt = if(h2){
      as.character(df$kryterium[ df$h2 > prog ])
    }else{
      as.character(df$kryterium[ abs(df$F_2) > prog])
    }

    if(length(wektorKryt) <= 1 ){
      message("Mniej niż 2 kryteria do połączenia.")
      return(list(dane = dane, polaczenie = NULL, wyniki = df))
    }

    krytNum = kryteria_z_nazw(wektorKryt)

    wiazki = if( !is.null(wiazki_pyt_kryt) ){
      wiazki_pyt_kryt[wiazki_pyt_kryt$id_kryterium %in% krytNum, c("id_wiazki", "id_kryterium")]
    } else{
      data.frame(id_wiazki = 0, id_kryterium = krytNum)
    }

    polaczenie = NULL
    for(wiazka in unique(wiazki$id_wiazki)){

      krytTemp = wiazki$id_kryterium[wiazki$id_wiazki == wiazka]

      if( length(krytTemp) <= 1  ){
        message("Wiazka: ", wiazka, " posiada jedno kryterium (", krytTemp, ") do połączenia." )
        next()
      }

      for(kryt2 in krytTemp[-1]){

        indK1 = which( df$kryterium == names(dane)[grepl(krytTemp[1], names(dane))] )
        indK2 = which( df$kryterium == names(dane)[grepl(kryt2, names(dane))] )
        pol =  data.frame( kr1 = as.character(df$kryterium[indK1]),
                           kr2 = as.character(df$kryterium[indK2]),
                           F_2_1 = df$F_2[indK1],
                           F_2_2 = df$F_2[indK2],
                           h2_1 = df$h2[indK1],
                           h2_2 = df$h2[indK2])

        polaczenie = rbind(polaczenie, pol)

        dane[, grepl(krytTemp[1], names(dane))] = dane[, grepl(krytTemp[1], names(dane))] + dane[, grepl(kryt2, names(dane))]
        dane = dane[, !grepl(kryt2, names(dane))]
      }
    }
  }

  return(list(dane = dane, polaczenie = polaczenie, wyniki = df))
}
#' @title Łączenie i skalowanie na podstawie wyników funkcji mirt.
#' @description
#' Funkcja skaluje i łączy kryteria korzystając ze skalowania funkcją mirt modelami IRT zawierającymi 2 zmienne ukryte.
#' @param dane dane.
#' @param wiazki_pyt_kryt tablica zawierająca informacje o wiązkach, pytaniach, kryteriach.
#' Patrz \code{\link{pobierz_wiazki_pytania_kryteria}}.
#' @param prog liczba określająca próg, powyżej którego ma dochodzić do połączeń.
#' @param h2 zmienna logiczna, która określa czy wykorzystywać miarę h2
#' czy wartość drugiego konstruktu do łączeń kryteriów.
#' @param maxIter maksymalna liczba iteracji.
#' @param czyLiczyc1 zmienna logiczna określająca
#' czy w wynikach działania funkcji ma się znaleźć rezultat skalowania funkcją mirt dla jednego kostruktu
#' czy dla dwóch konstruktów. Ustawienie parametru na FALSE przyśpiesza obliczenia.
#' @details
#' Jeżeli parametr prog wynosi NULL to łączone są dwa kryteria o najwyższej mierze określanej przez parametr h2.
#' Jeżeli parametr wiazki_pyt_kryt równaja się NULL to podział na wiązki nie jest wykorzystywany przy łączeniu.
#' @return
#' Funkcja zwraca listę zawierającą:
#' \itemize{
#' \item \code{wyniki} - wyniki skalowań;
#' \item \code{polaczenia} - informacja o wykonanych polaczeniach;
#' \item \code{czasy} - informacja o czasach oszczegónych iteracji;
#' \item \code{dane} - dane użyte do wykonania ostatniego skalowania;
#' }
#' @export
#' @import mirt
# skaluj_laczenie_mirt(dane, wiazki_pyt_kryt, prog=0.3, h2=FALSE, czyLiczyc1 = TRUE)
skaluj_laczenie_mirt <- function(dane, wiazki_pyt_kryt = NULL, prog = NULL, h2 = FALSE,
                                 maxIter = ifelse(is.null(prog), 10, 100), czyLiczyc1 = FALSE ){

  if(is.null(colnames(dane))){
    warning("Brak nazw kryteriów. Zostaną nadane nazwy domyślne.")
    colnames(dane) <- paste0("kr_",1:ncol(dane))
  }

  polaczeniaRet = NULL
  daneRand = dane
  mirtResults = list()
  czasy = NULL
  pvalues = NULL
  start.time = Sys.time()
  for(iterNum in 1:maxIter){
    message("Iteracja: ", iterNum)

    daneDoSkalowania = daneRand
    for(iC in seq_along(colnames(daneDoSkalowania))){
      daneDoSkalowania[, iC] = polacz_nieliczne(daneDoSkalowania[, iC], poziomy = 5)
    }

    mirtRet = mirt(daneDoSkalowania, model = 2)
    # Obowiązkowe 'mirt::'. Bez tego uruchamiana jest inna funkcja summary.
    mirtSummary = mirt::summary(mirtRet, rotate = "none", verbose=FALSE)

    df = data.frame(kryterium = colnames(daneRand), mirtSummary$rotF, mirtSummary$h2)

    mirtResults[ length(mirtResults) + 1 ] = if(czyLiczyc1){
      mirt(daneDoSkalowania, model =  1)
    } else{
      mirtRet
    }

    anovaPvalue = as.numeric(as.character(anova(mirtRet, mirtResults[[length(mirtResults)]])$p[2]))
    pvalues = rbind(pvalues, c(mirtResults[[length(mirtResults)]]@BIC, mirtRet@BIC, anovaPvalue))

    danePolacz = polacz_kryteria_mirt(daneRand, mirtSummary, wiazki_pyt_kryt, prog = prog, h2 = h2)
    if( is.null(danePolacz$polaczenie) ){
      break();
    }
    polaczeniaRet = rbind(polaczeniaRet, data.frame(iterNum = iterNum, danePolacz$polaczenie))
    daneRand = danePolacz$dane

    end.time = Sys.time()
    czasy = rbind(czasy, c(iterNum, end.time - start.time))
    start.time = end.time
  }

  colnames(pvalues) <- c("BIC 1", "BIC 2", "AN pvalue")

  mirtWyniki = list(wyniki = mirtResults, polaczenia = polaczeniaRet, czasy = czasy, dane = daneRand, pvalues = pvalues )
  return(mirtWyniki)
}
#' @title Wylicznie parametrów modeli IRT
#' @description
#' Funkcja wylicza parametry modelu IRT.
#' @param skalowanie_laczenie wyniki skalowania funkcją mirt.
#' @return
#' Funkcja zwraca listę zawierającą:
#' \itemize{
#' \item \code{theta} - umiejętności;
#' \item \code{a} - dyskryminacje;
#' \item \code{d} - średnia trudność;
#' }
#' @export
#' @import plyr
przygotuj_parametry_IRT <- function(skalowanie_laczenie){
  theta = list()
  param2PL_a = list()
  param2PL_d = list()
  for(ind in 1:length(skalowanie_laczenie$wyniki)){
    model = skalowanie_laczenie$wyniki[[ind]]
    theta[[ind]] = fscores(model, full.scores=TRUE, method="EAP")
    tmp = rbind.fill(lapply(coef(model)[names(coef(model)) != "GroupPars"], as.data.frame))
    param2PL_a[[ind]] = tmp$a1
    if("d" %in% names(tmp)){
      d = tmp$d
    } else{
      d = apply(tmp[, grepl("^d",names(tmp))],1, mean, na.rm = TRUE)
    }

    # d[is.na(d)] = apply(tmp[, grepl("^d.", colnames(tmp))], 1, function(x) mean(na.omit(x)))
    param2PL_d[[ind]] = d
  }

  ret = list(theta=theta, a=param2PL_a, d=param2PL_d )
  return(ret)
}

