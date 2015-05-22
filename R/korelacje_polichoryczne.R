#' @title Skalowanie i łączenie kryteriów przy użyciu korelacji polichorycznych
#' @description
#' Funkcja w pętli uruchamia skalowania. Każde kolejne skalowanie jest wykonywane
#' na zbiorze danych z dwoma połączonymi kryteriami. Łączenie kryteriów odbywa się na podstawie
#' korelacji polichorycznych.
#' @param dane ramka danych zawierająca wyniki egzaminów.
#' @param proceduraEG procedura skalowania.
#' @param korelacjaWiazki tablica zawierająca kolumny kr1 oraz kr2 z wartościami kryteriów do połącznia
#' oraz kolumnę numer określającą kolejność obliczeń korelacji. Jeżeli korelacjaWiazki=NULL
#' to funkcja liczy korelacje polichoryczne przed każdym połączeniem.
#' @param nazwaSkalowania ciąg znaków opisujący skalowanie.
#' @param ileKrokow liczba kolejnych skalowań, które ma wykonać funkcja.
#' @param minMiara wartość miary 1-R2, powyżej której obliczenia są przerywane.
#' @details
#' Parametr ileKrokow określa liczbę połączeń, które zostaną wykonane na danych. Funkcja także wykonuje
#' skalowanie dla danych bez połączeń. Jeżeli ileKrokow wynosi 0 to wtedy zostaną wykonane obliczenia dla danych nie połączonych.
#' Definicja połączeń jest zawarta w tablicy korelacjaWiazki. Funkcja łączy kryteria określone przez początkowe
#' wiersze tej tablicy.
#' @return
#' Funkcja zwraca obiekt klasy WynikSkalowania, który zawiera elementy:
#' \itemize{
#' \item \code{skalowania} - lista przeprowadzonych skalowań,
#' \item \code{korelacjaWiazki} - kryteria do połączenia,
#' \item \code{kolejnyIndeks} - indeks kolejnego skalowania,
#' \item \code{wartosciStartowe} - startowe wartości dla kolejnego skalowania,
#' \item \code{procedura} - procedura użyta przy ostatnim skalowaniu,
#' \item \code{nazwaSkalowania} - nazwa skalowania,
#' \item \code{dane} - dane użyte przy ostatnich obliczeniach,
#' \item \code{polaczenie} - wykonane połączenia oraz miary odpowiadające im,
#' }
#' @export
skaluj_polichorycznie <- function(dane, proceduraEG, korelacjaWiazki, nazwaSkalowania,
                                  ileKrokow = ifelse(is.null(korelacjaWiazki),
                                                     1000, nrow(korelacjaWiazki)),
                                  minMiara = NULL){

  wynikSkalowania = initializuj_skalowanie_polich(dane, proceduraEG,
                                                                  korelacjaWiazki, nazwaSkalowania,
                                                                  minMiara)
  n = ileKrokow
  while(n >= 0){
    index = wynikSkalowania$kolejnyIndeks
    wynikSkalowania <- kolejny_krok_polich(wynikSkalowania)
    n=n-1
    if(wynikSkalowania$kolejnyIndeks == index){
      break
    }
  }
  return(wynikSkalowania)
}
#' @title Zainiciowanie obliczeń.
#' @description
#' Funkacja tworzy obiekt klasy WynikSkalowania używany do dalszych obliczeń.
#' @param dane ramka danych zawierająca wyniki egzaminów.
#' @param procedura procedura skalowania egzaminów.
#' @param korelacjaWiazki tablica zawierająca kolumny kr1 oraz kr2 z wartościami kryteriów do połącznia
#' oraz kolumnę numer określającą kolejność obliczeń korelacji.
#' @param nazwaSkalowania ciąg znaków opisujący skalowanie.
#' @param minMiara patrz definicja \link{skaluj_polichorycznie}.
#' @return
#' Patrz opis funkcji \code{\link{skaluj_polichorycznie}}.
initializuj_skalowanie_polich <- function(dane, procedura, korelacjaWiazki, nazwaSkalowania, minMiara){
  ret = list(wyniki = list(), korelacjaWiazki = korelacjaWiazki,
             kolejnyIndeks = 0, wartosciStartowe = NULL,
             procedura = procedura, nazwaSkalowania = nazwaSkalowania,
             dane = dane, polaczenie = NULL,
             metoda = ifelse(is.null(procedura), "mirt", "mplus"), minMiara = minMiara)
  class(ret) <- "WynikSkalowania"
  return(ret)
}
#' @title Jedno skalowanie z połączeniem danych
#' @description
#' Funkcja wykonuje jednen krok skalowania wraz z pobocznymi operacjami, jak połączenie danych
#' oraz zwiększenie indeksu.
#' @param wynikSkalowania element klasy WynikSkalowania.
#' @details
#' Patrz detale funkcji \code{\link{skaluj_polichorycznie}}.
#' @return
#' Element klasy WynikSkalowania.
kolejny_krok_polich <- function(wynikSkalowania){

  if( class(wynikSkalowania) != "WynikSkalowania" ){
    stop("Argument nie jest klasy 'WynikSkalowania'")
  }
  index = wynikSkalowania$kolejnyIndeks

  cat("Index: ", index, "\n")

  korelacjaWiazki =  wynikSkalowania$korelacjaWiazki

  if( !is.null(korelacjaWiazki) && index > nrow(korelacjaWiazki)){
    warning("Wykonano już wszystkie połączenia.")
    return (wynikSkalowania)
  }

  indexStary = index

  if(is.null(korelacjaWiazki)){
    daneKor = wynikSkalowania$dane[,grepl("^([[:alnum:]]+_)[[:digit:]]+", names(wynikSkalowania$dane))]
    kryteria = as.numeric(gsub("^[[:alnum:]]+_", "", names(daneKor)))
    wiazki_pyt_kryt = pobierz_wiazki_pytania_kryteria(kryteria)
    korelacjaWiazki = policz_korelacje_wiazki(daneKor, wiazki_pyt_kryt)
    index = 1;
  }

  if(indexStary == 0 ){
    nazwaSkalowaniaTemp = paste0(wynikSkalowania$nazwaSkalowania, "_bez_pol")
  } else{
    nazwaSkalowaniaTemp = paste0(wynikSkalowania$nazwaSkalowania, "_pol_",
                                 paste(korelacjaWiazki[index, c("kr1", "kr2")], collapse="_"))
  }

  pol = korelacjaWiazki[index, ]

  if(is.null(korelacjaWiazki)){
    message("Wszystkie kryteria są we wszystkich wiązkach. Koniec.")
    return (wynikSkalowania)
  }

  if(pol$miara > wynikSkalowania$minMiara){
    message("Miara przekracza: ", wynikSkalowania$minMiara,". Koniec.")
    return (wynikSkalowania)
  }

  if(is.null(wynikSkalowania$procedura )){
    message("Skalowanie z użyciem funkcji mirt.")
    tempSkaluj = skaluj_krok_mirt(wynikSkalowania$dane,
                                                  wynikSkalowania$nazwaSkalowania, korelacjaWiazki,
                                                  index)
  } else{
    tempSkaluj = skaluj_krok(wynikSkalowania$dane, wynikSkalowania$procedura,
                             wynikSkalowania$nazwaSkalowania, korelacjaWiazki,
                             index, wynikSkalowania$wartosciStartowe)
  }

  # dostosowanie do przestarzałych obiektów wynikSkalowania:
  # maskiZmienne = grepl("^(g[h]_)[[:digit:]]{1,4}$|^id_obs$", names(wynikSkalowania$dane))
  # proceduraEG = podmien_wartosci_lista( wynikSkalowania$procedura, wynikSkalowania$wartosciStartowe, "wartosciStartowe")
  # tempSkaluj = skaluj_krok(wynikSkalowania$dane[, maskiZmienne], proceduraEG, wynikSkalowania$nazwaSkalowania, korelacjaWiazki, index, wynikSkalowania$wartosciStartowe)

  retList = wynikSkalowania$wyniki

  retList[ length(retList) + 1 ] = tempSkaluj$wynikSkalowania
  names(retList)[length(retList)] = nazwaSkalowaniaTemp

  ret = wynikSkalowania
  ret$wyniki = retList
  ret$kolejnyIndeks = indexStary + 1

  # ret$wartosciStartowe = tempSkaluj$wartosciStartowe[, c("typ", "zmienna1", "zmienna2", "wartosc")]
  ret$wartosciStartowe = NULL

  ret$procedura = tempSkaluj$procedura
  ret$dane = tempSkaluj$dane

  ret$polaczenie = rbind(wynikSkalowania$polaczenie, tempSkaluj$polaczenie)
  return(ret)
}
#' @title Łączenie kryteriów
#' @description
#' Funkcja łączy w danych dwie kolumny zawierające wyniki kryteriów.
#' @param dane ramka danych zawierająca wyniki egzaminów.
#' @param korelacjaWiazki tablica zawierająca kolumny kr1 oraz kr2 z wartościami kryteriów do połącznia
#' oraz kolumnę numer określającą kolejność obliczeń korelacji.
#' @param index numer określający wiersz tablicy korelacjaWiazki, który wskazuje kryteria do połączenia.
#' @return
#' Funkcja zwaca ramkę danych, na której wykonano operację połączenia.
polacz_dane <- function(dane, korelacjaWiazki = NULL, index = 0 ){
  if( is.null(korelacjaWiazki) | index == 0 ){
    return(dane)
  }

  if( is.null(korelacjaWiazki) | !all(c("kr1","kr2","numer") %in% colnames(korelacjaWiazki)) ){
    stop("Niepoprawna forma korelacji.")
  }

  kryterium_do_polaczenia <- function(pol_numer, pol_kryt, korelacjaWiazki){
    if(pol_kryt > 0) {
      return(pol_kryt)
    }
    pol = korelacjaWiazki[which( pol_numer + pol_kryt == korelacjaWiazki$numer), ]
    return (kryterium_do_polaczenia(pol$numer, pol$kr1, korelacjaWiazki))
  }

  pol = korelacjaWiazki[index, ]
  pyt1 = kryterium_do_polaczenia(pol$numer, pol$kr1, korelacjaWiazki)
  pyt2 = kryterium_do_polaczenia(pol$numer, pol$kr2, korelacjaWiazki)

  if( sum(grepl(paste0("_",pyt1,"$"), names(dane)))!=1 |
        sum(grepl(paste0("_",pyt2,"$"), names(dane)))!=1 ){
    stop("Zle zdefiniowane polaczenie: \n", paste(pol, collapse=" " ), "\n")
  }

  dane[, grepl(paste0("_",pyt1,"$"), names(dane))] = dane[, grepl(paste0("_",pyt1,"$"), names(dane))] + dane[, grepl(paste0("_", pyt2,"$"), names(dane))]
  dane = dane[, !grepl(paste0("_",pyt2,"$"), names(dane))]
  attr(dane, "polaczenie") = c(pyt1, pyt2, pol$miara)

  return(dane)
}
#' @title Łączenie danych kryteriów oraz ich skalowanie.
#' @description
#' Funkcja, która wykonuje łączenie danych oraz skalowanie z użyciem Mplusa.
#' @param dane ramka daych kryteriów (przed połączeniem).
#' @param proceduraEG procedura skalowania.
#' @param opisSkalowania ciąg znaków opisujący skalowanie.
#' @param korelacjaWiazki ramka danych zawierająca korelacje polichoryczne.
#' @param index numer wiersza ramki korelacjeWiazki, który zawiera informacje o połączeniach.
#' @param wartosciStartowe wartości startowe skalowania. Domyślna wartość to NULL.
#' @return
#' Funkcja zwraca listę z wynikami skalowania, wartości startowe, dane po połączeniu, procedurę oraz połączenie.
skaluj_krok <- function(dane, proceduraEG, opisSkalowania, korelacjaWiazki, index, wartosciStartowe = NULL)
{
  daneTmp = polacz_dane(dane, korelacjaWiazki, index)
  zmienne = names(daneTmp)[grep("^gh_[[:digit:]]", names(daneTmp))]
  proceduraEG = podmien_wartosci_lista(proceduraEG, zmienne, "zmienne")

  # potencjalny błąd z kolumną id_obs
  daneDoSkalowania = daneTmp
  for(iC in seq_along(colnames(daneDoSkalowania))){
    daneDoSkalowania[, iC] = polacz_nieliczne(daneDoSkalowania[, iC], poziomy = 5)
  }
  wynikSkalowania = suppressWarnings(skaluj(daneDoSkalowania, proceduraEG, "id_obs", opisSkalowania))

  return( list(wynikSkalowania = wynikSkalowania,
               wartosciStartowe = wynikSkalowania[[1]]$kalibracja1$parametry$surowe,
               dane = daneTmp, procedura = proceduraEG, polaczenie = attr(daneTmp, "polaczenie")
  )
  )
}
#' @title Łączenie danych kryteriów oraz ich skalowanie.
#' @description
#' Funkcja, która wykonuje łączenie danych oraz skalowanie w wykorzystaniem pakietu mirt.
#' @param dane ramka daych kryteriów (przed połączeniem).
#' @param opisSkalowania ciąg znaków opisujący skalowanie.
#' @param korelacjaWiazki ramka danych zawierająca korelacje polichoryczne.
#' @param index numer wiersza ramki korelacjeWiazki, który zawiera informacje o połączeniach.
#' @return
#' Funkcja zwraca listę z wynikami skalowania, wartości startowe, dane po połączeniu, procedurę oraz połączenie.
skaluj_krok_mirt <- function(dane, opisSkalowania, korelacjaWiazki, index)
{
  daneTmp = polacz_dane(dane, korelacjaWiazki, index)
  polaczenie = attr(daneTmp, "polaczenie")
  zmienne = names(daneTmp)[grep("^gh_[[:digit:]]", names(daneTmp))]

  daneTmp = daneTmp[, colnames(daneTmp) != "id_obs"]
  message(names(daneTmp))

  daneDoSkalowania = daneTmp
  for(iC in seq_along(colnames(daneDoSkalowania))){
    daneDoSkalowania[, iC] = polacz_nieliczne(daneDoSkalowania[, iC], poziomy = 5)
  }
  wynikSkalowania = mirt(daneDoSkalowania, model = 1)

  return( list(wynikSkalowania = wynikSkalowania,
               wartosciStartowe = NULL,
               dane = daneTmp, procedura = NULL, polaczenie = polaczenie
  )
  )
}
#' @title Zmiana wartości jednego z węzłów listy
#' @description
#' Funkcja zmienia rekurencyjnie wartości węzłów o nazwie określonej w parametrze "doZmiany".
#' @param wezel lista zawierająca, której węzły chcemy zmienić.
#' @param nowaWartosc nowa wartość dla zmienianego elementu
#' @param doZmiany nazwa węzła do zmiany.
#' @return
#' Zmodyfikowana lista.
podmien_wartosci_lista <- function(wezel, nowaWartosc, doZmiany){

  if(length(wezel) == 0) {
    return(wezel)
  }

  czyZmieniac =  names(wezel) == doZmiany
  if( any(czyZmieniac) ){
    startowe = which(czyZmieniac)
    for(m in startowe){
      if(is.null(nowaWartosc)){
        wezel[m] <- list(NULL)
      } else{
        wezel[[m]] = nowaWartosc
      }
    }
  }

  nieStartowe = which(!czyZmieniac)
  for(k in nieStartowe){
    if( is.list(wezel[[k]]) ){
      wezel[[k]] = podmien_wartosci_lista(wezel[[k]], nowaWartosc, doZmiany)
    }
  }

  return(wezel)
}
#' @title Procedury skalowania egzaminow.
#' @description
#' Procedura skalowania części humanistycznej egzaminu gimnazjalnego.
#' Funkcja przygotowuj opis procedury skalowania do użycia przez funkcję \code{\link{skaluj}}.
#' @param nazwyZmiennych nazwy zmiennych z data.frame'a z danymi, na których ma być prowadzona estymacja
#' @param parametryGH wartości startowe parametrów.
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @return lista, która zostanie użyta jako argument \code{opisProcedury} funkcji \code{\link{skaluj}}
#' @export
procedura_eg_hum <- function(nazwyZmiennych, parametryGH=NULL, processors=3) {
  procedura=list(
    "czesc hum."=list(
      czescPomiarowa=list(
        gh=list(
          zmienne=nazwyZmiennych[grep("^gh_[[:digit:]]", nazwyZmiennych)],
          var1=TRUE,
          rasch=FALSE,
          kryteriaUsuwania=list(
            dyskryminacjaPonizej=0.2,
            istotnoscPowyzej=1,
            nigdyNieUsuwaj=NULL
          ),
          wartosciStartowe=NULL,
          wartosciZakotwiczone=NULL
        )
      ),
      parametry=list(
        estimator="MLR",
        processors=processors,
        integration="STANDARD (15)",
        fscores=TRUE
      )
    )
  )
  if (!is.null(parametryGH)) {
    procedura$"czesc hum."$czescPomiarowa$gh=list(
      zmienne = parametryGH$zmienna2[parametryGH$typ=="by" & parametryGH$zmienna1=="gh"],
      var1=TRUE,
      rasch=FALSE,
      kryteriaUsuwania=NULL,
      wartosciStartowe = parametryGH[, c("typ", "zmienna1", "zmienna2", "wartosc")],
      wartosciZakotwiczone = NULL
    )
  }
  return(procedura)
}
#' @title Pobierz: wiązki, pytania, kryteria
#' @description
#' Funkcja pobiera dane dla zdefiniowanych kryteriów.
#' @param kryteria wektor zawietający numery kryteriów.
#' @param zrodloODBC źródło danych ODBC.
#' @return
#' Funkcja zwraca ramkę danych.
#' @export
pobierz_wiazki_pytania_kryteria <- function(kryteria, zrodloODBC="EWD"){
  if(is.null(zrodloODBC)){
    ret = data.frame(id_wiazki=rep(1,length(kryteria)),
                     id_pytania=rep(1,length(kryteria)),
                     id_kryterium = kryteria)
    return(ret)
  }

  P = odbcConnect(zrodloODBC)
  zapytanie = paste0("select distinct id_wiazki, id_pytania, id_kryterium
                     from pytania
                     join kryteria_oceny using (id_pytania)
                     where id_kryterium in ", paste0("(", paste(kryteria,collapse=","), ")"),
                     " order by id_pytania, id_kryterium, id_wiazki"
  )
  ret = sqlQuery(P, zapytanie)
  odbcClose(P)

  return(ret)
}
# Docelowa funkcja, która w przyszłości zastąpi powyższą.
# pobierz_wiazki_pytania_kryteria2 <- function(testy, zrodloODBC="EWD"){
#   P = odbcConnect(zrodloODBC)
#
#   zapytanie = "select distinct id_wiazki, id_pytania, id_kryterium
#   from pytania
#   join kryteria_oceny using (id_pytania)
#   join testy_kryteria using (id_kryterium)
#   where id_testu in (?)
#   order by id_pytania, id_kryterium, id_wiazki"
#   ret =  sqlPrepare(P, zapytanie, data = testy, fetch = TRUE)
#   odbcClose(P)
#   return( ret )
# }
#' @title Kryteria z nazw
#' @description
#' Funkcja wyznacza numer kryteriów z ich nazw.
#' @param nazwy nazwy kryteriów.
#' @return
#' Funkcja zwraca wektor liczb oznaczających numery kryteriów.
#' @export
kryteria_z_nazw <- function(nazwy){
  kryt_nazwy = nazwy[grepl("^([[:alnum:]]+_)+[[:digit:]]+", nazwy)]
  return( as.numeric(gsub("^[[:alnum:]]+_", "", kryt_nazwy)) )
}
#' @title Korelacje polichoryczne dla tabeli danych
#' @description
#' Funkcja liczy macierz korelacji polichorycznych.
#' @param dane dane w formie tabeli do obliczeń korelacji polichorycznych
#' @return
#' Funkcja zwraca wyniki w postaci macierzy kwadratowej o rozmiarze równym liczbie kolumn
#' tablicy danes.
#' @import polycor
korelacjePolihoryczne <- function(dane){
  polyRet = array(0,c(ncol(dane),ncol(dane)))
  for(k in 1:ncol(dane)){
    for(m in 1:ncol(dane)){
      if(k <= m){
        polyRet[k,m] = 0;
        next()
      }
      polyRet[k,m] = polychor(dane[,k], dane[,m])

      cat(paste0("\r", k," ",m," :",polyRet[k,m]))
    }
  }
  rownames(polyRet) <- colnames(dane)
  colnames(polyRet) <- colnames(dane)
  return(polyRet)
}
#' @title Liczenie korelacji polichorycznych
#' @description
#' Funkcja liczy korelacje polichoryczne dla kryteriów zebranych w wiązki.
#' @param dane_pytan ramka danych zawierająca wyniki kryteriów.
#' @param wiazki_pyt_kryt ramka danych, która zawiera informacje o przynależności pytań do poszczególnych wiązek.
#' @return
#' Funkcja zwraca ramkę danych, w której znajdują się obliczone korelacje polichoryczne.
#' @import RODBCext
policz_korelacje_wiazki <- function (dane_pytan, wiazki_pyt_kryt){
  kryt_nazwy = colnames(dane_pytan)[grepl("^([[:alnum:]]+_)+[[:digit:]]+", colnames(dane_pytan))]
  kryt = as.numeric(gsub("^([[:alnum:]]+_)+", "", kryt_nazwy))
  
  sprawdz_zgodnosc_kryteriow <- function(kryt1, kryt2, nazwa1, nazwa2, error=TRUE){
    if(  length(fInds <- which( ! unique(kryt1) %in% unique(kryt2))) !=0  ){
      if(error){
        stop(paste0(nazwa1, " i ", nazwa2," nie pokrywają się.",
                    " Brakujące ", nazwa1, ":\n"),
             paste(unique(kryt1)[fInds], collapse="\n"))
      } else {
        warning(paste0(nazwa1, " i ", nazwa2, " nie pokrywają się.",
                       " Brakujące ", nazwa1, ":\n"),
                paste(unique(kryt1)[fInds], collapse="\n"))
      }
    }
    invisible(NULL)
  }

  sprawdz_zgodnosc_kryteriow(kryt, wiazki_pyt_kryt$id_kryterium, "Kryteria z pytan", "Kryteria z wiazek")
  sprawdz_zgodnosc_kryteriow(wiazki_pyt_kryt$id_kryterium, kryt, "Kryteria z wiazek", "Kryteria z pytan")

  daneZad = dane_pytan[, grepl("^([[:alnum:]]+_)+[[:digit:]]+", colnames(dane_pytan))]
  wiazki = unique(wiazki_pyt_kryt$id_wiazki)

  polaczenia = NULL
  for(ww in wiazki){
    kryteriaTmp = wiazki_pyt_kryt$id_kryterium[wiazki_pyt_kryt$id_wiazki == ww ]
    indKryteria = as.numeric(gsub("^([[:alnum:]]+_)+", "", colnames(daneZad))) %in% kryteriaTmp

    if(sum(indKryteria)==1){
      next()
    }
    korsPoli = korelacjePolihoryczne(daneZad[, indKryteria])

    hc = hclust(as.dist(1-korsPoli^2))
    mh = data.frame(hc$merge)

    nedInds = mh < 0
    mh[mh>0] =  mh[mh>0]  -  outer(1:nrow(mh), rep(1, ncol(mh)), "*")[mh>0]
    mh[nedInds] = kryteriaTmp[-mh[nedInds]]

    polaczenia = rbind(polaczenia, cbind(mh, hc$height))
  }

  if(is.null(polaczenia)){
    return(polaczenia)
  }

  polaczeniaTab = cbind(1:nrow(polaczenia), polaczenia)
  colnames(polaczeniaTab) <- c("numer", "kr1", "kr2", "miara")

  czyToSamoPytanie = numeric(nrow(polaczeniaTab))
  nrPytaniaTmp = numeric(nrow(polaczeniaTab))
  for(i in 1:nrow(polaczeniaTab)){
    pol = polaczeniaTab[i,c("kr1","kr2")]

    pyt1 = ifelse(pol$kr1 > 0, wiazki_pyt_kryt$id_pytania[ wiazki_pyt_kryt$id_kryterium == pol$kr1] ,
                  czyToSamoPytanie[i + pol$kr1] )
    pyt2 = ifelse(pol$kr2 > 0, wiazki_pyt_kryt$id_pytania[ wiazki_pyt_kryt$id_kryterium == pol$kr2] ,
                  czyToSamoPytanie[i + pol$kr2] )

    if( pyt1 == pyt2 ){
      czyToSamoPytanie[i] = pyt1
    }else{
      czyToSamoPytanie[i] = -1
    }
  }
  polaczeniaTab = cbind(polaczeniaTab, czyToSamoPytanie)

  return(polaczeniaTab[order(polaczeniaTab$miara), ])
}
