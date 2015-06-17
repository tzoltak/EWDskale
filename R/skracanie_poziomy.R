#' @title Skracanie skal oceny
#' @description
#' Funkcja dokonuje skrótu skali oceny (pseudo)kryterium w oparciu o rozkład jego
#' wyników w grupie kalibracyjnej i informację o wszystkich możliwych wartościach,
#' jakie może przyjąć dane (pseudo)kryterium.
#' @param x data frame zawierający wyniki cześći egzaminu (typowo pobrane przy pomocy
#' funkcji \code{pobierz_czesc_egzaminu()} z pakietu \code{ZPD}) lub lista takich
#' data frame'ów
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy poziom
#' @param print wartość logiczna - czy pokazywać informacje o skracaniu?
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do
#' bazy (domyślnie "EWD")
#' @return Data frame, pasujący swoją strukturą jako argument \code{elementy} do
#' funkcji \code{edytuj_skale} z pakietu \code{ZPD} lub lista takich data frame'ów.
#' @export
skroc_skale <- function(x, maxLPozWyk=5, minLiczebnPozWyk=100, minOdsPozWyk=0.05, print=TRUE, zrodloDanychODBC="EWD") {
  stopifnot(is.data.frame(x) | is.list(x),
            is.numeric(maxLPozWyk)      , length(maxLPozWyk      ) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1)
  stopifnot(maxLPozWyk >= 2,
            minLiczebnPozWyk>=0, minLiczebnPozWyk < Inf,
            minOdsPozWyk >= 0, minOdsPozWyk <= 1)
  if (is.data.frame(x)) x = list(x)

  elementy = vector(mode="list", length=length(x))
  names(elementy) = names(x)
  for (j in names(x)) {
    nazwyKryteriow =  # właściwie kryteriów i pseudokryteriów
      names(x[[j]])[grep("^[kp]_[[:digit:]]+$", names(x[[j]]))]
    mozliweWartosci = pobierz_mozliwe_wartosci(nazwyKryteriow, zrodloDanychODBC)
    skroty = vector(mode="list", length=length(nazwyKryteriow))
    names(skroty) = nazwyKryteriow
    for (k in nazwyKryteriow) {
      message(j, ": ", k)
      temp = okresl_wzor_skracania(x[[j]][, k], mozliweWartosci[[k]],
                                   maxLPozWyk, minLiczebnPozWyk, minOdsPozWyk)
      #print(data.frame(as.list(temp$rozkladPrzed), check.names=FALSE), row.names=FALSE)
      print(matrix(temp$rozkladPrzed, ncol=1, dimnames=list(names(temp$rozkladPrzed), "liczebność")))
      if (length(unique(temp$poSkroceniu)) < length(unique(temp$przedSkroceniem))) {
        print(as.data.frame(temp[c("przedSkroceniem", "poSkroceniu")]), row.names=FALSE)
        print(matrix(temp$rozkladPo, ncol=1, dimnames=list(names(temp$rozkladPo), "liczebność")))
        skroty[[k]] = paste0(paste0(temp$przedSkroceniem, collapse=";"), "|",
                             paste0(temp$poSkroceniu, collapse=";"), collapse="")
      } else {
        skroty[[k]] = NA
      }
    }
    elementy[[j]] = data.frame(id_kryterium = nazwyKryteriow,
                               id_pseudokryterium = nazwyKryteriow,
                               id_skrotu = unlist(skroty))
    elementy[[j]] = within(elementy[[j]], {
      id_kryterium[grep("^p_", id_kryterium)] = NA
      id_pseudokryterium[grep("^k_", id_pseudokryterium)] = NA
    })
    elementy[[j]] = within(elementy[[j]], {
      id_kryterium = as.numeric(sub("^k_", "", id_kryterium))
      id_pseudokryterium = as.numeric(sub("^p_", "", id_pseudokryterium))
    })
  }
  if(length(elementy) == 1) elementy = elementy[[1]]
  return(elementy)
}
#' @title Okreslenie wzoru skrocenia skali oceny
#' @description
#' Funkcja dokonuje skrótu skali oceny (pseudo)kryterium w oparciu o rozkład jego
#' wyników w grupie kalibracyjnej i informację o wszystkich możliwych wartościach,
#' jakie może przyjąć dane (pseudo)kryterium.
#' @param x wektor z wynikami danego pytania
#' @param mozliweWartosci wektor z dopuszczalnymi poziomami dla danego pytania
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy poziom.
#' @return Funkcja zwraca trzyelementową listę, której elementy zawierają:
#' \itemize{
#'   \item{\code{przedSkroceniem} wartość parametru \code{mozliweWartosci},}
#'   \item{\code{poSkroceniu} wartości po skróceniu, odpowiadające wartościom pierwszego
#'         elementu,}
#'   \item{\code{rozkladPrzed} rozkład \code{x},}
#'   \item{\code{rozkladPo} rozkład \code{x} po skróceniu skali.}
#' }
okresl_wzor_skracania <- function(x, mozliweWartosci, maxLPozWyk=5, minLiczebnPozWyk=100, minOdsPozWyk=0.05) {
  stopifnot(is.numeric(x)               , length(x) > 0,
            is.numeric(mozliweWartosci) , length(mozliweWartosci) > 1,
            is.numeric(maxLPozWyk)      , length(maxLPozWyk      ) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1)
  stopifnot(maxLPozWyk >= 2,
            minLiczebnPozWyk>=0, minLiczebnPozWyk < Inf,
            minOdsPozWyk >= 0, minOdsPozWyk <= 1)
  stopifnot(all( unique(x) %in% c(mozliweWartosci, NA) ))

  rozklad = table(factor(x, levels=mozliweWartosci))
  rekodowanie = mozliweWartosci
  while(TRUE) {
    koniecSkracania =
      (length(rozklad) <= maxLPozWyk) &
      (min(rozklad) > minLiczebnPozWyk) &
      (min(rozklad / sum(rozklad)) > minOdsPozWyk)
    if (koniecSkracania | (length(rozklad) <= 2)) break

    doPolaczenia1 = which.min(as.numeric(rozklad))
    if (doPolaczenia1 == 1){
      doPolaczenia2 = 2
    } else if (doPolaczenia1 == length(rozklad)){
      doPolaczenia2 = length(rozklad) -1
    } else if (rozklad[doPolaczenia1 - 1] <= rozklad[doPolaczenia1 + 1]) {
      doPolaczenia2 = doPolaczenia1 - 1
    } else {
      doPolaczenia2 = doPolaczenia1 + 1
    }
    rekodowanie[rekodowanie == as.numeric(names(rozklad)[doPolaczenia1])] =
      rekodowanie[rekodowanie == as.numeric(names(rozklad)[doPolaczenia2])][1]
    rozklad[doPolaczenia2] = rozklad[doPolaczenia2] + rozklad[doPolaczenia1]
    rozklad = rozklad[-doPolaczenia1]
  }
  # podaje explicite argument levels, żeby dmuchać na zimne z kolejnością
  rekodowanie = as.numeric(factor(rekodowanie, levels=unique(rekodowanie))) - 1
  names(rozklad) = unique(rekodowanie)
  return(list(przedSkroceniem = mozliweWartosci,
              poSkroceniu = rekodowanie,
              rozkladPrzed = table(factor(x, levels=mozliweWartosci)),
              rozkladPo = rozklad))
}
#' @title Wartosci mozliwe do przyjecia przez (pseudo)kryterium
#' @description
#' Funkcja pobiera z bazy i zwraca informacje o wszystkich dopuszczalnych wartościach
#' kryteriów i/lub pseudokryteriów.
#' @param nazwyKryteriow wektor tekstowy z nazwami postaci "k_idKryterium" lub
#' "p_idPseudokryterium" (typowo nazwy zmiennych z data frame'a zwróconego przez
#' funkcję \code{obierz_czesc_egzaminu()} z pakietu \code{ZPD}).
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do
#' bazy (domyślnie "EWD")
#' @return lista wektorów liczbowych
#' @export
pobierz_mozliwe_wartosci <- function(nazwyKryteriow, zrodloDanychODBC="EWD"){
  stopifnot(is.character(nazwyKryteriow), length(nazwyKryteriow) > 0,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1)
#   maska = !grepl("^[kp]_[[:digit:]]+$", nazwyKryteriow)
#   if (any(maska)) stop("Niepoprawne nazwy (pseudo)kryteriów: ",
#                        paste(nazwyKryteriow[maska], sep=","))
#   idKryteriow       = as.numeric(gsub("^k_", "",
#                                       nazwyKryteriow[grepl("^k_", nazwyKryteriow)]))
#   idPseudokryteriow = as.numeric(gsub("^p_", "",
#                                       nazwyKryteriow[grepl("^p_", nazwyKryteriow)]))
#   if (length(idPseudokryteriow) > 0) {
#     zapytanie = "SELECT id_pseudokryterium, id_kryterium, wartosc
#     FROM pseudokryteria_oceny
#     JOIN pseudokryteria_oceny_kryteria USING (id_pseudokryterium)
#     JOIN kryteria_oceny USING (id_kryterium)
#     JOIN sl_schematy_pkt USING (schemat_pkt)
#     JOIN sl_schematy_pkt_wartosci USING (schemat_pkt)
#     WHERE id_pseudokryterium = ?"
#     tryCatch({
#       P = odbcConnect(zrodloDanychODBC)
#       pseudokryteria = sqlExecute(P, zapytanie, data = data.frame(idPseudokryteriow),
#                                   fetch = TRUE)
#     },
#     error = stop,
#     finally = odbcClose(P)
#     )
#     pseudokryteria = dlply(pseudokryteria, ~id_pseudokryterium,
#                            function(x) {
#                              x = dlply(x, ~id_kryterium, function(x) {return(x$wartosc)})
#                              return(sort(unique(rowSums(expand.grid(x)))))
#                            })
#     names(pseudokryteria) = paste0("p_", names(pseudokryteria))
#   } else {
#     pseudokryteria = NULL
#   }
#   if (length(idKryteriow) > 0) {
#     zapytanie = "SELECT id_kryterium, wartosc
#                  FROM kryteria_oceny
#                    JOIN sl_schematy_pkt USING (schemat_pkt)
#                    JOIN sl_schematy_pkt_wartosci USING (schemat_pkt)
#                  WHERE id_kryterium = ?"
#     tryCatch({
#       P = odbcConnect(zrodloDanychODBC)
#       kryteria = sqlExecute(P, zapytanie, data = data.frame(idKryteriow),
#                             fetch = TRUE)
#     },
#     error = stop,
#     finally = odbcClose(P)
#     )
#     kryteria = dlply(kryteria, ~id_kryterium, function(x) {return(x$wartosc)})
#     names(kryteria) = paste0("k_", names(kryteria))
#   } else {
#     kryteria = NULL
#   }
#   temp = unlist(list(kryteria, pseudokryteria), recursive=FALSE)
#   return(temp[nazwyKryteriow])
  }
