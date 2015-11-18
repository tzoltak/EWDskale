#' @title Skracanie skal oceny
#' @description
#' Funkcja dokonuje skrótu skali oceny (pseudo)kryteriów w oparciu o rozkład
#' ich wyników w grupie kalibracyjnej i informację o wszystkich możliwych
#' wartościach, jakie może przyjąć dane (pseudo)kryterium.
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy
#' poziom
#' @param print wartość logiczna - czy pokazywać informacje o skracaniu?
#' @return data frame
#' Kolumna \code{elementy} zawiera data frame'y które mogą zostać użyte
#' jako argument funkcji \code{\link[ZPDzapis]{edytuj_skale}}.
#' @export
skroc_skale_oceny = function(skale, katalogDane = "dane surowe/",
                             maxLPozWyk = 5, minLiczebnPozWyk = 100,
                             minOdsPozWyk = 0.05, print = TRUE) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1),
            is.character(katalogDane),    length(katalogDane) == 1,
            is.numeric(maxLPozWyk),       length(maxLPozWyk) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1)
  stopifnot(maxLPozWyk >= 2,
            minLiczebnPozWyk >= 0, minLiczebnPozWyk < Inf,
            minOdsPozWyk >= 0, minOdsPozWyk <= 1)
  if (!dir.exists(katalogDane)) {
    stop("Katalog '", katalogDane, "' nie istnieje.")
  }

  # pobieranie danych o kryteriach
  if (is.character(skale)) {
    skale = pobierz_skale(polacz(), doPrezentacji = NA) %>%
      collect() %>%
      filter_(~grepl(skale, opis_skali), ~is.na(czesc_egzaminu)) %>%
      select_(~id_skali, ~opis_skali, ~rodzaj_egzaminu, ~czesc_egzaminu, ~rok) %>%
      distinct()
  }
  if (nrow(skale) == 0) {
    stop("Nie znaleziono żadnych skal, których opis pasowałby do podanego wyrażenia regularnego.")
  }

  skale = group_by_(skale, ~id_skali, ~czesc_egzaminu) %>%
    do_(.dots = setNames(list(~skroc_skale_oceny_w_ramach_skali(., katalogDane,
                                                                maxLPozWyk,
                                                                minLiczebnPozWyk,
                                                                minOdsPozWyk, print)),
                         "elementy"))
  return(skale)
}
#' @title Skracanie skal oceny
#' @description Koń roboczy wywoływany przez \code{\link{skroc_skale_oceny}}.
#' @param x pojedynczy wiersz data frame'a, opisujący skalę
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param maxLPozWyk maksymalna liczba poziomów pytania
#' @param minLiczebnPozWyk minimalna liczebność obserwacji w każdym z poziomów
#' @param minOdsPozWyk minimalny odsetek obserwacji, które ma zawierać każdy
#' poziom
#' @param print wartość logiczna - czy pokazywać informacje o skracaniu?
#' @return Data frame, pasujący swoją strukturą jako argument \code{elementy}
#' do funkcji \code{edytuj_skale} z pakietu \code{ZPD} lub lista takich data
#' frame'ów.
#' @import ZPD
#' @export
skroc_skale_oceny_w_ramach_skali = function(x, katalogDane = "../dane surowe/",
                                            maxLPozWyk = 5, minLiczebnPozWyk = 100,
                                            minOdsPozWyk = 0.05, print = TRUE) {
  stopifnot(is.data.frame(x), nrow(x) == 1,
            is.character(katalogDane),    length(katalogDane) == 1,
            is.numeric(maxLPozWyk),       length(maxLPozWyk) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1)
  stopifnot(maxLPozWyk >= 2,
            minLiczebnPozWyk >= 0, minLiczebnPozWyk < Inf,
            minOdsPozWyk >= 0, minOdsPozWyk <= 1)
  if (!dir.exists(katalogDane)) {
    stop("Katalog '", katalogDane, "' nie istnieje.")
  }

  message("Skala '", x$opis_skali, "', id_skali = ", x$id_skali, ": ",
          x$rodzaj_egzaminu, " ", x$rok, ": ", x$czesc_egzaminu)
  # wczytywanie danych z wynikami egzaminu
  message("  Wczytywanie danych.")
  dane = wczytaj_wyniki_surowe(katalogDane, x$rodzaj_egzaminu,
                               x$czesc_egzaminu, x$rok, x$id_skali)
  maskaObserwacje = with(dane, {populacja_wy & !pomin_szkole})
  dane = dane[, grep("^[kp]_", names(dane))]

  # pobieranie schematów punktowania zadań
  kryteria = suppressMessages(
    pobierz_kryteria_oceny(polacz(), testy = FALSE) %>%
      filter_(~id_skali == x$id_skali) %>%
      arrange_(~kolejnosc_w_skali) %>%
      select_(~kryterium, ~schemat_pkt) %>%
      distinct() %>%
      collect()
  )
  kryteria$schemat_pkt = strsplit(kryteria$schemat_pkt, "-", fixed = TRUE) %>%
    lapply(as.numeric) %>%
    lapply(function(x) {
      if (length(x) > 1) {
        return(x)
      } else if (!is.na(x)) {
        return(0:x)
      } else {
        return(x)
      }
    })
  stopifnot(all(names(dane) %in% kryteria$kryterium),
            all(kryteria$kryterium %in% names(dane)))
  dane = dane[, kryteria$kryterium]

  # uzupełnianie schematów punktowania dla pseudokryteriow
  maska = lapply(kryteria$schemat_pkt, function(x) {return(all(is.na(x)))}) %>%
    unlist() %>% which()
  for (i in maska) {
    kryteria$schemat_pkt[[i]] = sort(unique(na.omit(dane[, i][[1]])))
  }
  # dopiero teraz zawężamy zakres danych do "populacji wzorcowej"
  dane = subset(dane, maskaObserwacje)
  # na wszelki wypadek
  maska = lapply(kryteria$schemat_pkt, function(x) {return(!(0 %in% x))}) %>%
    unlist() %>% which()
  for (i in maska) {
    kryteria$schemat_pkt[[i]] = c(0, kryteria$schemat_pkt[[i]])
  }

  # samo skracanie skal
  message("  Skracanie skal oceny.")
  kryteria = within(kryteria, {
    skrot = mapply(okresl_wzor_skracania, dane, kryteria$schemat_pkt,
                   MoreArgs = list(maxLPozWyk = maxLPozWyk,
                                   minLiczebnPozWyk = minLiczebnPozWyk,
                                   minOdsPozWyk = minOdsPozWyk),
                   SIMPLIFY = FALSE)
    id = as.numeric(sub("^[kp]_", "", kryteria$kryterium))
  })
  rm(dane)

  # ew. wypluwanie na ekran
  if (print) {
    message("  Dokonane skrocenia:")
    for (i in 1:nrow(kryteria)) {
      temp = kryteria$skrot[i][[1]]
      if (all(temp$przedSkroceniem == temp$poSkroceniu)) {
        next
      }
      message("    ", kryteria$kryterium[i])
      temp$rozkladPo =
        c(temp$rozkladPo,
          setNames(rep(NA, length(temp$rozkladPrzed) - length(temp$rozkladPo)),
                   temp$poSkroceniu[duplicated(temp$poSkroceniu)]))
      temp$rozkladPo = temp$rozkladPo[order(as.numeric(names(temp$rozkladPo)))]
      print(data.frame(" " = "  ",
                       "wartość" = temp$przedSkroceniem,
                       "po skróceniu" = temp$poSkroceniu,
                       "przed [n]" =
                         format(unclass(temp$rozkladPrzed), big.mark = "'"),
                       "przed [%]" =
                         paste0(format(100 * temp$rozkladPrzed /
                                         sum(temp$rozkladPrzed),
                                       digits = 1, nsmall = 1), " %"),
                       "po [n]" =
                         sub("NA", "  ",
                             format(unclass(temp$rozkladPo), big.mark = "'")),
                       "po [%]" =
                         sub("NA %", "   ",
                             paste0(format(100 * temp$rozkladPo /
                                             sum(temp$rozkladPo, na.rm = TRUE),
                                           digits = 1, nsmall = 1), " %")),
                       check.names = FALSE),
            row.names = FALSE)
    }
  }

  # kończenie
  kryteria$skrot = lapply(kryteria$skrot, function(x) {
    if (all(x$przedSkroceniem == x$poSkroceniu)) {
      return(NA)
    } else {
      return(paste0(paste0(x$przedSkroceniem, collapse = ";"), "|",
                    paste0(x$poSkroceniu, collapse = ";")))
    }
  })
  kryteria = data.frame(
    id_kryterium = ifelse(grepl("^k_", kryteria$kryterium),
                          kryteria$id, NA),
    id_pseudokryterium = ifelse(grepl("^p_", kryteria$kryterium),
                                kryteria$id, NA),
    opis = NA,
    id_skrotu = unlist(kryteria$skrot),
    stringsAsFactors = FALSE
  )
  return(list(kryteria))
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
okresl_wzor_skracania = function(x, mozliweWartosci, maxLPozWyk = 5,
                                 minLiczebnPozWyk = 100, minOdsPozWyk = 0.05) {
  stopifnot(is.numeric(x)               , length(x) > 0,
            is.numeric(mozliweWartosci) , length(mozliweWartosci) > 1,
            is.numeric(maxLPozWyk)      , length(maxLPozWyk      ) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1,
            is.numeric(minLiczebnPozWyk), length(minLiczebnPozWyk) == 1)
  stopifnot(maxLPozWyk >= 2,
            minLiczebnPozWyk >= 0, minLiczebnPozWyk < Inf,
            minOdsPozWyk >= 0, minOdsPozWyk <= 1)
  stopifnot(all( unique(x) %in% c(mozliweWartosci, NA) ))

  rozklad = table(factor(x, levels = mozliweWartosci))
  rekodowanie = mozliweWartosci
  while (TRUE) {
    koniecSkracania =
      (length(rozklad) <= maxLPozWyk) &
      (min(rozklad) > minLiczebnPozWyk) &
      (min(rozklad / sum(rozklad)) > minOdsPozWyk)
    if (koniecSkracania | (length(rozklad) <= 2)) break

    doPolaczenia1 = which.min(as.numeric(rozklad))
    if (doPolaczenia1 == 1) {
      doPolaczenia2 = 2
    } else if (doPolaczenia1 == length(rozklad)) {
      doPolaczenia2 = length(rozklad) - 1
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
  rekodowanie = as.numeric(factor(rekodowanie, levels = unique(rekodowanie))) - 1
  names(rozklad) = unique(rekodowanie)
  return(list(przedSkroceniem = mozliweWartosci,
              poSkroceniu = rekodowanie,
              rozkladPrzed = table(factor(x, levels = mozliweWartosci)),
              rozkladPo = rozklad))
}
