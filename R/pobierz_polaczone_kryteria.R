#' @title Skracanie skal oceny
#' @description
#' Funkcja pobiera definicje pseudokryteriów przypisanych do podanych skal
#' i zwraca je w formie, w jakiej mogą zostać przekazane do
#' \code{\link{lacz_kryteria_recznie}}.
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @param echo wartość logiczna - czy wypisać na konsoli kod pozwalający
#' zdefiniować zwracaną listę?
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
#' @return Lista wektorów tekstowych - każdy element zawiera nazwy kryteriów
#' oceny tworzących jedno pseudokryterium - które mogą zostać użyte
#' jako argument \code{laczenia} funkcji \code{\link{lacz_kryteria_recznie}}.
#' @export
pobierz_polaczone_kryteria = function(skale, echo = FALSE, src = NULL) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1),
            is.logical(echo), length(echo) == 1, !anyNA(echo),
            dplyr::is.src(src) | is.null(src))
  if (is.null(src)) {
    src = ZPD::polacz()
    srcPass = NULL
  } else {
    srcPass = src
  }

  # pobieranie danych o pseudokryteriach
  if (is.character(skale)) {
    skale = pobierz_skale(src, doPrezentacji = NA, skalowania = FALSE) %>%
      collect() %>%
      filter(grepl(skale, .data$opis_skali)) %>%
      select("id_skali", "opis_skali", "rodzaj_egzaminu", "rok") %>%
      distinct()
  } else {
    skale = pobierz_skale(src, doPrezentacji = NA, skalowania = FALSE) %>%
      filter(.data$id_skali %in% skale) %>%
      select("id_skali", "opis_skali", "rodzaj_egzaminu", "rok") %>%
      distinct() %>%
      collect()
  }
  if (nrow(skale) == 0) {
    stop("Nie znaleziono żadnych skal, których opis pasowałby do podanego wyrażenia regularnego.")
  }
  pseudokryt = suppressMessages(
    ZPD::pobierz_kryteria_oceny(src, testy = FALSE, skale = TRUE,
                                krytSkladowe = TRUE)) %>%
    filter(.data$id_skali %in% skale$id_skali,
           !is.na(.data$kryt_skladowe)) %>%
    arrange(.data$id_skali, .data$kryterium, .data$kryt_skladowe) %>%
    collect()
  przypisanieDoSkal = pseudokryt %>%
    select("kryterium", "id_skali") %>%
    distinct() %>%
    left_join(skale,
              by = "id_skali")
  pseudokryt = split(pseudokryt$kryt_skladowe, pseudokryt$kryterium)
  if (echo) {
    cat(paste0("\nlaczenia = list(",
               paste0("c(",
                      sapply(pseudokryt,
                             function(x) {
                               paste0('"', x, '"', collapse = ", ")
                             }), ")",
                      collapse = ",\n                "), ")\n"))
  }
  return(structure(pseudokryt,
                   przypisanieDoSkal = przypisanieDoSkal))
}
