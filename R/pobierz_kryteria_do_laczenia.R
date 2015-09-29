#' @title Laczenie kryteriow
#' @description
#' Na podstawie informacji pozwalających zidentyfikować skale funkcja
#' przygotowuje obiekt z kryteriami oceny, który obrabiają potem funkcje
#' \code{\link{lacz_kryteria_z_nr_zadan}} lub
#' \code{\link{lacz_kryteria_z_korelacji}}.
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @return data table
#' @import ZPD
pobierz_kryteria_do_laczenia = function(skale) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1))
  # pobieranie danych o kryteriach
  src = polacz()
  on.exit(rozlacz(src))
  if (is.character(skale)) {
    skale = pobierz_skale(src, doPrezentacji = NA) %>%
      collect() %>%
      filter_(~grepl(skale, opis_skali)) %>%
      select_(~id_skali) %>%
      distinct() %>%
      as.list() %>% unlist() %>% unname()
  }
  if (length(skale) == 0) {
    stop("Nie znaleziono żadnych skal, których opis pasowałby do podanego wyrażenia regularnego.")
  } else if (length(skale) == 1) {
    skale = rep(skale, 2) # głupie, ale pozwala użyć %in% w filter()
  }
  kryteria = suppressMessages(
    pobierz_skale(src, doPrezentacji = NA) %>%
      filter_(~id_skali %in% skale) %>%
      select_(~id_skali, ~opis_skali, ~id_testu, ~rodzaj_egzaminu,
              ~czesc_egzaminu, ~rok) %>%
      inner_join(pobierz_kryteria_oceny(src)) %>%
      select_(~id_skali, ~opis_skali, ~rodzaj_egzaminu, ~czesc_egzaminu, ~rok,
              ~id_wiazki, ~kryterium, ~numer_pytania, ~typ_pytania,
              ~kolejnosc_w_skali) %>%
      distinct() %>%
      collect()
  )
  if (nrow(kryteria) == 0) {
    stop("Skala o id_skali równym ", skale[1],
         " nie ma przypisanych rzadnych kryteriów.")
  }
  # arrange nie działa dobrze przed collectem
  kryteria = arrange_(kryteria, ~id_skali, ~kolejnosc_w_skali) %>%
    select_(~-kolejnosc_w_skali)
  if (nrow(kryteria) == 0) {
    stop("Nie znaleziono żadnych kryteriów oceny przypisanych do skal ",
         "o podanych identyfikatorach.\n",
         "Upewnij się, że te skale zostały zdefiniowane.")
  }
  # w przypadku skal obejmujących kilka części egzaminu nie da się tak prosto
  # określić części egzaminów, z której pochodzą poszczególne kryteria
  if (any(is.na(kryteria$czesc_egzaminu))) {
    czesciEgzaminu = suppressMessages(
      pobierz_kryteria_oceny(src, testy = TRUE, skale = FALSE) %>%
        inner_join(pobierz_testy(src)) %>%
        filter_(~kryterium %in% kryteria$kryterium, ~czy_egzamin == TRUE) %>%
        select_(~kryterium, ~czesc_egzaminu) %>%
        distinct() %>%
        collect()
    )
    kryteria = suppressMessages(left_join(select_(kryteria, ~-czesc_egzaminu),
                                          czesciEgzaminu)) %>%
      distinct()
  }
  return(kryteria)
}
