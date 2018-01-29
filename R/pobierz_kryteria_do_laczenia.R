#' @title Laczenie kryteriow
#' @description
#' Na podstawie informacji pozwalających zidentyfikować skale funkcja
#' przygotowuje obiekt z kryteriami oceny, który obrabiają potem funkcje
#' \code{\link{lacz_kryteria_z_nr_zadan}} lub
#' \code{\link{lacz_kryteria_z_korelacji}}.
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @param nf opcjonalnie wartość logiczna (domyślnie FALSE) - czy w przypadku
#' matury (od 2015 r.) zaznaczać sufiksem dopisywanym do \code{czesc_egzaminu},
#' czy kryterium pochodzi z arkusza w "starej", czy w "nowej" formule egzaminu?
#' @return data table
#' @importFrom stats setNames
#' @import ZPD
pobierz_kryteria_do_laczenia = function(skale, nf = FALSE) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1),
            is.logical(nf), length(nf) == 1)
  # pobieranie danych o kryteriach
  src = polacz()
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
      # jeśli ze skalą jest powiązany test "nieegzaminacyjny" - bierzemy tylko go
      left_join(pobierz_testy(src)) %>%
      group_by_(~id_skali) %>%
      mutate_(.dots = list(tylko_testy_egazminu = ~all(czy_egzamin))) %>%
      filter_(~czy_egzamin == tylko_testy_egazminu) %>%
      # filter_(~czy_egzamin) %>%
      # koniec j.w.
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
        select_(~kryterium, ~rodzaj_egzaminu, ~czesc_egzaminu, ~arkusz) %>%
        distinct() %>%
        collect()
    )
    if (any(czesciEgzaminu$rodzaj_egzaminu == "matura" & nf)) {
      czesciEgzaminu =
        mutate_(czesciEgzaminu,
                .dots = setNames(list(~substr(arkusz, 7, 7) %in% c("X", "Y", "Z")),
                                 "czy_nf")) %>%
        mutate_(.dots = setNames(list(~paste0(czesc_egzaminu,
                                              ifelse(czy_nf, " nf", ""))),
                                 "czesc_egzaminu")) %>%
        select_(~-czy_nf)
    }
    czesciEgzaminu = select_(czesciEgzaminu, ~-rodzaj_egzaminu, ~-arkusz) %>%
      distinct()
    kryteria = suppressMessages(
      select_(kryteria, ~-czesc_egzaminu) %>%
        distinct() %>%
      left_join(czesciEgzaminu)
    )
  }
  return(kryteria)
}
