#' @title Laczenie kryteriow na podstawie numerow zadan
#' @description
#' Funkcja łączy, w ramach podanych skal, kryteria oceny poszczególnych zadań
#' (wyróżnionych na podstawie numeracji w arkuszu egzaminu) w pseudokryteria.
#'
#' Jeśli w ramach skali są już zdefiniowane jakieś pseudokryteria, to nie
#' będą one w ogóle uwzględniane w procesie łączenia.
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @return lista, której każdy element jest dwuelemntową listą, zawierającą id
#' skali oraz data frame, której można użyć jako argument \code{elementy}
#' funkcji \code{\link[ZPDzapis]{edytuj_skale}}
#' @import ZPD
#' @export
lacz_kryteria_z_nr_zadan = function(skale) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1))

  kryteria = pobierz_kryteria_do_laczenia(skale)
  temp = group_by_(kryteria, ~id_skali) %>%
    do_(.dots = setNames(list(~lacz_kryteria_z_nr_zadan_w_ramach_skali(.)),
                         "elementy"))

  temp = mapply(function(x, y) {return(list(id_skali = unname(x),
                                            elementy = y))},
                select_(temp, ~id_skali) %>% as.list %>% unlist %>% as.list,
                (select_(temp, ~elementy) %>% as.list)[[1]],
                SIMPLIFY = FALSE)
  class(temp) = append(class(temp), "listaArgumentowEdytujSkale")
  if (is.character(skale)) {
    names(temp) = paste0(skale, ": skala", 1:length(temp))
  } else {
    names(temp) = skale
  }
  return(temp)
}
#' @title Laczenie kryteriow na podstawie numerow zadan
#' @description Koń roboczy wywoływany przez \code{\link{lacz_kryteria_z_nr_zadan}}.
#' @param x data frame opisujący kryteria oceny w ramach skali
#' @return data frame, której można uzyć jako argument \code{elementy}
#' funkcji \code{\link[ZPDzapis]{edytuj_skale}}
#' @import dplyr
#' @import reshape2
lacz_kryteria_z_nr_zadan_w_ramach_skali = function(x) {
  message("id_skali: ", x$id_skali[1])
  x = mutate_(x,
              .dots = setNames(list(~sub("^(.*,.*)$", "pseudo", numer_pytania)),
                               "numer_pytania")) %>%  # zaczynamy od wyłączenia pseudokryteriów
    mutate_(.dots = setNames(list(~sub("^([[:digit:]]+).*$", "\\1", numer_pytania)),
                             "numer_pytania")) %>%
    mutate_(.dots = setNames(list(~suppressWarnings(as.numeric(numer_pytania))),
                             "numer_pytania")) %>%
    group_by_(~rodzaj_egzaminu, ~czesc_egzaminu, ~numer_pytania) %>%
    mutate(n = n()) %>%
    mutate_(.dots = setNames(list(~n > 1 &
                                    (numer_pytania > 0 & !is.na(numer_pytania)) &
                                    typ_pytania != "rozprawka",
                                  ~sub("^[kp]_", "", kryterium),
                                  ~grepl("^[p]_", kryterium)),
                             c("maska", "id_kryterium", "czy_pseudo")))
  doPolaczenia = filter_(x, ~maska) %>%
    mutate_(.dots = setNames(list(~paste("ewd", rodzaj_egzaminu, czesc_egzaminu,
                                         rok, numer_pytania, sep = ";")),
                             "opis")) %>%
    group_by_(~opis) %>%
    select_(~opis, ~id_kryterium) %>%
    mutate_(.dots = setNames(list(~paste0("id_kryterium_",
                                          dense_rank(id_kryterium))),
                             "kolejnosc")) %>%
    dcast(... ~ kolejnosc, value.var = "id_kryterium") %>%
    mutate_(.dots = setNames(list(~NA, ~NA, ~NA,
                                  ~id_kryterium_1),
                             c("id_skrotu", "id_kryterium", "id_pseudokryterium",
                               "kolejnosc")))
  kryteriaNieLacz = filter_(x, ~!czy_pseudo, ~!maska) %>%
    ungroup() %>%
    select_(~id_kryterium) %>%
    mutate_(.dots = setNames(list(~NA, ~id_kryterium),
                             c("id_pseudokryterium", "kolejnosc")))
  pseudokryteriaNieLacz = filter_(x, ~czy_pseudo, ~!maska) %>%
    ungroup() %>%
    select_(~id_kryterium) %>%
    mutate_(.dots = setNames(list(~id_kryterium, ~id_kryterium),
                             c("id_pseudokryterium", "kolejnosc"))) %>%
    mutate_(.dots = setNames(list(~NA), "id_kryterium"))
  elementy = bind_rows(kryteriaNieLacz, pseudokryteriaNieLacz, doPolaczenia) %>%
    arrange_(~kolejnosc) %>%
    select_(~-kolejnosc)
  return(elementy)
}
