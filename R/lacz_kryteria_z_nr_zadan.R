#' @title Laczenie kryteriow na podstawie numerow zadan
#' @description
#' Funkcja łączy, w ramach podanych skal, kryteria oceny poszczególnych zadań
#' (wyróżnionych na podstawie numeracji w arkuszu egzaminu) w pseudokryteria.
#'
#' Jeśli w ramach skali są już zdefiniowane jakieś pseudokryteria, to nie
#' będą one w ogóle uwzględniane w procesie łączenia.
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
#' @return lista, której każdy element jest dwuelemntową listą, zawierającą id
#' skali oraz data frame, której można użyć jako argument \code{elementy}
#' funkcji \code{\link[ZPDzapis]{edytuj_skale}}
#' @importFrom stats setNames
#' @import dplyr
#' @import ZPD
#' @export
lacz_kryteria_z_nr_zadan = function(skale, src = NULL) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1),
            dplyr::is.src(src) | is.null(src))

  kryteria = pobierz_kryteria_do_laczenia(skale, nf = TRUE, src = src)
  temp = group_by(kryteria, .data$id_skali) %>%
    summarise(elementy = lacz_kryteria_z_nr_zadan_w_ramach_skali(cur_data_all()))

  temp = mapply(function(x, y) {return(list(id_skali = unname(x),
                                            elementy = y))},
                select(temp, "id_skali") %>% as.list %>% unlist %>% as.list,
                (select(temp, "elementy") %>% as.list)[[1]],
                SIMPLIFY = FALSE)
  class(temp) = c("listaArgumentowEdytujSkale", class(temp))
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
#' @importFrom stats setNames
#' @import dplyr
#' @import tidyr
lacz_kryteria_z_nr_zadan_w_ramach_skali = function(x) {
  message("id_skali: ", x$id_skali[1])
  x = mutate(x,
             numer_pytania = sub("^(.*,.*)$", "pseudo",
                                 .data$numer_pytania)) %>%  # zaczynamy od wyłączenia pseudokryteriów
    mutate(numer_pytania = sub("^([[:digit:]]+)(|_.*)$", "\\1",
                               .data$numer_pytania)) %>%
    mutate(numer_pytania = suppressWarnings(as.numeric(.data$numer_pytania))) %>%
    group_by(.data$rodzaj_egzaminu, .data$czesc_egzaminu, .data$numer_pytania) %>%
    mutate(n = n()) %>%
    mutate(maska = .data$n > 1 &
             (.data$numer_pytania > 0 & !is.na(.data$numer_pytania)) &
             .data$typ_pytania != "rozprawka",
           id_kryterium = sub("^[kp]_", "", .data$kryterium),
           czy_pseudo = grepl("^[p]_", .data$kryterium))
  if (any(x$maska)) {
    doPolaczenia = filter(x, .data$maska) %>%
      mutate(opis = paste("ewd", .data$rodzaj_egzaminu, .data$czesc_egzaminu,
                          .data$rok, .data$numer_pytania, sep = ";")) %>%
      group_by(.data$opis) %>%
      select("opis", "id_kryterium") %>%
      mutate(kolejnosc = paste0("id_kryterium_",
                                dense_rank(.data$id_kryterium))) %>%
      pivot_wider(names_from = "kolejnosc", values_from = "id_kryterium") %>%
      mutate(id_skrotu = NA_character_,
             id_kryterium = NA_character_,
             id_pseudokryterium = NA_character_,
             kolejnosc = .data$id_kryterium_1)
  } else {
    doPolaczenia = NULL
  }
  kryteriaNieLacz = filter(x, !.data$czy_pseudo, !.data$maska) %>%
    ungroup() %>%
    select("id_kryterium") %>%
    mutate(id_pseudokryterium = NA_character_,
           kolejnosc = .data$id_kryterium)
  pseudokryteriaNieLacz = filter(x, .data$czy_pseudo, !.data$maska) %>%
    ungroup() %>%
    select("id_kryterium") %>%
    mutate(id_pseudokryterium = .data$id_kryterium,
           kolejnosc = .data$id_kryterium) %>%
    mutate(id_kryterium = NA_character_)
  elementy = bind_rows(kryteriaNieLacz, pseudokryteriaNieLacz, doPolaczenia) %>%
    arrange(.data$kolejnosc) %>%
    select(-"kolejnosc") %>%
    distinct()  # to ostatnie na okoliczność wspólnych zadań starej i nowej formuły matury
  return(list(elementy))
}
