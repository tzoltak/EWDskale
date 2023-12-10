#' @title Laczenie kryteriow na podstawie podanych identyfikatorow
#' @description
#' Funkcja łączy, w ramach podanych skal, podane kryteria oceny poszczególnych
#' zadań w pseudokryteria. Jest ona użyteczna w szczególności do skal
#' opisujących wyniki egzaminu na wejściu do skalowania na potrzeby modeli
#' latentnej EWD, gdzie określenie kryteriów do połączenia odbywa się na
#' podstawie innych skal (obejmujących tylko dany egzamin w danym roku), trzeba
#' jednak potem móc te łączenia zastosować do skal łączących wyniki egzaminu
#' z dwóch kolejnych lat (dla danego typu szkoły).
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @param laczenia lista wektorów tekstowych - każdy element list zawiera nazwy
#' kryteriów (w formie 'k_idKryterium'), które powinny zostać połączone w jedno
#' pseudokryterium
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
#' @return data frame
#' Kolumna \code{elementy} zawiera data frame'y które mogą zostać użyte
#' jako argument funkcji \code{\link[ZPDzapis]{edytuj_skale}}.
#' @importFrom dplyr %>% cur_data_all group_by summarise
#' @import ZPD
#' @export
lacz_kryteria_recznie = function(skale, laczenia, src = NULL) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1),
            is.list(laczenia), all(sapply(laczenia, is.character)),
            is.list(laczenia), all(sapply(laczenia, length) > 1),
            !any(duplicated(unlist(laczenia))),
            all(grepl("^[kp]_[[:digit:]]+$", unlist(laczenia))),
            dplyr::is.src(src) | is.null(src))
  if (is.null(src)) {
    src = ZPD::polacz()
    srcPass = NULL
  } else {
    srcPass = src
  }

  suppressWarnings(pobierz_kryteria_do_laczenia(skale, src = src)) %>%
    group_by(.data$id_skali) %>%
    summarise(elementy = list(zdefiniuj_laczenia(cur_data_all(),
                                                 laczenia = laczenia)),
              .groups = "drop") %>%
    filter(!sapply(.data$elementy, is.null)) %>%
    return()
}
#' @title Laczenie kryteriow na podstawie podanych identyfikatorow
#' @description Koń roboczy wywoływany przez
#' \code{\link{lacz_kryteria_recznie}}.
#' @param kryteriaSkali data frame opisujący kryteria oceny w ramach
#' pojedynczej skali
#' @param laczenia lista wektorów tekstowych - każdy element list zawiera nazwy
#' kryteriów (w formie 'k_idKryterium'), które powinny zostać połączone w jedno
#' pseudokryterium
#' @return data frame
#' @importFrom dplyr %>% .data cur_data filter mutate select starts_with
zdefiniuj_laczenia = function(kryteriaSkali, laczenia) {
  stopifnot(is.data.frame(kryteriaSkali),
            "id_skali" %in% names(kryteriaSkali),
            length(unique(kryteriaSkali$id_skali)) == 1,
            is.list(laczenia), all(sapply(laczenia, is.character)),
            is.list(laczenia), all(sapply(laczenia, length) > 1),
            !any(duplicated(unlist(laczenia))),
            all(grepl("^[kp]_[[:digit:]]+$", unlist(laczenia))))
  kryteria = unique(kryteriaSkali$kryterium)
  laczenia = laczenia[sapply(laczenia,
                             function(x, kryteria) {
                               return(any(x %in% kryteria))
                             },
                             kryteria = kryteria)]
  if (length(laczenia) == 0) {
    return(NULL)
  }
  brakujace = setdiff(unlist(laczenia), kryteria)
  laczeniaZBrakujacymi = laczenia[sapply(laczenia,
                                         function(x, brakujace) {
                                           return(any(brakujace %in% x))
                                         },
                                         brakujace = brakujace)]
  if (length(brakujace) > 0) {
    stop("Skala o id ", kryteriaSkali$id_skali[1],
         " ('", kryteriaSkali$opis_skali[1], ") nie zawiera kryteriów:\n- ",
         paste(brakujace, collapse = ",\n- "),
         ",\nchoć pojawiają się one w pasujących definicjach łączenia:\n- ",
         paste(sapply(laczeniaZBrakujacymi, paste, collapse = ", "),
               collapse = ",\n- "), ".")
  }
  bind_rows(kryteriaSkali %>% # kryteria
              filter(!(.data$kryterium %in% unlist(laczenia)),
                     grepl("^k_", .data$kryterium)) %>%
              select(id_kryterium = "kryterium") %>%
              mutate(id_kryterium = as.integer(sub("^k_", "",
                                                   .data$id_kryterium))),
            kryteriaSkali %>% # istniejące pseudokryteria
              filter(!(.data$kryterium %in% unlist(laczenia)),
                     grepl("^p_", .data$kryterium)) %>%
              select(id_pseudokryterium = "kryterium") %>%
              mutate(id_pseudokryterium = as.integer(sub("^p_", "",
                                                         .data$id_pseudokryterium))),
            lapply(laczenia, # pseudokryteria do utworzenia
                   function(x) {
                     x = as.integer(sub("^[kp]_", "", x))
                     names(x) = paste0("id_kryterium_", 1L:length(x))
                     return(x)
                   }) %>%
              bind_rows()) %>%
    mutate(opis = ifelse(is.na(.data$id_kryterium),
                         paste0(kryteriaSkali$rodzaj_egzaminu[1], ";",
                                kryteriaSkali$czesc_egzaminu[1], ";",
                                kryteriaSkali$rok[1], ";",
                                cur_data() %>%
                                  select(starts_with("id_kryterium_")) %>%
                                  apply(1, function(x) {
                                    return(paste(x[!is.na(x)], collapse = ";"))
                                  })),
                         NA_character_),
           id_skrotu = NA_character_) %>%
    return()
}
