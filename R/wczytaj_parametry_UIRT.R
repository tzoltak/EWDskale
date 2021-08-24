#' @title Wczytuje parametry skalowania przeprowadzonego UIRT-em
#' @description Wczytuje parametry z pliku CSV i konwertuje je na strukturę
#'   danych zgodną z tą, jakiej używa funkcja
#'   \code{\link[ZPDzapis]{zapisz_skalowanie}}.
#'
#'   Opcjonalnie zapisuje wynik konwersji do pliku .RData.
#' @param sciezkaWe ścieżka do pliku CSV z parametrami skalowania
#'   przeprowadzonego UIRT-em
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#'   \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#'   automatycznego nawiązania połączenia.
#' @param sciezkaWy NULL ścieżka do pliku .RData, w którym zapisane zostaną
#'   wyniki. Jeśli NULL, wynik nie zostanie zapisany do pliku.
#' @param skalowanie NULL jeśli podane, nadpisuje id skalowania wczytane z pliku
#'   wejściowego (uwaga, ma sens raczej tylko gdy plik wejściowy opisuje tylko
#'   jedno skalowanie)
#' @param opis NULL jeśli podany, nadpisuje opis skalowania wczytany z pliku
#'   wejściowego (uwaga, ma sens raczej tylko gdy plik wejściowy opisuje tylko
#'   jedno skalowanie)
#' @return lista opisująca wyniki skalowania w formacie zgodnym z tym, jakiego
#'   używa unkcja \code{\link[ZPDzapis]{zapisz_skalowanie}}.
#' @import dplyr
#' @export
wczytaj_parametry_UIRT = function(sciezkaWe, src = NULL, sciezkaWy = NULL, skalowanie = NULL, opis = NULL) {
  dane = suppressMessages(readr::read_csv(sciezkaWe))
  if (is.null(src)) {
    src = ZPD::polacz()
  }
  wyjscie = list()
  for (skalowanieId in unique(dane$source)) {
    sk = list()
    idSkali = as.integer(sub('.*_s([0-9]+).*', '\\1', skalowanieId))
    idSkalowania = ifelse(is.null(skalowanie), as.integer(sub('.*_sk([0-9]+).*', '\\1', skalowanieId)), skalowanie)
    opisSkalowania = ifelse(is.null(opis), sub('^(.*)_s[0-9]+_sk[0-9]+.*', '\\1', skalowanieId), opis)
    daneSk = dane %>%
      dplyr::filter(.data$source == skalowanieId) %>%
      dplyr::rename(
        kryterium = .data$item,
        parametr = .data$parameter,
        wartosc = .data$value
      ) %>%
      dplyr::mutate(
        model = sub('^([123])PLM$', '\\1PL', toupper(.data$model)),
        parametr = sub('^b$', 'trudność', .data$parametr),
      ) %>%
      dplyr::select(-.data$source)
    skaleElementy = ZPD::pobierz_kryteria_oceny(src, testy = FALSE, skale = TRUE, krytSkladowe = FALSE) %>%
      dplyr::filter(.data$id_skali == idSkali) %>%
      dplyr::select(.data$kryterium, .data$kolejnosc_w_skali) %>%
      dplyr::rename(kolejnosc = .data$kolejnosc_w_skali) %>%
      dplyr::collect()

    sk$skalowania = data.frame(
      skalowanie = idSkalowania,
      opis = opisSkalowania,
      estymacja = 'MML (UIRT)',
      id_skali = idSkali,
      do_prezentacji = FALSE,
      data = Sys.Date()
    )
    sk$skalowania_grupy = data.frame(
      id_skali = idSkali,
      skalowanie = idSkalowania,
      grupa = unique(daneSk$item_bk[daneSk$model == 'GROUP'])
    )

    paramGrupy = daneSk %>%
      dplyr::filter(.data$model == 'GROUP') %>%
      dplyr::select(-.data$kryterium, -.data$model) %>%
      dplyr::mutate(
        kolumna  = dplyr::if_else(grepl('se_', .data$parametr), 'bs', 'wartosc'),
        parametr = paste0('group_', sub('se_', '', .data$parametr))
      ) %>%
      dplyr::rename(grupa = .data$item_bk) %>%
      tidyr::pivot_wider(c('grupa', 'parametr'), names_from = 'kolumna', values_from = 'wartosc') %>%
      dplyr::mutate(
        grupowy     = TRUE,
        kolejnosc   = NA_integer_,
        model       = 'n.d.',
        uwagi       = NA_character_
      )
    paramTematy = daneSk %>%
      dplyr::filter(grepl('^s_|^t[0-9]+_', .data$kryterium)) %>%
      dplyr::select(-.data$item_bk) %>%
      dplyr::rename(uwagi = .data$kryterium) %>%
      dplyr::mutate(
        kolejnosc    = NA_integer_,
        bs           = NA_real_,
        grupowy      = FALSE,
        grupa        = NA_character_
      )
    paramKryteria = daneSk %>%
      dplyr::filter(.data$model != 'GROUP' & !grepl('^s_|^t[0-9]+_', .data$kryterium)) %>%
      dplyr::mutate(
        uwagi   = NA_character_,
        bs      = NA_real_,
        grupowy = FALSE,
        grupa   = NA_character_
      ) %>%
      dplyr::left_join(skaleElementy) %>%
      dplyr::select(-.data$item_bk, -.data$kryterium)
    stopifnot(all(!is.na(paramKryteria$kolejnosc)))
    sk$skalowania_elementy = dplyr::bind_rows(paramGrupy, paramTematy, paramKryteria) %>%
      dplyr::mutate(
        id_skali = idSkali,
        skalowanie = idSkalowania,
        id_elementu = NA_integer_
      ) %>%
      dplyr::select(
        .data$id_skali, .data$kolejnosc, .data$skalowanie, .data$parametr, .data$model,
        .data$wartosc, .data$uwagi, .data$bs, .data$id_elementu, .data$grupowy, .data$grupa
      )

    class(sk) = c(class(sk), 'wynikiSkalowania')
    klucz = (strsplit(skalowanieId, ';')[[1]])[2]
    wyjscie[[klucz]] = sk
  }
  class(wyjscie) = c(class(wyjscie), 'listaWynikowSkalowania')
  if (!is.null(sciezkaWy)) {
    save(wyjscie, file = sciezkaWy)
  }
  return(invisible(wyjscie))
}
