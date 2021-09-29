#' @title Konwersja tabeli parametrow na format bazy
#' @description
#' Funkcja przekształca tabelę wartości parametrów modelu skalowania z formatu
#' zwracanego przez funkcję \code{\link[EWDskalowanie]{skaluj}} na format,
#' w jakim wartości parametrów zapisywane są w bazie i na klasyczną,
#' jednowymiarowąz parametryzację IRT.
#' @param x data frame z wartościami parametrów w formacie zwracanym przez
#' funkcję \code{\link[EWDskalowanie]{skaluj}}
#' @param idSkali liczba naturalna - id_skali w bazie
#' @param skalowanie liczba naturalna - nr skalowania w bazie
#' @param rzetelnoscEmpiryczna opcjonalnie liczba lub data frame - rzetelność
#' empiryczna testu (w przypadku data frame'a - w poszczególnych grupach)
#' @param standaryzacja opcjonalnie data frame o kolumnach \code{grupa},
#' \code{sr}, \code{os}, zawierający parametry standaryzacji oszacowań
#' w poszczególnych grupach
#' @param grupy data frame zawierający mapowanie numerów grup na ich nazwy);
#' musi składać się z dwóch zmiennych: 'grupa', zawierającej nazwy grup
#' i drugiej, o dowolnej nazwie, zawierającej numery grup
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
#' @return
#' ramka danych w formacie odpowiadającym strukturze tablicy
#' \code{skalowania_elementy} w bazie
#' @importFrom stats setNames
#' @import ZPD
zmien_parametry_na_do_bazy = function(x, idSkali, skalowanie,
                                      rzetelnoscEmpiryczna = NULL,
                                      standaryzacja = NULL, grupy = NULL,
                                      src = NULL) {
  stopifnot(is.data.frame(x),
            is.numeric(idSkali), length(idSkali) == 1,
            is.numeric(skalowanie), length(skalowanie) == 1,
            is.numeric(rzetelnoscEmpiryczna) |
              is.data.frame(rzetelnoscEmpiryczna) |
              is.null(rzetelnoscEmpiryczna),
            is.data.frame(standaryzacja) |
              is.null(standaryzacja),
            is.data.frame(grupy) | is.null(grupy),
            dplyr::is.src(src) | is.null(src))
  if (is.null(src)) {
    src = ZPD::polacz()
  }
  if (is.numeric(rzetelnoscEmpiryczna)) {
    stopifnot(length(rzetelnoscEmpiryczna) == 1)
    rzetelnoscEmpiryczna =
      data.frame(grupa = "", wartosc = rzetelnoscEmpiryczna,
                 stringsAsFactors = FALSE)
  } else if (is.data.frame(rzetelnoscEmpiryczna)) {
    stopifnot(all(c("grupa", "wartosc") %in% names(rzetelnoscEmpiryczna)))
  }
  if (is.data.frame(standaryzacja)) {
    stopifnot(all(c("grupa", "sr", "os") %in% names(standaryzacja)))
  }
  if (is.data.frame(grupy)) {
    stopifnot("grupa" %in% names(grupy), ncol(grupy) == 2)
    names(grupy)[names(grupy) != "grupa"] = "nrGrupy"
  } else {
    grupy = data.frame(id_skali = idSkali, nrGrupy = NA, grupa = "",
                       stringsAsFactors = FALSE)
  }

  dyskryminacje = subset(x, get("typ") %in% "by")
  trudnosci = subset(x, get("typ") %in% "threshold")
  grupowe = subset(x, grepl("^(mean|variance)", get("typ")))

  # dyskryminacje
  dyskryminacje = with(dyskryminacje, data.frame(
    id_skali = idSkali, skalowanie = skalowanie, kryterium = get("zmienna2"),
    parametr = "a", model = NA_character_, wartosc = get("wartosc"),
    uwagi = NA_character_, bs = get("S.E."), id_elementu = NA_real_,
    grupowy = FALSE, nrGrupy = NA_character_, stringsAsFactors = FALSE
  ))
  # trudności
  trudnosci = suppressMessages(
    with(trudnosci, data.frame(
      id_skali = idSkali, skalowanie = skalowanie, kryterium = get("zmienna1"),
      parametr = paste0("b", get("zmienna2")), model = NA_character_,
      wartosc = get("wartosc"), uwagi = NA_character_, bs = get("S.E."),
      id_elementu = NA_real_, grupowy = FALSE, nrGrupy = NA_character_,
      stringsAsFactors = FALSE
    )) %>%
      inner_join(setNames(select(dyskryminacje, "kryterium", "wartosc"),
                          c("kryterium", "a"))) %>%
      mutate(wartosc = .data$wartosc / .data$a) %>%
      select(-"a") %>%
      group_by(.data$kryterium) %>%
      mutate(lpw = n()) %>%
      ungroup()
  )
  trudnosciBinarne = filter(trudnosci, .data$lpw == 1) %>%
    select(-"lpw") %>%
    mutate(model = "2PL",
           parametr = "trudność")
  # zmiana parametryzacji trudności poziomów wykonania na względną
  trudnosciGrm = filter(trudnosci, .data$lpw > 1) %>%
    mutate(model = "GRM")
  trudnosciZadanGrm = group_by(trudnosciGrm, .data$kryterium) %>%
    summarise(b = mean(.data$wartosc))
  trudnosciGrm = suppressMessages(
    inner_join(trudnosciGrm, trudnosciZadanGrm) %>%
      mutate(wartosc = .data$wartosc - .data$b) %>%
      select(-"b", -"lpw")
  )
  if (nrow(trudnosciZadanGrm) > 0) {
    trudnosciZadanGrm = with(trudnosciZadanGrm, data.frame(
      id_skali = idSkali, skalowanie = skalowanie, kryterium = get("kryterium"),
      parametr = "trudność", model = "GRM", wartosc = get("b"),
      uwagi = NA_character_, bs = NA_real_, id_elementu = NA_real_,
      grupowy = FALSE, nrGrupy = NA_character_, stringsAsFactors = FALSE
    ))
  }
  # przypisywanie modelu dyskryminacjom
  dyskryminacje$model[dyskryminacje$kryterium %in% trudnosciBinarne$kryterium] = "2PL"
  dyskryminacje$model[dyskryminacje$kryterium %in% trudnosciZadanGrm$kryterium] = "GRM"
  # wartości oczekiwane i wariancje konstruktu
  if (nrow(grupowe) > 0) {
    grupowe = with(grupowe, data.frame(
      id_skali = idSkali, skalowanie = skalowanie, kryterium = NA,
      parametr = get("typ"), model = "n.d.", wartosc = get("wartosc"), uwagi = NA,
      bs = get("S.E."), id_elementu = NA, grupowy = TRUE,
      nrGrupy = sub("^(mean|variance)(|[.]gr)(|.+)$", "\\3", get("typ")),
      stringsAsFactors = FALSE
    ))
    grupowe$bs = ifelse(grupowe$bs == 0, NA, grupowe$bs)
    grupowe$parametr = sub("^(mean|variance)(|[.]gr)(|.+)$",
                           paste0("group_\\1"), grupowe$parametr)
    grupowe$parametr = sub("variance", paste0("sd"), grupowe$parametr)
    maskaWar = grepl("group_sd", grupowe$parametr)
    grupowe$wartosc[maskaWar] = sqrt(grupowe$wartosc[maskaWar])
  } else {
    grupowe = NULL
  }
  # łączenie, obsługa parametrów specjalnych i przypisywanie kolejnosci
  x = bind_rows(dyskryminacje, trudnosciBinarne, trudnosciZadanGrm,
                trudnosciGrm, grupowe)
  x$nrGrupy = as.numeric(x$nrGrupy)
  x = suppressMessages(
    left_join(x, grupy) %>% select(-"nrGrupy")
  )
  if (!is.null(rzetelnoscEmpiryczna)) {
    x = bind_rows(x,
                  data.frame(id_skali = idSkali, skalowanie = skalowanie,
                             parametr = "r EAP", model = "n.d.",
                             wartosc = rzetelnoscEmpiryczna$wartosc,
                             grupowy = TRUE, grupa = rzetelnoscEmpiryczna$grupa,
                             stringsAsFactors = FALSE))
  }
  if (!is.null(standaryzacja)) {
    x = bind_rows(x,
                  data.frame(id_skali = idSkali, skalowanie = skalowanie,
                             parametr = "std_mean", model = "n.d.",
                             wartosc = standaryzacja$sr,
                             grupowy = TRUE, grupa = standaryzacja$grupa,
                             stringsAsFactors = FALSE),
                  data.frame(id_skali = idSkali, skalowanie = skalowanie,
                             parametr = "std_sd", model = "n.d.",
                             wartosc = standaryzacja$os,
                             grupowy = TRUE, grupa = standaryzacja$grupa,
                             stringsAsFactors = FALSE))
  }
  x = arrange(x, .data$kryterium, .data$parametr)
  maskaSpecjalne = !(grepl("^[kp]_", x$kryterium) | is.na(x$kryterium))
  x$uwagi[maskaSpecjalne] = x$kryterium[maskaSpecjalne]
  x$kryterium[maskaSpecjalne] = NA
  kryteria = suppressMessages(
    pobierz_kryteria_oceny(src) %>%
      filter(.data$id_skali %in% c(idSkali, idSkali)) %>%
      select("id_skali", "kryterium", "kolejnosc_w_skali") %>%
      collect() %>%
      unique()
  )
  names(kryteria) = sub("kolejnosc_w_skali", "kolejnosc", names(kryteria))
  x = suppressMessages(left_join(x, kryteria))
  # koniec
  x = x[, c("id_skali", "kolejnosc", "skalowanie", "parametr", "model",
            "wartosc", "uwagi", "bs", "id_elementu", "grupowy", "grupa")]
  return(x)
}
