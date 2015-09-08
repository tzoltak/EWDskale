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
#' @param rzetelnoscEmpiryczna opcjonalnie liczba - rzetelność empiryczna testu
#' @return
#' ramka danych w formacie odpowiadającym strukturze tablicy
#' \code{skalowania_elementy} w bazie
#' @import ZPD
zmien_parametry_na_do_bazy = function(x, idSkali, skalowanie,
                                      rzetelnoscEmpiryczna = NULL) {
  stopifnot(is.data.frame(x),
            is.numeric(idSkali), length(idSkali) == 1,
            is.numeric(skalowanie), length(skalowanie) == 1,
            is.numeric(rzetelnoscEmpiryczna) | is.null(rzetelnoscEmpiryczna))
  if (!is.null(rzetelnoscEmpiryczna)) {
    stopifnot(length(rzetelnoscEmpiryczna) == 1)
  }

  dyskryminacje = subset(x, get("typ") %in% "by")
  trudnosci = subset(x, get("typ") %in% "threshold")
  grupowe = subset(x, grepl("^(mean|variance)", get("typ")))

  # dyskryminacje
  dyskryminacje = with(dyskryminacje, data.frame(
    id_skali = idSkali, skalowanie = skalowanie, kryterium = get("zmienna2"),
    parametr = "dyskryminacja", model = NA, wartosc = get("wartosc"),
    uwagi = NA, bs = get("S.E."), id_elementu = NA, grupowy = FALSE, grupa = NA,
    stringsAsFactors = FALSE
  ))
  # trudności
  trudnosci = suppressMessages(
    with(trudnosci, data.frame(
      id_skali = idSkali, skalowanie = skalowanie, kryterium = get("zmienna1"),
      parametr = paste0("b", get("zmienna2")), model = NA,
      wartosc = get("wartosc"), uwagi = NA, bs = get("S.E."), id_elementu = NA,
      grupowy = FALSE, grupa = NA, stringsAsFactors = FALSE
    )) %>%
      inner_join(setNames(select_(dyskryminacje, ~kryterium, ~wartosc),
                          c("kryterium", "a"))) %>%
      mutate_(.dots = setNames(list(~wartosc / a), "wartosc")) %>%
      select_(~-a) %>%
      group_by_(~kryterium) %>%
      mutate_(.dots = setNames(list(~n()), "lpw")) %>%
      ungroup()
  )
  trudnosciBinarne = filter_(trudnosci, ~lpw == 1) %>%
    select_(~-lpw) %>%
    mutate_(.dots = setNames(list(~"2PL", ~"trudność"), c("model", "parametr")))
  # zmiana parametryzacji trudności poziomów wykonania na względną
  trudnosciGrm = filter_(trudnosci, ~lpw > 1) %>%
    mutate_(.dots = setNames(list(~"GRM"), "model"))
  trudnosciZadanGrm = group_by_(trudnosciGrm, ~kryterium) %>%
    summarise_(.dots = setNames(list(~mean(wartosc)), "b"))
  trudnosciGrm = suppressMessages(
    inner_join(trudnosciGrm, trudnosciZadanGrm) %>%
      mutate_(.dots = setNames(list(~wartosc - b), "wartosc")) %>%
      select_(~-b, ~-lpw)
  )
  if (nrow(trudnosciZadanGrm) > 0) {
    trudnosciZadanGrm = with(trudnosciZadanGrm, data.frame(
      id_skali = idSkali, skalowanie = skalowanie, kryterium = get("kryterium"),
      parametr = "trudność", model = "GRM", wartosc = get("b"), uwagi = NA,
      bs = NA, id_elementu = NA, grupowy = FALSE, grupa = NA,
      stringsAsFactors = FALSE
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
      grupa = sub("^(mean|variance)(|[.]gr)(|.+)$", "\\3", get("typ")),
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
  if (!is.null(rzetelnoscEmpiryczna)) {
    x = bind_rows(x,
                  data.frame(id_skali = idSkali, skalowanie = skalowanie,
                             parametr = "r EAP", model = "n.d.",
                             wartosc = rzetelnoscEmpiryczna, grupowy = TRUE,
                             grupa = "", stringsAsFactors = FALSE))
  }
  x = arrange_(x, ~kryterium, ~parametr)
  maskaSpecjalne = !(grepl("^[kp]_", x$kryterium) | is.na(x$kryterium))
  x$uwagi[maskaSpecjalne] = x$kryterium[maskaSpecjalne]
  x$kryterium[maskaSpecjalne] = NA
  kryteria = suppressMessages(
    pobierz_kryteria_oceny(polacz()) %>%
      filter_(~id_skali %in% c(idSkali, idSkali)) %>%
      select_(~id_skali, ~kryterium, ~kolejnosc_w_skali) %>%
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
