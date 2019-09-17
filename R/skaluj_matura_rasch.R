#' @title Procedury skalowania egzaminow
#' @description
#' Funkcja przeprowadza skalowanie skalowanie matury z matematyki łącznie na
#' poziomach podstawowym i rozszerzonym, z wykorzystaniem wielogrupowego modelu
#' Rascha (na potrzeby maturalnego Kalkulatora EWD). Grupy definiowane są przez
#' wybór poziomu rozszerzonego i typ szkoły (LO/T).
#' @param rok rok przeprowadzenie egzaminu
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @param opis opcjonalnie ciąg znaków - opis skalowania
#' @param katalogSurowe opcjonalnie ścieżka do katalogu, w którym znajdują się
#' pliki z zapisanymi (przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}} z pakietu EWDdane) surowymi
#' wynikami egzaminu
#' @param katalogWyskalowane opcjonalnie ścieżka do katalogu, w którym znajdują
#' się pliki z zapisanymi (przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_wyskalowane}} z pakietu EWDdane)
#' wyskalowanymi wynikami egzaminu
#' @param zapisz wartość logiczna - czy zapisać wyniki do pliku .RData?
#' @param skala id_skali (liczba naturalna) lub ciąg znaków z wyrażeniem
#' regularnym, do którego ma pasować opis skali
#' @param proba opcjonalnie liczba natrualna - wielkość próby, jaka ma być
#' wylosowana z danych przed estymacją modelu; przydatne (tylko) do testów
#' działania funkcji
#' @details
#' Schemat przekodowania sum punktów na oszacowania umiejętności obliczany jest
#' na podstawie danych wzorcowych, przy pomocy funkcji
#' \code{\link{przewidywanie_rasch}}, a następnie na jego podstawie przypisywane
#' są wartości przewidywane wszystkim zdającym.
#'
#' \bold{Uwaga}, oszacowania i normy zwracane przez funkcję \bold{nie są
#' porównywalne pomiędzy LO a T!}
#' @return
#' lista klasy \code{listaWynikowSkalowania}, której elementy są listami
#' klasy \code{wynikiSkalowania} i składają się z elementów:
#' \itemize{
#'   \item{\code{skalowania} data frame o kolumnach:
#'         \itemize{
#'           \item{\code{skalowanie,}}
#'           \item{\code{opis,}}
#'           \item{\code{estymacja,}}
#'           \item{\code{id_skali,}}
#'           \item{\code{do_prezentacji,}}
#'           \item{\code{data;}}
#'         }}
#'   \item{\code{skalowania_grupy} data frame o kolumnach:
#'         \itemize{
#'           \item{\code{id_skali,}}
#'           \item{\code{skalowanie,}}
#'           \item{\code{grupa;}}
#'         }}
#'   \item{\code{skalowania_elementy} data frame o kolumnach:
#'         \itemize{
#'           \item{\code{id_skali,}}
#'           \item{\code{kolejnosc,}}
#'           \item{\code{skalowanie,}}
#'           \item{\code{parametr,}}
#'           \item{\code{model,}}
#'           \item{\code{wartosc,}}
#'           \item{\code{uwagi,}}
#'           \item{\code{bs,}}
#'           \item{\code{grupowy,}}
#'           \item{\code{grupa;}}
#'         }}
#'   \item{\code{skalowania_obserwacje} data frame o kolumnach:
#'         \itemize{
#'           \item{\code{id_skali,}}
#'           \item{\code{skalowanie,}}
#'           \item{\code{id_obserwacji,}}
#'           \item{\code{id_testu,}}
#'           \item{\code{estymacja,}}
#'           \item{\code{nr_pv,}}
#'           \item{\code{wynik,}}
#'           \item{\code{bs,}}
#'           \item{\code{grupa;}}
#'         }}
#'   \item{\code{skalowania} data frame o kolumnach:
#'         \itemize{
#'           \item{\code{skalowanie,}}
#'           \item{\code{opis,}}
#'           \item{\code{estymacja,}}
#'           \item{\code{id_skali,}}
#'           \item{\code{do_prezentacji,}}
#'           \item{\code{data;}}
#'         }}
#'   \item{\code{normy} data frame o kolumnach:
#'         \itemize{
#'           \item{\code{id_skali,}}
#'           \item{\code{skalowanie,}}
#'           \item{\code{grupa,}}
#'           \item{\code{wartosc,}}
#'           \item{\code{wartosc_zr;}}
#'        }}
#'   \item{\code{usunieteKryteria} wektor tekstowy z nazwami (pseudo)kryteriów,
#'         które zostały usunięte podczas skalowania wzorcowego;}
#' }
#' @seealso \code{\link[EWDskalowanie]{skaluj}},
#' \code{\link[EWDskalowanie]{procedura_1k_1w}},
#' \code{\link{sprawdz_wyniki_skalowania}}
#' @importFrom stats setNames
#' @import EWDdane
#' @importFrom EWDskalowanie procedura_1k_1w skaluj
#' @export
skaluj_matura_rasch = function(rok, processors = 2,
                               opis = "skalowanie do Kalkulatora EWD",
                               katalogSurowe = "../../dane surowe",
                               katalogWyskalowane = "../../dane wyskalowane",
                               zapisz = TRUE, skala = NULL, proba = -1) {
  doPrezentacji = TRUE
  stopifnot(is.numeric(rok), length(rok) == 1,
            is.numeric(processors), length(processors) == 1,
            is.character(opis), length(opis) == 1,
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.character(katalogWyskalowane), length(katalogWyskalowane) == 1,
            is.logical(zapisz), length(zapisz) == 1,
            is.null(skala) | is.numeric(skala) | is.character(skala),
            is.numeric(proba), length(proba) == 1)
  stopifnot(as.integer(rok) == rok, rok >= 2010,
            processors %in% (1:32),
            dir.exists(katalogSurowe),
            dir.exists(katalogWyskalowane),
            zapisz %in% c(TRUE, FALSE),
            as.integer(proba) == proba, proba == -1 | proba > 0)
  if (!is.null(skala)) {
    stopifnot(length(skala) == 1)
    doPrezentacji = NA
  }
  if (rok > 2019) {
    stop("Funkcja nie obsługuje skalowania dla egzaminów po 2019 r.")
  }

  # sprawdzanie, czy w bazie są zapisane skala i jakieś skalowanie z parametrami
  if (is.null(skala)) {
    skala = paste0("^ewd;m_mR;", rok)
  } else if (is.character(skala)) {
    if (!grepl("^ewd;m_", skala)) {
      warning("Skale, których opis ma pasować do wyrażenia '", skala,
              "' raczej nie odnoszą się do matury!", immediate. = TRUE)
    }
  }
  parametry = suppressMessages(
    pobierz_parametry_skalowania(skala, doPrezentacji = doPrezentacji,
                                 parametryzacja = "mplus"))
  if (nrow(parametry) == 0) {
    if (is.character(skala)) {
      stop("Nie znaleziono skal o opisie pasującym do wyrażenia '", skala,
           "', która byłaby oznaczona jako 'do prezentacji'.")
    } else {
      stop("Nie znaleziono skali o id_skali = ", skala,
           ", która byłaby oznaczona jako 'do prezentacji'.")
    }
  }

  normy = suppressMessages(
    pobierz_normy(polacz()) %>%
      semi_join(select_(parametry, ~-parametry), copy = TRUE) %>%
      collect()
  )
  if (ncol(normy) == 0) {  # semi_join() brzydko zwraca, jak mu się nic nie łączy
    normy = as.data.frame(
      matrix(nrow = 0, ncol = 5,
             dimnames = list(NULL, c("id_skali", "skalowanie", "grupa",
                                     "wartosc", "wartosc_zr"))))
  }

  rodzajEgzaminu = unique(parametry$rodzaj_egzaminu)
  if (length(rodzajEgzaminu) > 1) {
    stop("Skale są związane z więcej niż jednym egzaminem: '",
         paste0(rodzajEgzaminu, collapse = "', "), "'.")
  }
  skale = group_by_(parametry, ~id_skali) %>%
    summarize_(.dots = setNames(list(~n(), ~opis_skali[1]),
                                c("lSkalowan", "opis"))) %>%
    ungroup()
  if (any(skale$lSkalowan > 1)) {
    stop("Dla skal '", paste0(skale$opis[skale$lSkalowan > 1], collapse = "', '"),
         "' znaleziono wiele skalowań oznaczonych jako 'do prezentacji'.")
  }

  wyniki = vector(mode = "list", length = nrow(skale))
  names(wyniki) = gsub("^.*ewd;([^;]+);.*$", "\\1", parametry$opis_skali)
  for (i in 1:nrow(parametry)) {
    idSkali = parametry$id_skali[i]
    opis = parametry$opis_skali[i]
    skalowanie = parametry$skalowanie[i]
    parametrySkala = parametry$parametry[[i]]
    rzetelnoscEmpiryczna = attributes(parametrySkala)$"r EAP"
    normySkala = filter_(normy, ~id_skali == idSkali)
    odsUtraconejWariancji = NULL

    message(rodzajEgzaminu, " ", rok, " (id_skali: ", idSkali, ", '", opis,
            "'; skalowanie ", skalowanie, ".):")
    # wczytywanie danych z dysku i sprawdzanie, czy jest dla kogo skalować
    dane = wczytaj_wyniki_surowe(katalogSurowe, rodzajEgzaminu, "", rok, idSkali)
    dane = filter_(dane, ~typ_szkoly %in% c("LO", "T"))
    # będziemy wyrzucać wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
    zmienneGrupujace = c("typ_szkoly", "s_mat_r", "czy_sf")
    zmienneKryteria = names(dane)[grep("^[kpst]_[[:digit:]]+$", names(dane))]
    maskaZmienne = c("id_obserwacji", "id_testu", zmienneGrupujace, zmienneKryteria)
    tytulWzorcowe = paste0(names(wyniki)[i], rok, " wzor")
    tytulWszyscy = paste0(names(wyniki)[i], rok, " wszyscy")
    # przyłączanie informacji o tym, kto co zdawał
    src = polacz()
    czesciEgzaminow = suppressMessages(
      pobierz_kryteria_oceny(src, skale = FALSE) %>%
        semi_join(data.frame(kryterium = zmienneKryteria), copy = TRUE) %>%
        select_(~kryterium, ~id_testu) %>%
        left_join(pobierz_testy(src)) %>%
        filter_(~czy_egzamin) %>%
        select_(~kryterium, ~prefiks, ~arkusz) %>%
        distinct() %>%
        collect() %>%
        mutate_(.dots = setNames(list(~!(substr(arkusz, 7, 7) %in%
                                           c("X", "Y", "Z"))),
                                 "czy_sf")) %>%
        select_(~-arkusz) %>%
        distinct()
    )
    # dołączanie zmiennych do grupowania
    czesci = unique(czesciEgzaminow$prefiks)
    wyborCzesci = as.data.frame(matrix(nrow = nrow(dane), ncol = length(czesci),
                                       dimnames = list(NULL, czesci)))
    for (j in czesci) {
      temp = dane[, filter_(czesciEgzaminow, ~prefiks == j)$kryterium]
      wyborCzesci[[j]] = rowSums(!is.na(temp)) > 0
    }
    rm(temp)
    names(wyborCzesci) = sub("m_", "s_", names(wyborCzesci))
    dane = cbind(dane, wyborCzesci)
    rm(wyborCzesci)
    czyStaraFormula = unique(czesciEgzaminow$czy_sf)
    if (length(czyStaraFormula) > 1) {
      maskaZmienneTemp = setdiff(filter_(czesciEgzaminow, ~czy_sf)$kryterium,
                                 filter_(czesciEgzaminow, ~!czy_sf)$kryterium)
      temp = dane[, maskaZmienneTemp]
      dane = cbind(dane, czy_sf = rowSums(!is.na(temp)) > 0)
      rm(temp)
    } else {
      zmienneGrupujace = setdiff(zmienneGrupujace, "czy_sf")
      maskaZmienne = setdiff(maskaZmienne, "czy_sf")
    }
    grupy = distinct(dane[, zmienneGrupujace])
    for (i in ncol(grupy):1) {
      grupy = grupy[order(grupy[[i]]), ]
    }
    # usuwanie grup, których nie jesteśmy w stanie sensownie obsłużyć:
    # uczniów LO piszących starą formułę
    if ("czy_sf" %in% zmienneGrupujace) {
      grupy = filter_(grupy, ~!(typ_szkoly == "LO" & czy_sf))
    }
    grupy = cbind(grupy, gr_tmp1 = 1:nrow(grupy))
    # ładne nazwy grup
    grupy = within(grupy, {
      grupa = paste0(get("typ_szkoly"), " ",
                     ifelse(get("s_mat_r"), "PP i PR", "tylko PP"))
    })
    if ("czy_sf" %in% zmienneGrupujace) {
      grupy = within(grupy, {
        grupa = paste0(grupa, " ", ifelse(get("czy_sf"), "sf", "nf"))
      })
    }
    # usuwanie z danych zdających spoza obsługiwanych grup
    lPrzed = nrow(dane)
    dane = suppressMessages(semi_join(dane, grupy))
    dane = filter_(dane, ~!(s_mat_r & !s_mat_p))
    lPo = nrow(dane)
    if (lPo != lPrzed) {
      message(" Usunięto ", format(lPrzed - lPo, big.mark = "'"), " zdających, ",
              "należących do grup, które nie występują w zbiorowości 'wzorcowej' ",
              "i nie da się im w związku z tym przypisać wyskalowanych wyników ",
              "(np. uczniowie LO, którzy w 2015 r. zdawali maturę w 'starej formule').")
    }
    # i osób, które mają same braki danych w punktacji
    dane = dane[rowSums(!is.na(dane[, zmienneKryteria])) > 0,]
    # jeśli nic w bazie nie znaleźliśmy, to robimy skalowanie wzorcowe
    if (!is.data.frame(parametrySkala) | nrow(normySkala) == 0) {
      if (is.data.frame(parametrySkala)) {
        wartosciZakotwiczone = as.data.frame(parametrySkala)  # pozbywamy się "tbl_df-owatości"
      } else {
        wartosciZakotwiczone = NULL
      }
      daneWzorcowe = filter_(dane, ~populacja_wy & !pomin_szkole)
      # czyszczenie wyników laureatów
      for (p in unique(czesciEgzaminow$prefiks)) {
        maskaObserwacje = daneWzorcowe[[paste0("laur_", p)]] %in% TRUE
        maskaZmienneTemp = filter_(czesciEgzaminow, ~prefiks == p)$kryterium %>%
          unique()
        daneWzorcowe[maskaObserwacje, maskaZmienneTemp] = NA
      }
      daneWzorcowe = daneWzorcowe[, maskaZmienne]
      maskaObserwacje =
        rowSums(is.na(select_(daneWzorcowe, ~-id_obserwacji, ~-id_testu)))
      daneWzorcowe = subset(daneWzorcowe,
                            maskaObserwacje < (ncol(daneWzorcowe) - 2))
      # ew. wybieranie próby
      if (proba > 0) {
        daneWzorcowe = daneWzorcowe[sample(nrow(daneWzorcowe), proba), ]
      }
      # skalowanie wzorcowe
      message("\n### Skalowanie wzorcowe ###\n")
      opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(wyniki)[i],
                                     wartosciZakotwiczone,
                                     wieleGrup = zmienneGrupujace,
                                     rasch = TRUE, processors = processors)
      mWzorcowe = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji",
                         tytul = tytulWzorcowe, zmienneDolaczaneDoOszacowan = "id_testu")
      # kontrola grupowania
      mapowanieGrup =
        mWzorcowe[[1]][[length(mWzorcowe[[1]])]]$parametry$grupyMapowanie
      if (suppressMessages(nrow(semi_join(grupy, mapowanieGrup)) < nrow(grupy))) {
        blad = paste0("Schemat kodowania grup z wyestymowanego modelu nie jest ",
                      "zgodny z założonym na podstawie danych.")
        if (proba > 0) {
          warning(blad, immediate. = TRUE)
        } else {
          stop(blad)
        }
      }
      # zapamiętywanie parametrów modelu
      wartosciZakotwiczone =
        mWzorcowe[[1]][[length(mWzorcowe[[1]])]]$parametry$surowe
      # przygotowywanie danych z oszacowaniami do uśredniania
      daneWzorcowe = suppressMessages(left_join(daneWzorcowe, grupy))
      daneWzorcowe = daneWzorcowe[, !(names(daneWzorcowe) %in%
                                        c(zmienneGrupujace, "gr_tmp1"))]
      temp = as.data.frame(matrix(nrow = nrow(daneWzorcowe),
                                  ncol = length(czesci),
                                  dimnames = list(NULL, czesci)))
      for (j in czesci) {
        maskaZmienneTemp =
          filter_(czesciEgzaminow, ~prefiks == j)$kryterium %>% unique()
        temp[[j]] = rowSums(daneWzorcowe[, maskaZmienneTemp], na.rm = TRUE)
        maskaNA =
          rowSums(!is.na(daneWzorcowe[, filter_(czesciEgzaminow,
                                                ~prefiks == j)$kryterium]))
        temp[[j]][maskaNA == 0] = NA
      }
      names(temp) = sub("^m_", "suma_", names(temp))
      daneWzorcowe =
        cbind(daneWzorcowe[, !(names(daneWzorcowe) %in% zmienneKryteria)], temp)
      oszacowania = suppressMessages(
        mWzorcowe[[1]][[length(mWzorcowe[[1]])]]$zapis %>%
        left_join(grupy)
      )
      names(oszacowania) = sub(tolower( names(wyniki)[i]), names(wyniki)[i],
                               names(oszacowania))
      oszacowania = oszacowania[, c("id_obserwacji", "grupa", names(wyniki)[i],
                                    "id_testu")]
      # obliczanie rzetelności empirycznych
      rzetelnoscEmpiryczna = oszacowania[, c("grupa", names(wyniki)[i])]
      names(rzetelnoscEmpiryczna) = sub(names(wyniki)[i], "oszacowanie",
                                        names(rzetelnoscEmpiryczna))
      warPopGr = suppressMessages(
        filter_(wartosciZakotwiczone, ~grepl("^variance", typ)) %>%
          group_by_(~typ) %>%
          summarise_(.dots = setNames(list(~as.numeric(sub("^variance.gr", "", typ)),
                                           ~wartosc),
                                      c("gr_tmp1", "war_pop"))) %>%
          left_join(select_(grupy, ~gr_tmp1, ~grupa)) %>%
          select_(~-typ, ~-gr_tmp1)
      )
      rzetelnoscEmpiryczna = suppressMessages(
        group_by_(rzetelnoscEmpiryczna, ~grupa) %>%
          summarise_(.dots = setNames(list(~var(oszacowanie, na.rm = TRUE)),
                                      "war")) %>%
          full_join(warPopGr) %>%
          right_join(grupy) %>%
          mutate_(.dots = setNames(list(~war / war_pop), "wartosc")) %>%
          select_(~grupa, ~wartosc)
      )
      # wyprowadzanie oszacowań na 0;1 w ramach LO i w ramach T
      maskaLO = grep("^LO ", oszacowania$grupa)
      maskaT = grep("^T ", oszacowania$grupa)
      oszacowania[[names(wyniki)[i]]][maskaLO] =
        scale(oszacowania[[names(wyniki)[i]]][maskaLO])
      oszacowania[[names(wyniki)[i]]][maskaT] =
        scale(oszacowania[[names(wyniki)[i]]][maskaT])
      # uśrednianie oszacowań, aby były funkcją sum punktów (i przynależności do grup)
      temp = vector(mode = "list", length = nrow(grupy))
      for (j in 1:length(temp)) {
        temp[[j]] = suppressMessages(
          przewidywanie_rasch(semi_join(daneWzorcowe, grupy[j, ]),
                              semi_join(oszacowania, grupy[j, ]),
                              c(suma_mat_p = 50, suma_mat_r = 50))
        )
        temp[[j]]$mapowanie = cbind(temp[[j]]$mapowanie, grupa = grupy$grupa[j],
                                    stringsAsFactors = FALSE)
      }
      normySkala = bind_rows(lapply(temp, function(x) {return(x$mapowanie)}))
      names(normySkala) = sub("^suma$", "wartosc", names(normySkala))
      names(normySkala) = sub(paste0("^", names(wyniki)[i], "$"),
                                  "wartosc_zr", names(normySkala))
      normySkala = data.frame(id_skali = idSkali, skalowanie = skalowanie,
                              normySkala[, c("grupa", "wartosc", "wartosc_zr")],
                              stringsAsFactors = FALSE)
      odsUtraconejWariancji =
        data.frame(grupa = grupy$grupa,
                   odsUtraconejWariancji =
                     unlist(lapply(temp,
                                   function(x) {return(x$odsUtraconejWariancji)})),
                   stringsAsFactors = FALSE)
      rm(mWzorcowe, daneWzorcowe, temp)
      message("\n### Przypisywanie oszacowań wszystkim zdającym ###\n")
    } else {
      # w przeciwnym wypadku podstawiamy zapisane w bazie parametry
      # i sprawdzamy, czy ktoś już ma zapisane oszacowania
      daneWyskalowane = wczytaj_wyniki_wyskalowane(katalogWyskalowane,
                                                   rodzajEgzaminu, rok, idSkali)
      lPrzed = nrow(dane)
      dane = suppressMessages(anti_join(dane, daneWyskalowane))
      rm(daneWyskalowane)
      lPo = nrow(dane)
      if (lPo == 0) {
        message("\n### Brak zdających, dla których trzeba by obliczyć oszacowania. ###\n")
        next
      } else if (lPo < lPrzed) {
        message("\n### Przypisywanie oszacowań ", format(lPo, big.mark = "'"),
                " zdającym, ###\n    którzy ich jeszcze nie mają.")
      } else {
        message("\n### Przypisywanie oszacowań wszystkim zdającym ###\n")
      }
    }
    # zamiast skalowania dla oszacowań
    dane = suppressMessages(left_join(dane, grupy))
    dane = cbind(dane[, c("id_obserwacji", "id_testu", "grupa")],
                 wartosc = rowSums(dane[, zmienneKryteria], na.rm = TRUE))
    dane = suppressMessages(inner_join(dane, normySkala))
    # przypisywanie wyników
    wyniki[[i]] = list(
      skalowania = data.frame(skalowanie = skalowanie, opis = opis,
                              estymacja = "MML (Mplus)", id_skali = idSkali,
                              do_prezentacji = FALSE, data = Sys.Date(),
                              stringsAsFactors = FALSE),
      skalowania_grupy = data.frame(id_skali = idSkali, skalowanie = skalowanie,
                                    grupa = grupy$grupa, stringsAsFactors = FALSE),
      skalowania_elementy = NULL,
      normy = normySkala,
      skalowania_obserwacje =
        data.frame(id_skali = idSkali, skalowanie = skalowanie,
                   dane[, c("id_obserwacji", "id_testu")],
                   estymacja = "EAP", nr_pv = -1,
                   wynik = dane$wartosc_zr,
                   bs = NA,
                   grupa = dane$grupa, stringsAsFactors = FALSE),
      usunieteKryteria = vector(mode = "character", length = 0),
      odsUtraconejWariancji = odsUtraconejWariancji
    )
    if (!is.data.frame(parametrySkala)) {
      wyniki[[i]][["skalowania_elementy"]] =
        zmien_parametry_na_do_bazy(wartosciZakotwiczone, idSkali, skalowanie,
                                   rzetelnoscEmpiryczna,
                                   grupy = select_(grupy, ~grupa, ~gr_tmp1))
    }
    class(wyniki[[i]]) = c(class(wyniki), "wynikiSkalowania")
    attributes(wyniki[[i]])$dataSkalowania = Sys.time()
  }
  # koniec
  class(wyniki) = c(class(wyniki), "listaWynikowSkalowania")
  if (zapisz) {
    nazwaObiektu = paste0("mRasch", rok, "Skalowanie")
    assign(nazwaObiektu, wyniki)
    save(list = nazwaObiektu, file = paste0(nazwaObiektu, ".RData"))
  }
  return(wyniki)
}

