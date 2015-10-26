#' @title Procedury skalowania egzaminow
#' @description
#' Funkcja przeprowadza skalowanie czterech wskaźników osiągnięć maturalnych:
#' 1) humanistycznego (j. polski, historia, WOS), 2) matematyczno-przyrodniczego
#' (matematyka, biologia, chemia, fizyka, geografia, informatyka),
#' 3) polonistycznego (j. polski), 4) matematycznego (matematyka), na potrzeby
#' wyliczania trzyletnich wskaźników dla LO i techników. Wykorzystywane są
#' wielogrupowe modele 2PL/SGRM. Grupy definiowane są albo przez wybór poziomu
#' rozszerzonego i typ szkoły (LO/T) - dla wskaźników 1), i 4), albo przez
#' typ szkoły (LO/T) - dla wskaźników 2), i 3).
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
#' \bold{Uwaga}, oszacowania zwracane przez funkcję \bold{nie są porównywalne
#' pomiędzy LO a T!}
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
#'   \item{\code{usunieteKryteria} wektor tekstowy z nazwami (pseudo)kryteriów,
#'         które zostały usunięte podczas skalowania wzorcowego;}
#' }
#' @seealso \code{\link[EWDskalowanie]{skaluj}},
#' \code{\link[EWDskalowanie]{procedura_1k_1w}},
#' \code{\link{sprawdz_wyniki_skalowania}}
#' @import EWDdane
#' @importFrom EWDskalowanie procedura_1k_1w skaluj
#' @export
skaluj_matura = function(rok, processors = 2, opis = "skalowanie do EWD",
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
  if (rok > 2015) {
    stop("Funkcja nie obsługuje skalowania dla egzaminów po 2015 r.")
  }

  # sprawdzanie, czy w bazie są zapisane skala i jakieś skalowanie z parametrami
  if (is.null(skala)) {
    skala = paste0("^ewd;m_(h|jp|m|mp);", rok)
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
    standaryzacja = NULL

    message(rodzajEgzaminu, " ", rok, " (id_skali: ", idSkali, ", '", opis,
            "'; skalowanie ", skalowanie, ".):")
    # wczytywanie danych z dysku i sprawdzanie, czy jest dla kogo skalować
    dane = wczytaj_wyniki_surowe(katalogSurowe, rodzajEgzaminu, "", rok, idSkali)
    dane = filter_(dane, ~typ_szkoly %in% c("LO", "T"))

    zmienneKryteria = names(dane)[grep("^[kp]_[[:digit:]]+$", names(dane))]
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
    rozlacz(src)
    # ew. tworzenie zmiennych opisujących wybór tematów
    if (grepl(";m_(jp|h);", opis)) {
      src = polacz()
      kryteriaRozprawki = suppressMessages(
        pobierz_kryteria_oceny(src) %>%
        inner_join(pobierz_testy(src) %>% filter_(~czy_egzamin == TRUE)) %>%
        filter_(~id_skali == idSkali, ~typ_pytania == "rozprawka") %>%
        select_(~czesc_egzaminu, ~kryterium, ~numer_pytania, ~numer_kryterium) %>%
        collect() %>%
        left_join(czesciEgzaminow) %>%
        mutate_(.dots = setNames(list(~gsub("^(0|I|II|III|IV|V)_.*$", "\\1",
                                            numer_pytania),
                                      ~gsub("^(0|I|II|III|IV|V)(_|)(.*)$", "\\3",
                                            numer_pytania)),
                                 c("temat", "zadanie")))
      )
      rozlacz(src)
      zadaniaTematy =
        select_(kryteriaRozprawki, ~czesc_egzaminu, ~czy_sf, ~zadanie) %>%
        distinct()
      mapowanieNr = list("0" = 0, "I" = 1, "II" = 2, "III" = 3, "IV" = 4,
                         "V" = 5, "VI" = 6, "VII" = 7, "VIII" = 8, "IX" = 9)
      for (j in 1:nrow(zadaniaTematy)) {
        tematy = suppressMessages(
          semi_join(kryteriaRozprawki, zadaniaTematy[j, ]) %>%
            select_(~prefiks, ~zadanie, ~czy_sf, ~temat) %>%
            mutate_(.dots = setNames(list(~NA), "n")) %>%
            distinct()
        )
        maskaTematy = vector(mode = "list", length = nrow(tematy))
        names(maskaTematy) = paste0("t", mapowanieNr[tematy$temat],
                                    ifelse(zadaniaTematy$czy_sf[j], "", "nf"),
                                    "_", sub("^m_", "", tematy$prefiks))
        for (k in 1:nrow(tematy)) {
          maskaZmienne =
            suppressMessages(semi_join(kryteriaRozprawki, tematy[k, ])$kryterium)
          maskaTematy[[k]] =
            as.numeric(rowSums(!is.na(dane[, maskaZmienne, drop = FALSE])) > 0)
          tematy$n[k] = sum(maskaTematy[[k]])
        }
        maskaTematy = as.data.frame(maskaTematy)
        maskaTematy[rowSums(maskaTematy) == 0, ] = NA
        dane = cbind(dane, maskaTematy[, -which.max(tematy$n), drop = FALSE])
        message("W części egzaminu '", zadaniaTematy$czesc_egzaminu[j],
                "' utworzono ", ncol(maskaTematy) - 1, " zmienne/ych ",
                "opisujące/ych wybór tematu wypracowania.\n",
                "Rozkład wybieralności tematów w danych wzorcowych:")
        with(tematy, print(data.frame(temat = temat,
                                      n = format(n, big.mark = "'"),
                                      "%" = paste0(format(100 * n / sum(n),
                                                          digits = 1, nsmall = 1),
                                                   "%"),
                                      check.names = FALSE)))
      }
      rm(maskaTematy, kryteriaRozprawki, zadaniaTematy, tematy)
    }
    zmienneTematy = names(dane)[grep("^[t][[:digit:]](nf|)_.+$", names(dane))]
    # dołączanie zmiennych opisujących grupowanie i przystępowanie do przedmiotów
    if (grepl(";m_jp;", opis)) {
      zmienneGrupujace = c("typ_szkoly", "s_pol_r", "czy_sf")
    } else if (grepl(";m_m;", opis)) {
      zmienneGrupujace = c("typ_szkoly", "s_mat_r", "czy_sf")
    } else {
      zmienneGrupujace = c("typ_szkoly", "czy_sf")
    }
    czesci = unique(czesciEgzaminow$prefiks)
    wyborCzesci = as.data.frame(matrix(nrow = nrow(dane), ncol = length(czesci),
                                       dimnames = list(NULL, czesci)))
    for (j in czesci) {
      temp = dane[, filter_(czesciEgzaminow, ~prefiks == j)$kryterium]
      wyborCzesci[[j]] = rowSums(!is.na(temp)) > 0
    }
    rm(temp)
    names(wyborCzesci) = sub("m_", "s_", names(wyborCzesci))
    wyborCzesci = wyborCzesci[, !(names(wyborCzesci) %in% c("s_pol_p", "s_mat_p")),
                              drop = FALSE]
    zmienneSelekcja = names(wyborCzesci)
    dane = cbind(dane, wyborCzesci)
    rm(wyborCzesci)
    czyStaraFormula = unique(czesciEgzaminow$czy_sf)
    if (length(czyStaraFormula) > 1) {
      maskaZmienne = setdiff(filter_(czesciEgzaminow, ~czy_sf)$kryterium,
                             filter_(czesciEgzaminow, ~!czy_sf)$kryterium)
      temp = dane[, maskaZmienne]
      dane = cbind(dane, czy_sf = rowSums(!is.na(temp)) > 0)
      rm(temp)
    } else {
      zmienneGrupujace = setdiff(zmienneGrupujace, "czy_sf")
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
    if (grepl(";m_(jp|m);", opis)) {
      nazwaZmPR = names(dane)[grepl("^s_(mat|pol)_r$", names(dane))]
      grupy = within(grupy, {
        grupa = paste0(get("typ_szkoly"), " ",
                       ifelse(get(nazwaZmPR), "PP i PR", "tylko PP"))
      })
    } else {
      grupy = within(grupy, {
        grupa = get("typ_szkoly")
      })
    }
    if ("czy_sf" %in% zmienneGrupujace) {
      grupy = within(grupy, {
        grupa = paste0(grupa, " ", ifelse(get("czy_sf"), "sf", "nf"))
      })
    }
    # usuwanie z danych zdających spoza obsługiwanych grup
    lPrzed = nrow(dane)
    dane = suppressMessages(semi_join(dane, grupy))
    lPo = nrow(dane)
    if (lPo != lPrzed) {
      message(" Usunięto ", format(lPrzed - lPo, big.mark = "'"), " zdających, ",
              "należących do grup, które nie występują w zbiorowości 'wzorcowej' ",
              "i nie da się im w związku z tym przypisać wyskalowanych wyników ",
              "(np. uczniowie LO, którzy w 2015 r. zdawali maturę w 'starej formule').")
    }
    # jeśli nic w bazie nie znaleźliśmy, to robimy skalowanie wzorcowe
    if (!is.data.frame(parametrySkala)) {
      daneWzorcowe = filter_(dane, ~populacja_wy & !pomin_szkole)
      # czyszczenie wyników laureatów
      for (p in unique(czesciEgzaminow$prefiks)) {
        maskaObserwacje = daneWzorcowe[[paste0("laur_", p)]] %in% TRUE
        maskaZmienne = filter_(czesciEgzaminow, ~prefiks == p)$kryterium %>%
          unique()
        daneWzorcowe[maskaObserwacje, maskaZmienne] = NA
      }
      # będziemy wyrzucać wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
      maskaZmienne = unique(c("id_obserwacji", "id_testu", zmienneGrupujace,
                              zmienneKryteria, zmienneTematy, zmienneSelekcja))
      daneWzorcowe = daneWzorcowe[, maskaZmienne]
      if (proba > 0) {
        daneWzorcowe = daneWzorcowe[sample(nrow(daneWzorcowe), proba), ]
      }
      # skalowanie wzorcowe
      message("\n### Skalowanie wzorcowe ###\n")
      opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(wyniki)[i],
                                     wieleGrup = zmienneGrupujace,
                                     nigdyNieUsuwaj = "^(s|t[[:digit:]]+)(nf|)_",
                                     processors = processors)
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
      zmienneKryteriaPoUsuwaniu =
        wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
      # wyliczanie rzetelności empirycznych
      oszacowania = suppressMessages(
        mWzorcowe[[1]][[length(mWzorcowe[[1]])]]$zapis %>%
          left_join(grupy)
      )
      names(oszacowania) = sub(tolower( names(wyniki)[i]), names(wyniki)[i],
                               names(oszacowania))
      oszacowania = oszacowania[, c("id_obserwacji", "grupa", names(wyniki)[i],
                                    "id_testu")]
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
      # parametry do wyprowadzanie oszacowań na 0;1 w ramach LO i w ramach T
      maskaLO = grep("^LO ", oszacowania$grupa)
      maskaT = grep("^T ", oszacowania$grupa)
      standaryzacja = cbind(select_(grupy, ~grupa), sr = NA, os = NA)
      standaryzacja = within(standaryzacja, {
        sr[grepl("^LO", grupa)] =  mean(oszacowania[[names(wyniki)[i]]][maskaLO])
        sr[grepl("^T", grupa)] = mean(oszacowania[[names(wyniki)[i]]][maskaT])
        os[grepl("^LO", grupa)] =  sd(oszacowania[[names(wyniki)[i]]][maskaLO])
        os[grepl("^T", grupa)] = sd(oszacowania[[names(wyniki)[i]]][maskaT])
      })

      rm(mWzorcowe, daneWzorcowe, oszacowania, warPopGr, maskaLO, maskaT)
      message("\n### Przypisywanie oszacowań wszystkim zdającym ###\n")
    } else {
      # w przeciwnym wypadku podstawiamy zapisane w bazie parametry
      # i sprawdzamy, czy ktoś już ma zapisane oszacowania
      wartosciZakotwiczone = as.data.frame(parametrySkala)  # pozbywamy się "tbl_df-owatości"
      zmienneKryteriaPoUsuwaniu =
        zmienneKryteria[zmienneKryteria %in% unique(wartosciZakotwiczone$zmienna2)]

      daneWyskalowane = wczytaj_wyniki_wyskalowane(katalogWyskalowane,
                                                   rodzajEgzaminu, rok, idSkali)
      lPrzed = nrow(dane)
      dane = suppressMessages(anti_join(dane, daneWyskalowane))
      rm(daneWyskalowane)
      lPo = nrow(dane)
      if (lPo == 0) {
        message("\n### Brak zdających, dla których trzeba by wyliczyć oszacowania. ###\n")
        next
      } else if (lPo < lPrzed) {
        message("\n### Wyliczanie oszacowań dla ", format(lPo, big.mark = "'"),
                " zdających, ###\n    którzy ich jeszcze nie mają.")
      } else {
        message("\n### Wyliczanie oszacowań dla wszystkich zdających ###\n")
      }
    }
    dane = dane[, maskaZmienne]
    if (proba > 0) {
      dane = dane[sample(nrow(dane), proba), ]
    }
    # skalowanie dla oszacowań
    opisWszyscy = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, names(wyniki)[i],
                                  wieleGrup = zmienneGrupujace,
                                  wartosciZakotwiczone, processors = processors)
    mWszyscy = skaluj(dane, opisWszyscy, "id_obserwacji", tytul = tytulWszyscy,
                       zmienneDolaczaneDoOszacowan = "id_testu")

    oszacowania = suppressMessages(
      mWszyscy[[1]][[length(mWszyscy[[1]])]]$zapis %>%
        inner_join(grupy) %>%
        inner_join(standaryzacja))
    # wyprowadzanie oszacowań na 0;1 w ramach LO i w ramach T
    oszacowania = within(oszacowania, {
      assign(names(wyniki)[i], (get(names(wyniki)[i]) - sr ) / os)
    })

    rm(mWszyscy, dane)
    # przypisywanie wyników
    wyniki[[i]] = list(
      skalowania = data.frame(skalowanie = skalowanie, opis = opis,
                              estymacja = "MML (Mplus)", id_skali = idSkali,
                              do_prezentacji = FALSE, data = Sys.Date(),
                              stringsAsFactors = FALSE),
      skalowania_grupy = data.frame(id_skali = idSkali, skalowanie = skalowanie,
                                    grupa = grupy$grupa, stringsAsFactors = FALSE),
      skalowania_elementy = NULL,
      skalowania_obserwacje =
        data.frame(id_skali = idSkali, skalowanie = skalowanie,
                   dane[, c("id_obserwacji", "id_testu")],
                   estymacja = "EAP", nr_pv = -1,
                   wynik = oszacowania[[names(wyniki)[i]]],
                   bs = NA,
                   grupa = oszacowania$grupa, stringsAsFactors = FALSE),
      usunieteKryteria = setdiff(zmienneKryteria, zmienneKryteriaPoUsuwaniu)
    )
    if (!is.data.frame(parametrySkala)) {
      wyniki[[i]][["skalowania_elementy"]] =
        zmien_parametry_na_do_bazy(wartosciZakotwiczone, idSkali, skalowanie,
                                   rzetelnoscEmpiryczna, standaryzacja,
                                   select_(grupy, ~grupa, ~gr_tmp1))
    }
    class(wyniki[[i]]) = c(class(wyniki), "wynikiSkalowania")
    attributes(wyniki[[i]])$dataSkalowania = Sys.time()
  }
  # koniec
  class(wyniki) = c(class(wyniki), "listaWynikowSkalowania")
  if (zapisz) {
    nazwaObiektu = paste0("g", rok, "Skalowanie")
    assign(nazwaObiektu, wyniki)
    save(list = nazwaObiektu, file = paste0(nazwaObiektu, ".RData"))
  }
  return(wyniki)
}
