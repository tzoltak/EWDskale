#' @title Procedury skalowania egzaminow
#' @description
#' Funkcja przeprowadza skalowanie czterech wskaźników osiągnięć maturalnych:
#' 1) humanistycznego (j. polski, historia, WOS), 2) matematyczno-przyrodniczego
#' (matematyka, biologia, chemia, fizyka, geografia, informatyka),
#' 3) polonistycznego (j. polski), 4) matematycznego (matematyka), na potrzeby
#' obliczania trzyletnich wskaźników dla LO i techników. Wykorzystywane są
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
#' @param usunWieleNaraz opcjonalnie wartość logiczna - jeśli wiele
#' (pseudo)kryteriów oceny ma dyskryminację poniżej 0,2, to czy usuwać je
#' wszystkie w jednym kroku?
#' @param nieEstymuj opcjonalnie wartość logiczna - jeśli istnieją już zapisane
#' pliki \code{.out}, których nazwy odpowiadają tym, jakie powstałyby przy
#' estymacji modeli dla danych "wzorcowych", to czy wczytać je, zamiast
#' przeprowadzania estymacji w Mplusie?
#' @param tylkoDaneDoUIRTa jeśli TRUE, zamiast przeprowadzić (lub wczytać już
#'   wykonane) skalowanie funkcja zrzuca jedynie w katalogu \code{katalogSurowe}
#'   pliki CSV z danymi do skalowania parametrów zadań UIRT-em
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
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
#' @importFrom stats setNames sd
#' @import EWDdane
#' @importFrom EWDskalowanie procedura_1k_1w skaluj
#' @export
skaluj_matura = function(rok, processors = 2, opis = "skalowanie do EWD",
                         katalogSurowe = "../../dane surowe",
                         katalogWyskalowane = "../../dane wyskalowane",
                         zapisz = TRUE, skala = NULL, proba = -1,
                         usunWieleNaraz = FALSE, nieEstymuj = FALSE,
                         tylkoDaneDoUIRTa = FALSE,
                         src = NULL) {
  doPrezentacji = TRUE
  stopifnot(is.numeric(rok), length(rok) == 1,
            is.numeric(processors), length(processors) == 1,
            is.character(opis), length(opis) == 1,
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.character(katalogWyskalowane), length(katalogWyskalowane) == 1,
            is.logical(zapisz), length(zapisz) == 1,
            is.null(skala) | is.numeric(skala) | is.character(skala),
            is.numeric(proba), length(proba) == 1,
            is.logical(usunWieleNaraz), length(usunWieleNaraz) == 1,
            is.logical(nieEstymuj), length(nieEstymuj) == 1,
            is.logical(tylkoDaneDoUIRTa), length(tylkoDaneDoUIRTa) == 1,
            dplyr::is.src(src) | is.null(src)
  )
  stopifnot(as.integer(rok) == rok, rok >= 2010,
            processors %in% (1:32),
            dir.exists(katalogSurowe),
            dir.exists(katalogWyskalowane),
            zapisz %in% c(TRUE, FALSE),
            as.integer(proba) == proba, proba == -1 | proba > 0,
            usunWieleNaraz %in% c(TRUE, FALSE),
            nieEstymuj %in% c(TRUE, FALSE)
  )
  if (is.null(src)) {
    src = ZPD::polacz()
  }
  if (!is.null(skala)) {
    stopifnot(length(skala) == 1)
    doPrezentacji = NA
  }
  if (rok > 2020) {
    stop("Funkcja nie obsługuje skalowania dla egzaminów po 2020 r.")
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
    pobierz_parametry_skalowania(skala, doPrezentacji = doPrezentacji, parametryzacja = "mplus", src = src)
  )
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
  skale = parametry %>%
    group_by(.data$id_skali) %>%
    summarise(
      lSkalowan = n(),
      opis = .data$opis_skali[1]
    ) %>%
    ungroup()
  if (any(skale$lSkalowan > 1)) {
    stop("Dla skal '", paste0(skale$opis[skale$lSkalowan > 1], collapse = "', '"),
         "' znaleziono wiele skalowań oznaczonych jako 'do prezentacji'.")
  }

  wyniki = vector(mode = "list", length = nrow(skale))
  names(wyniki) = gsub("^.*ewd;([^;]+);.*$", "\\1", parametry$opis_skali)
  class(wyniki) = c("listaWynikowSkalowania", class(wyniki))
  for (i in 1:nrow(parametry)) {
    idSkali = parametry$id_skali[i]
    opis = parametry$opis_skali[i]
    skalowanie = parametry$skalowanie[i]
    parametrySkala = parametry$parametry[[i]]
    if (!is.data.frame(parametrySkala)) {
      parametrySkala = NULL
    }
    rzetelnoscEmpiryczna = attributes(parametrySkala)$"r EAP"
    standaryzacja = attributes(parametrySkala)$"paramStd"

    message(rodzajEgzaminu, " ", rok, " (id_skali: ", idSkali, ", '", opis,
            "'; skalowanie ", skalowanie, ".):")
    # wczytywanie danych z dysku i sprawdzanie, czy jest dla kogo skalować
    dane = wczytaj_wyniki_surowe(katalogSurowe, rodzajEgzaminu, "", rok, idSkali, src = src)
    dane = filter(dane, .data$typ_szkoly %in% c("LO", "T"))

    zmienneKryteria = names(dane)[grep("^[kp]_[[:digit:]]+$", names(dane))]
    tytulWzorcowe = paste0(names(wyniki)[i], rok, " wzor")
    tytulWszyscy = paste0(names(wyniki)[i], rok, " wszyscy")
    # przyłączanie informacji o tym, kto co zdawał
    czesciEgzaminow = suppressMessages(
      pobierz_kryteria_oceny(src, skale = FALSE) %>%
        semi_join(data.frame(kryterium = zmienneKryteria), copy = TRUE) %>%
        select("kryterium", "id_testu") %>%
        left_join(pobierz_testy(src)) %>%
        filter(.data$czy_egzamin) %>%
        select("kryterium", "prefiks", "arkusz") %>%
        distinct() %>%
        collect() %>%
        mutate(czy_sf = !(substr(.data$arkusz, 7, 7) %in% c("X", "Y", "Z"))) %>%
        select(-"arkusz") %>%
        distinct()
    )
    # sufiks służący do wyróżniania param. w grupie zdających w nowej formule
    #   ale tylko, jeśli jednocześnie są też tacy, co zdają w starej
    czyDwieFormuly = all(c(FALSE, TRUE) %in% czesciEgzaminow$czy_sf)
    if (czyDwieFormuly) {
      sufiksNF = "nf"
    } else {
      sufiksNF = ""
    }
    # ew. tworzenie zmiennych opisujących wybór tematów
    if (grepl(";m_(jp|h);", opis)) {
      # zawikłane (można pomyśleć, czy nie dałoby się łatwiej):
      # najpierw tworzymy sobie listę wszystkich kryteriów skali, podmieniając
      # (rozmnażając) pseudokryteria na ich kryteria składowe
      kryteriaRozprawki = suppressMessages(
        pobierz_kryteria_oceny(src, krytSkladowe = TRUE) %>%
          inner_join(pobierz_testy(src) %>% filter(.data$czy_egzamin == TRUE)) %>%
          filter(.data$id_skali == idSkali) %>%
          select("kryterium", "typ_pytania", "kryt_skladowe") %>%
          distinct() %>%
          collect() %>%
          mutate(kryterium_temp = .data$kryterium,
                 kryterium = ifelse(.data$typ_pytania == "pseudokryterium",
                                    .data$kryt_skladowe, .data$kryterium)) %>%
          select("kryterium", "kryterium_temp")
      )
      # następnie odfiltrowujemy tylko rozprawki, i wracamy z tą informacją
      # na poziom elemntów skali (a więc pseudokryteriów, jeśli takie były)
      kryteriaRozprawki = suppressMessages(
        pobierz_kryteria_oceny(src) %>%
          inner_join(kryteriaRozprawki, copy = TRUE) %>%
          filter(.data$typ_pytania == "rozprawka") %>%
          select("kryterium_temp") %>%
          distinct() %>%
          collect()
      )
      names(kryteriaRozprawki) = "kryterium"
      # i używamy tej informacji do odfiltrowania elementów skali, do których
      # mamy przyłączone sporo innych, potrzebnych informacji
      kryteriaRozprawki = suppressMessages(
        pobierz_kryteria_oceny(src) %>%
          inner_join(pobierz_testy(src) %>% filter(.data$czy_egzamin == TRUE)) %>%
          semi_join(kryteriaRozprawki, copy = TRUE) %>%
          select("czesc_egzaminu", "kryterium", "numer_pytania", "numer_kryterium") %>%
          collect() %>%
          left_join(czesciEgzaminow) %>%
          mutate(numer_pytania = sub("^0*_", "0_", .data$numer_pytania)) %>%
          mutate(temat = gsub("^(0|I|II|III|IV|V)_.*$", "\\1",
                              .data$numer_pytania),
                 zadanie = gsub("^(0|I|II|III|IV|V)(_|)(.*)$", "\\3",
                                .data$numer_pytania)) %>%
          distinct()
      )
      zadaniaTematy =
        select(kryteriaRozprawki, "czesc_egzaminu", "czy_sf", "zadanie") %>%
        distinct()
      mapowanieNr = list("0" = 0, "I" = 1, "II" = 2, "III" = 3, "IV" = 4,
                         "V" = 5, "VI" = 6, "VII" = 7, "VIII" = 8, "IX" = 9)
      for (j in 1:nrow(zadaniaTematy)) {
        tematy = suppressMessages(
          semi_join(kryteriaRozprawki, zadaniaTematy[j, ]) %>%
            select("prefiks", "zadanie", "czy_sf", "temat") %>%
            mutate(n = NA) %>%
            distinct()
        )
        maskaTematy = vector(mode = "list", length = nrow(tematy))
        names(maskaTematy) = paste0("t", mapowanieNr[tematy$temat],
                                    ifelse(zadaniaTematy$czy_sf[j], "", sufiksNF),
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
    # dołączanie zmiennych opisujących przystępowanie do przedmiotów
    czesci = select(czesciEgzaminow, "prefiks", "czy_sf") %>% distinct()
    wyborCzesci = as.data.frame(matrix(nrow = nrow(dane), ncol = nrow(czesci)))
    colnames(wyborCzesci) = paste0("s", ifelse(czesci$czy_sf, "", sufiksNF),
                                   "_", sub("^m_", "", czesci$prefiks))
    for (j in 1:nrow(czesci)) {
      temp = dane[, suppressMessages(semi_join(czesciEgzaminow, czesci[j, ]))$kryterium]
      wyborCzesci[, j] = as.numeric(rowSums(!is.na(temp)) > 0)
    }
    rm(temp)
    wyborCzesci = wyborCzesci[, -grep("^s(nf|)_(pol|mat)_p$", names(wyborCzesci)),
                              drop = FALSE]
    zmienneSelekcja = names(wyborCzesci)
    dane = cbind(dane, wyborCzesci)
    rm(wyborCzesci)
    # ew. dołączenia zmiennej opisującej, w jakiej uczeń zdawał formule (2015 r.)
    if (czyDwieFormuly) {
      maskaZmienne = setdiff(filter(czesciEgzaminow, .data$czy_sf)$kryterium,
                             filter(czesciEgzaminow, !.data$czy_sf)$kryterium)
      temp = dane[, maskaZmienne]
      dane = cbind(dane, czy_sf = rowSums(!is.na(temp)) > 0)
      rm(temp)
    }
    # dołączanie zmiennych opisujących grupowanie
    if (grepl(";m_(jp|m);", opis)) {
      zmienneGrupujace = intersect(c("typ_szkoly", "s_pol_r", "snf_pol_r",
                                     "s_mat_r", "snf_mat_r", "czy_sf"),
                                   names(dane))
    } else {
      zmienneGrupujace = intersect(c("typ_szkoly", "czy_sf"), names(dane))
    }
    grupy = distinct(dane[, zmienneGrupujace, drop = FALSE])
    for (j in ncol(grupy):1) {
      grupy = grupy[order(grupy[[j]]), 1:ncol(grupy), drop = FALSE]
    }
    # usuwanie grup, których nie jesteśmy w stanie sensownie obsłużyć:
    # uczniów LO piszących starą formułę
    if ("czy_sf" %in% zmienneGrupujace) {
      grupy = filter(grupy, !(.data$typ_szkoly == "LO" & .data$czy_sf))
    }
    grupy = cbind(grupy, gr_tmp1 = 1:nrow(grupy))
    # ładne nazwy grup
    if (grepl(";m_(jp|m);", opis)) {
      nazwyZmPR = names(dane)[grepl("^s(nf|)_(mat|pol)_r$", names(dane))]
      grupy = within(grupy, {
        grupa = paste0(get("typ_szkoly"), " ",
                       ifelse(apply(grupy[, nazwyZmPR, drop = FALSE], 1, sum) > 0,
                              "PP i PR", "tylko PP"))
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
    # zamienianie nazw grup na numery w parametrySkala
    if (!is.null(parametrySkala)) {
      for (j in 1:nrow(grupy)) {
        parametrySkala$typ = sub(paste0("[.]gr[.]", grupy$grupa[j], "$"),
                                 paste0(".gr", grupy$gr_tmp1[j]),
                                 parametrySkala$typ)
      }
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
    if (!is.data.frame(parametrySkala) | tylkoDaneDoUIRTa) {
      daneWzorcowe = filter(dane, .data$populacja_wy & !.data$pomin_szkole)
      # czyszczenie wyników laureatów
      for (p in unique(czesciEgzaminow$prefiks)) {
        maskaObserwacje = daneWzorcowe[[paste0("laur_", p)]] %in% TRUE
        maskaZmienne = filter(czesciEgzaminow, .data$prefiks == p)$kryterium %>%
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
      # jeszcze obsługa braku kotwicy między starą a nową formułą
      if ("czy_sf" %in% zmienneGrupujace &
          !any(duplicated(czesciEgzaminow$kryterium))) {
        wartosciZakotwiczone = data.frame(
          typ = rep(c("mean", "variance"), each = 2),
          zmienna1 = names(wyniki)[i],
          zmienna2 = vector(mode = "character", length = 4),
          wartosc = rep(c(0, 1), each = 2),
          stringsAsFactors = FALSE)
        wartosciZakotwiczone$typ = paste0(wartosciZakotwiczone$typ, ".gr",
                                          grupy$gr_tmp1[!duplicated(grupy$czy_sf)])
      } else {
        wartosciZakotwiczone = NULL
      }
      if (tylkoDaneDoUIRTa) {
        write.csv(daneWzorcowe, paste0(katalogSurowe, '/', opis, '_s', idSkali, '_sk', skalowanie, ".csv"), na = '', row.names = FALSE)
        next
      }
      # skalowanie wzorcowe
      message("\n### Skalowanie wzorcowe ###\n")
      zmienneKonstrukt = setdiff(c(zmienneKryteria, zmienneTematy,
                                   zmienneSelekcja), zmienneGrupujace)
      opisWzorcowe = procedura_1k_1w(zmienneKonstrukt, names(wyniki)[i],
                                     wieleGrup = zmienneGrupujace,
                                     wartosciZakotwiczone,
                                     nigdyNieUsuwaj = "^(s|t[[:digit:]]+)(nf|)_",
                                     processors = processors,
                                     usunWieleNaraz = usunWieleNaraz,
                                     usunMimoKotwicy = TRUE)
      mWzorcowe = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji",
                         tytul = tytulWzorcowe, zmienneDolaczaneDoOszacowan = "id_testu",
                         bezWartosciStartowychParametrowTypu = "threshold",
                         nieEstymuj = nieEstymuj)
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
      zmienneKryteriaPoUsuwaniu = setdiff(zmienneKryteriaPoUsuwaniu,
                                          c(zmienneTematy, zmienneSelekcja))
      # obliczanie rzetelności empirycznych
      oszacowania = suppressWarnings(suppressMessages(
        mWzorcowe[[1]][[length(mWzorcowe[[1]])]]$zapis %>%
          left_join(grupy)
      ))
      names(oszacowania) = sub(tolower( names(wyniki)[i]), names(wyniki)[i],
                               names(oszacowania))
      oszacowania = oszacowania[, c("id_obserwacji", "grupa", names(wyniki)[i],
                                    "id_testu")]
      rzetelnoscEmpiryczna = oszacowania[, c("grupa", names(wyniki)[i])]
      names(rzetelnoscEmpiryczna) = sub(names(wyniki)[i], "oszacowanie",
                                        names(rzetelnoscEmpiryczna))
      warPopGr = suppressMessages(
        filter(wartosciZakotwiczone, grepl("^variance", .data$typ)) %>%
          group_by(.data$typ) %>%
          summarise(gr_tmp1 = as.numeric(sub("^variance.gr", "", .data$typ)),
                    war_pop = .data$wartosc) %>%
          left_join(select(grupy, "gr_tmp1", "grupa")) %>%
          select(-"typ", -"gr_tmp1")
      )
      rzetelnoscEmpiryczna = suppressMessages(
        group_by(rzetelnoscEmpiryczna, .data$grupa) %>%
          summarise(war = var(.data$oszacowanie, na.rm = TRUE)) %>%
          full_join(warPopGr) %>%
          right_join(grupy) %>%
          mutate(wartosc = .data$war / .data$war_pop) %>%
          select("grupa", "wartosc")
      )
      # parametry do wyprowadzanie oszacowań na 0;1 w ramach LO i w ramach T
      maskaLO = grep("^LO", oszacowania$grupa)
      maskaT = grep("^T", oszacowania$grupa)
      standaryzacja = cbind(select(grupy, "grupa"), sr = NA, os = NA)
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
        message("\n### Brak zdających, dla których trzeba by obliczyć oszacowania. ###\n")
        next
      } else if (lPo < lPrzed) {
        message("\n### Obliczanie oszacowań dla ", format(lPo, big.mark = "'"),
                " zdających, ###\n    którzy ich jeszcze nie mają.")
      } else {
        message("\n### Obliczanie oszacowań dla wszystkich zdających ###\n")
      }
    }
    maskaZmienne = unique(c("id_obserwacji", "id_testu", zmienneGrupujace,
                            zmienneKryteria, zmienneTematy, zmienneSelekcja))
    dane = dane[, maskaZmienne]
    if (proba > 0) {
      dane = dane[sample(nrow(dane), proba), ]
    }
    # skalowanie dla oszacowań
    zmienneKonstrukt = setdiff(c(zmienneKryteriaPoUsuwaniu, zmienneTematy,
                                 zmienneSelekcja), zmienneGrupujace)
    opisWszyscy = procedura_1k_1w(zmienneKonstrukt, names(wyniki)[i],
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

    rm(mWszyscy)
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
                   oszacowania[, c("id_obserwacji", "id_testu")],
                   estymacja = "EAP", nr_pv = -1,
                   wynik = oszacowania[[names(wyniki)[i]]],
                   bs = NA,
                   grupa = oszacowania$grupa, stringsAsFactors = FALSE),
      usunieteKryteria = setdiff(zmienneKryteria, zmienneKryteriaPoUsuwaniu)
    )
    if (!is.data.frame(parametrySkala)) {
      wyniki[[i]][["skalowania_elementy"]] =
        try(zmien_parametry_na_do_bazy(wartosciZakotwiczone, idSkali, skalowanie,
                                       rzetelnoscEmpiryczna, standaryzacja,
                                       select(grupy, "grupa", "gr_tmp1")))
      if ("try-error" %in% class(wyniki[[i]][["skalowania_elementy"]])) {
        wyniki[[i]][["skalowania_elementy"]] = list(
          wartosciZakotwiczone, idSkali, skalowanie, rzetelnoscEmpiryczna,
          standaryzacja, select(grupy, "grupa", "gr_tmp1")
        )
      }
    }
    class(wyniki[[i]]) = c("wynikiSkalowania", class(wyniki))
    attributes(wyniki[[i]])$dataSkalowania = Sys.time()

    # ew. zapis
    if (zapisz) {
      nazwaObiektu = paste0("m", rok, "Skalowanie")
      assign(nazwaObiektu, wyniki)
      save(list = nazwaObiektu, file = paste0(nazwaObiektu, ".RData"))
      rm(list = nazwaObiektu)
    }
  }
  return(wyniki)
}
