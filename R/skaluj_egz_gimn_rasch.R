#' @title Procedury skalowania egzaminow
#' @description
#' Funkcja przeprowadza skalowanie części mat.-przyr. egzaminu gimnazjalnego
#' z użyciem modelu Rascha (na potrzeby maturalnego Kalkulatora EWD).
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
#'   \item{\code{usunieteKryteria} wektor tekstowy z nazwami (pseudo)kryteriów, które
#'         zostały usunięte podczas skalowania wzorcowego;}
#' }
#' @seealso \code{\link[EWDskalowanie]{skaluj}},
#' \code{\link[EWDskalowanie]{procedura_1k_1w}},
#' \code{\link{sprawdz_wyniki_skalowania}}
#' @import EWDdane
#' @importFrom EWDskalowanie procedura_1k_1w skaluj
#' @export
skaluj_egz_gimn_rasch = function(rok, processors = 2,
                                 opis = "skalowanie do Kalkulatora EWD",
                                 katalogSurowe = "../../dane surowe",
                                 katalogWyskalowane = "../../dane wyskalowane",
                                 zapisz = TRUE, skala = NULL, proba = -1) {
  stopifnot(is.numeric(rok), length(rok) == 1,
            is.numeric(processors), length(processors) == 1,
            is.character(opis), length(opis) == 1,
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.character(katalogWyskalowane), length(katalogWyskalowane) == 1,
            is.logical(zapisz), length(zapisz) == 1,
            is.null(skala) | is.numeric(skala) | is.character(skala),
            is.numeric(proba), length(proba) == 1)
  stopifnot(as.integer(rok) == rok, rok >= 2002,
            processors %in% (1:32),
            dir.exists(katalogSurowe),
            dir.exists(katalogWyskalowane),
            zapisz %in% c(TRUE, FALSE),
            as.integer(proba) == proba, proba == -1 | proba > 0)
  if (!is.null(skala)) {
    stopifnot(length(skala) == 1)
  }

  # sprawdzanie, czy w bazie są zapisane skala i jakieś skalowanie z parametrami
  if (is.null(skala)) {
    skala = paste0("^ewd;g[hm](|_[hmp])R;", rok)
  } else if (is.character(skala)) {
    if (!grepl("^ewd;g", skala)) {
      warning("Skale, których opis ma pasować do wyrażenia '", skala,
              "' raczej nie odnoszą się do egzaminu gimnazjalnego!", immediate. = TRUE)
    }
  }
  parametry = suppressMessages(pobierz_parametry_skalowania(skala, doPrezentacji = TRUE,
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
  # sortujemy tak, żeby w nowej formule gh i gm były na końcu
  parametry = parametry[order(grepl(";g[hm];", parametry$opis)), ]

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
    # będziemy wyrzucać wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
    maskaZmienne = grep("^(id_obserwacji|id_testu|[kpst]_[[:digit:]]+)$", names(dane))
    zmienneKryteria = names(dane[grep("^[kpst]_[[:digit:]]+$", names(dane))])
    tytulWzorcowe = paste0(names(wyniki)[i], rok, " wzor")
    tytulWszyscy = paste0(names(wyniki)[i], rok, " wszyscy")
    # jeśli nic w bazie nie znaleźliśmy, to robimy skalowanie wzorcowe
    if (!is.data.frame(parametrySkala) | nrow(normySkala) == 0) {
      if (is.data.frame(parametrySkala)) {
        wartosciZakotwiczone = as.data.frame(parametrySkala)  # pozbywamy się "tbl_df-owatości"
      } else {
        wartosciZakotwiczone = NULL
      }
      zmLaur = sub("R$", "", paste0("laur_", names(wyniki)[i]))
      # trochę baroku, żeby móc wyskalować egzamin z 2005 r., który mamy tylko w danych z CKE
      if (all(c(zmLaur, "populacja_wy", "pomin_szkole") %in% names(dane))) {
        daneWzorcowe = subset(dane, get("populacja_wy") & !get("pomin_szkole") &
                                !get(zmLaur))
      } else {
        warning("Brak danych kontekstowych - skalowanie wzorcowe zostanie ",
                "przeprowadzone na wszystkich zdających, bez żadnych wykluczeń.",
                immediate. = TRUE)
        daneWzorcowe = dane
      }
      daneWzorcowe = daneWzorcowe[, maskaZmienne]
      if (proba > 0) {
        daneWzorcowe = daneWzorcowe[sample(nrow(daneWzorcowe), proba), ]
      }
      # skalowanie wzorcowe
      message("\n### Skalowanie wzorcowe ###\n")
      opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(wyniki)[i],
                                     wartosciZakotwiczone, rasch = TRUE,
                                     processors = processors)
      egWzorcowe = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji",
                          tytul = tytulWzorcowe, zmienneDolaczaneDoOszacowan = "id_testu")
      daneWzorcowe = cbind(daneWzorcowe[, "id_obserwacji", drop = FALSE],
                           sumaG = rowSums(daneWzorcowe[, zmienneKryteria]))
      daneWzorcowe = na.omit(daneWzorcowe)
      oszacowania =
        egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$zapis
      names(oszacowania) = sub(tolower( names(wyniki)[i]), names(wyniki)[i],
                               names(oszacowania))
      # wyliczanie rzetelności empirycznej
      rzetelnoscEmpiryczna = oszacowania[, names(wyniki)[i]]
      rzetelnoscEmpiryczna = var(rzetelnoscEmpiryczna)
      # uśrednianie oszacowań, aby były funkcją sum punktów (i przynależności do grup)
      oszacowania[, names(wyniki)[i]] =
        oszacowania[, names(wyniki)[i]] / sqrt(rzetelnoscEmpiryczna)
      if (rok < 2012) {
        maksSuma = setNames(50, "sumaG")
      } else {
        maksSuma = NULL
      }
      temp =
        przewidywanie_rasch(daneWzorcowe,
                            oszacowania[, c("id_obserwacji", names(wyniki)[i])],
                            maks = maksSuma)
      names(temp$mapowanie) = sub("^suma$", "wartosc", names(temp$mapowanie))
      names(temp$mapowanie) = sub(paste0("^", names(wyniki)[i], "$"),
                                  "wartosc_zr", names(temp$mapowanie))
      temp$mapowanie = temp$mapowanie[, c("wartosc", "wartosc_zr")]
      normySkala = data.frame(id_skali = idSkali, skalowanie = skalowanie,
                              grupa = "", temp$mapowanie,
                              stringsAsFactors = FALSE)
      # zapamiętywanie parametrów modelu
      wartosciZakotwiczone =
        egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$parametry$surowe
      odsUtraconejWariancji = oszacowania$odsUtraconejWariancji
      oszacowania = suppressMessages(
        left_join(temp$oszacowania, oszacowania[, !grepl(names(wyniki)[i],
                                                         names(oszacowania))])
      )
      rm(egWzorcowe, daneWzorcowe, temp)
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
        message("\n### Brak zdających, dla których trzeba by wyliczyć oszacowania. ###\n")
        next
      } else if (lPo < lPrzed) {
        message("\n### Przypisywanie oszacowań ", format(lPo, big.mark = "'"),
                " zdającym, ###\n    którzy ich jeszcze nie mają.")
      } else {
        message("\n### Przypisywanie oszacowań wszystkim zdającym ###\n")
      }
    }
    # zamiast skalowania dla oszacowań
    dane = cbind(dane[, c("id_obserwacji", "id_testu")],
                 wartosc = rowSums(dane[, zmienneKryteria]))
    dane = suppressMessages(inner_join(dane, normySkala))
    # przypisywanie wyników
    wyniki[[i]] = list(
      skalowania = data.frame(skalowanie = skalowanie, opis = opis,
                              estymacja = "MML (Mplus)", id_skali = idSkali,
                              do_prezentacji = FALSE, data = Sys.Date(),
                              stringsAsFactors = FALSE),
      skalowania_grupy = data.frame(id_skali = idSkali, skalowanie = skalowanie,
                                    grupa = "", stringsAsFactors = FALSE),
      skalowania_elementy = NULL,
      normy = normySkala,
      skalowania_obserwacje =
        data.frame(id_skali = idSkali, skalowanie = skalowanie,
                   dane[, c("id_obserwacji", "id_testu")],
                   estymacja = "EAP", nr_pv = -1,
                   wynik = dane$wartosc_zr / sqrt(rzetelnoscEmpiryczna),
                   bs = NA,
                   grupa = "", stringsAsFactors = FALSE),
      usunieteKryteria = vector(mode = "character", length = 0),
      odsUtraconejWariancji = odsUtraconejWariancji
    )
    if (!is.data.frame(parametrySkala)) {
      wyniki[[i]][["skalowania_elementy"]] =
        zmien_parametry_na_do_bazy(wartosciZakotwiczone, idSkali, skalowanie,
                                   rzetelnoscEmpiryczna)
    }
    class(wyniki[[i]]) = c(class(wyniki), "wynikiSkalowania")
    attributes(wyniki[[i]])$dataSkalowania = Sys.time()
  }
  # koniec
  class(wyniki) = c(class(wyniki), "listaWynikowSkalowania")
  if (zapisz) {
    nazwaObiektu = paste0("gRasch", rok, "Skalowanie")
    assign(nazwaObiektu, wyniki)
    save(list = nazwaObiektu, file = paste0(nazwaObiektu, ".RData"))
  }
  return(wyniki)
}
