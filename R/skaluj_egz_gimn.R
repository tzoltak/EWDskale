#' @title Procedury skalowania egzaminow
#' @description
#' Funkcja przeprowadza skalowanie wyników egzaminu gimnazjalnego.
#' @param rok rok przeprowadzenie egzaminu
#' @param processors liczba rdzeni do wykorzystania przy estymacji
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
#' @return
#' lista klasy \code{listaWynikowSkalowania}, której elementy są listami
#' klasy \code{wynikiSkalowania} i składają się z elementów:
#' \itemize{
#'   \item{\code{idSkali} id_skali w bazie;}
#'   \item{\code{skalowanie} nr skalowania w bazie;}
#'   \item{\code{usunieteKryteria} wektor tekstowy z nazwami (pseudo)kryteriów, które
#'         zostały usunięte podczas skalowania wzorcowego;}
#'   \item{\code{parametry} data frame z wyestymowanymi parametrami modelu w jego
#'         ostatecznej postaci (tj. takiej, jak w ostatnim kroku skalowania wzorcowego
#'         i w jedynym kroku skalowania na wszystkich zdających);}
#'   \item{\code{oszacowania} data frame zawierający id_obserwacji i wyliczone
#'         oszacowania umiejętności dla wszystkich zdających;}
#'   \item{\code{rzetelnoscEmpiryczna} rzetelność wyliczona na podstawie oszacowań ze
#'         skalowania wzorcowego (jako wariancja oszacowań EAP);}
#' }
#' @seealso \code{\link[EWDskalowanie]{skaluj}},
#' \code{\link[EWDskalowanie]{procedura_1k_1w}},
#' \code{\link{sprawdz_wyniki_skalowania}}
#' @import EWDdane
#' @importFrom EWDskalowanie procedura_1k_1w skaluj
#' @export
skaluj_egz_gimn = function(rok, processors = 2, katalogSurowe = "../../dane surowe",
                           katalogWyskalowane = "../../dane wyskalowane",
                           zapisz = TRUE, skala = NULL) {
  stopifnot(is.numeric(rok), length(rok) == 1,
            is.numeric(processors), length(processors) == 1,
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.character(katalogWyskalowane), length(katalogWyskalowane) == 1,
            is.logical(zapisz), length(zapisz) == 1,
            is.null(skala) | is.numeric(skala) | is.character(skala))
  stopifnot(as.integer(rok) == rok, rok >= 2002,
            processors %in% (1:32),
            dir.exists(katalogSurowe),
            dir.exists(katalogWyskalowane),
            zapisz %in% c(TRUE, FALSE))
  if (!is.null(skala)) {
    stopifnot(length(skala) == 1)
  }

  # sprawdzanie, czy w bazie są zapisane skala i jakieś skalowanie z parametrami
  if (is.null(skala)) {
    skala = paste0("^ewd;g[hm](|_[hmp]);", rok)
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
    parametrySkala = parametry$parametry[[i]][[1]]

    message(rodzajEgzaminu, " ", rok, " (id_skali: ", idSkali, ", '", opis,
            "'; skalowanie ", skalowanie, ".):")
    # wczytywanie danych z dysku i sprawdzanie, czy jest dla kogo skalować
    dane = wczytaj_wyniki_surowe(katalogSurowe, rodzajEgzaminu, "", rok, idSkali)
    # będziemy wyrzucać wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
    maskaZmienne = grep("^(id_obserwacji|id_testu|[kpst]_[[:digit:]]+)$", names(dane))
    zmienneKryteria = names(dane[grep("^[kpst]_[[:digit:]]+$", names(dane))])
    tytulWzorcowe = paste0("spr", rok, " wzor")
    tytulWszyscy = paste0("spr", rok, " wszyscy")
    # jeśli nic w bazie nie znaleźliśmy, to robimy skalowanie wzorcowe
    if (!is.data.frame(parametrySkala)) {
      zmLaur = paste0("laur_", names(wyniki)[i])
      daneWzorcowe = subset(dane, get("populacja_wy") & !get("pomin_szkole") &
                              !get(zmLaur))
      daneWzorcowe = daneWzorcowe[, maskaZmienne]
      # sztuczka, żeby przy skalowaniu gh i gm w nowej formule już nie usuwał (pseudo)kryteriów
      if ( ((names(wyniki)[i] == "gh") & all(c("gh_h", "gh_p") %in% names(wyniki))) |
           ((names(wyniki)[i] == "gm") & all(c("gm_p", "gm_m") %in% names(wyniki))) ) {
        # dajemy tu data frame, żeby nie było usuwania kryteriów, ale wtedy trzeba zadać w nim wartość oczekiwaną i wariancję
        zmUsuniete = unlist(lapply(wyniki, function(x) {return(x$usunieteKryteria)}))
        zmienneKryteria = zmienneKryteria[!(zmienneKryteria %in% zmUsuniete)]
        wartosciZakotwiczone = data.frame(typ = c("mean", "variance"),
                                          zmienna1 = names(daneWzorcowe)[i],
                                          zmienna2 = "", wartosc = c(0, 1),
                                          stringsAsFactors = FALSE)
      } else {
        wartosciZakotwiczone = NULL
      }
      # skalowanie wzorcowe
      message("\n### Skalowanie wzorcowe ###\n")
      opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(wyniki)[i],
                                     wartosciZakotwiczone, processors = processors)
      egWzorcowe = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji",
                          tytul = tytulWzorcowe, zmienneDolaczaneDoOszacowan = "id_testu")
      # wyliczanie rzetelności empirycznej
      rzetelnoscEmpiryczna = egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$zapis[[names(wyniki)[i]]]
      rzetelnoscEmpiryczna = var(rzetelnoscEmpiryczna)

      wartosciZakotwiczone =
        egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$parametry$surowe
      # Kiedyś tak robiliśmy, ale to wpływa tylko na to, do czego i o ile są
      # ściągane EAPY, co wolał bym mieć dobrze określone, bez względu na dane.
      #wartosciZakotwiczone =
      #  wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
      zmienneKryteriaPoUsuwaniu =
        wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
      message("\n### Wyliczanie oszacowań dla wszystkich zdających ###\n")
    } else {
      # w przeciwnym wypadku podstawiamy zapisane w bazie parametry
      # i sprawdzamy, czy ktoś już ma zapisane oszacowania
      wartosciZakotwiczone = as.data.frame(parametrySkala)  # pozbywamy się "tbl_df-owatości"
      zmienneKryteriaPoUsuwaniu =
        zmienneKryteria[zmienneKryteria %in% unique(wartosciZakotwiczone$zmienna2)]

      daneWyskalowane = wczytaj_wyniki_wyskalowane(katalogWyskalowane,
                                                   rodzajEgzaminu, idSkali)
      lPrzed = nrow(dane)
      dane = suppressMessages(anti_join(dane, daneWyskalowane))
      rm(daneWyskalowane)
      lPo = nrow(dane)
      if (lPo < lPrzed) {
        message("\n### Wyliczanie oszacowań dla ", format(lPo, big.mark = "'"),
                " zdających, ###\n    którzy ich jeszcze nie mają.")
      } else {
        message("\n### Wyliczanie oszacowań dla wszystkich zdających ###\n")
      }
    }
    dane = dane[, maskaZmienne]
    # skalowanie dla oszacowań
    opisWszyscy = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, names(wyniki)[i],
                                  wartosciZakotwiczone, processors = processors)
    egWszyscy = skaluj(dane, opisWszyscy, "id_obserwacji", tytul = tytulWszyscy,
                        zmienneDolaczaneDoOszacowan = "id_testu")
    # przypisywanie wyników
    wyniki[[i]] = list(
      idSkali = idSkali,
      skalowanie = skalowanie,
      usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)],
      parametry = NULL,
      oszacowania = egWszyscy[[1]][[length(egWszyscy[[1]])]]$zapis,
      rzetelnoscEmpiryczna = NULL
    )
    if (!is.data.frame(parametrySkala)) {
      wyniki[["parametry"]] = wartosciZakotwiczone
      wyniki[["rzetelnoscEmpiryczna"]] = rzetelnoscEmpiryczna
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
