#' @title Procedury skalowania egzaminow
#' @description
#' Funkcja przeprowadza skalowanie wyników sprawdzianu.
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
#' lista z elementami:
#' \itemize{
#'   \item{\code{idSkali} id_skali w bazie;}
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
skaluj_spr = function(rok, processors = 2, katalogSurowe = "../../dane surowe",
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
    skala = paste0("^ewd;s;", rok)
  } else if (is.character(skala)) {
    if (!grepl("^ewd;s;", skala)) {
      warning("Skala, której opis ma pasować do wyrażenia '", skala,
              "' raczej nie odnosi się do sprawdzianu!")
    }
  }
  parametry = suppressMessages(pobierz_parametry_skalowania(skala, doPrezentacji = TRUE,
                                           parametryzacja = "mplus"))
  if (nrow(parametry) == 0) {
    if (is.character(skala)) {
      stop("Nie znaleziono skali o opisie pasującym do wyrażenia '", skala,
           "', która byłaby oznaczona jako 'do prezentacji'.")
    } else {
      stop("Nie znaleziono skali o id_skali = ", skala,
           ", która byłaby oznaczona jako 'do prezentacji'.")
    }
  } else if (nrow(parametry) > 1) {
    stop("Znaleziono wiele skal oznaczonych jako 'do prezentacji', ",
         "których opis pasuje do wyrażenia '", skala, "'.")
  }
  rodzajEgzaminu = parametry$rodzaj_egzaminu[1]
  idSkali = parametry$id_skali[1]
  skalowanie = parametry$skalowanie[1]
  parametry = parametry$parametry[[1]]

  message(rodzajEgzaminu, " ", rok, " (id_skali: ", idSkali, "):")
  # wczytywanie danych z dysku i sprawdzanie, czy jest dla kogo skalować
  dane = wczytaj_wyniki_surowe(katalogSurowe, rodzajEgzaminu, "", rok, idSkali)
  # wyrzucamy wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
  maskaZmienne = grep("^(id_obserwacji|id_testu|[kpst]_[[:digit:]]+)$", names(dane))
  zmienneKryteria = names(dane[grep("^[kpst]_[[:digit:]]+$", names(dane))])
  tytulWzorcowe = paste0("spr", rok, " wzor")
  tytulWszyscy = paste0("spr", rok, " wszyscy")
  # jeśli nic w bazie nie znaleźliśmy, to robimy skalowanie wzorcowe
  if (!is.data.frame(parametry)) {
    daneWzorcowe = filter_(dane, ~populacja_wy & !pomin_szkole & !laur_s)
    daneWzorcowe = daneWzorcowe[, maskaZmienne]
    # skalowanie wzorcowe
    message("\n### Skalowanie wzorcowe ###\n")
    opisWzorcowe = procedura_1k_1w(zmienneKryteria, "s", processors = processors)
    sprWzorcowe = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji",
                         tytul = tytulWzorcowe, zmienneDolaczaneDoOszacowan = "id_testu")
    # wyliczanie rzetelności empirycznej
    rzetelnoscEmpiryczna = sprWzorcowe[[1]][[length(sprWzorcowe[[1]])]]$zapis[["s"]]
    rzetelnoscEmpiryczna = var(rzetelnoscEmpiryczna)

    wartosciZakotwiczone =
      sprWzorcowe[[1]][[length(sprWzorcowe[[1]])]]$parametry$surowe
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
    wartosciZakotwiczone = as.data.frame(parametry)  # pozbywamy się "tbl_df-owatości"
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
  opisWszyscy = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, "s",
                                 wartosciZakotwiczone, processors = processors)
  sprWszyscy = skaluj(dane , opisWszyscy , "id_obserwacji", tytul = tytulWszyscy,
                      zmienneDolaczaneDoOszacowan = "id_testu")

  # koniec
  wyniki = list(
    idSkali = idSkali,
    skalowanie = skalowanie,
    usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)],
    parametry = NULL,
    oszacowania = sprWszyscy[[1]][[length(sprWszyscy[[1]])]]$zapis,
    rzetelnoscEmpiryczna = NULL
  )
  if (!is.data.frame(parametry)) {
    wyniki[["parametry"]] = wartosciZakotwiczone
    wyniki[["rzetelnoscEmpiryczna"]] = rzetelnoscEmpiryczna
  }
  class(wyniki) = c(class(wyniki), "wynikiSkalowania")
  attributes(wyniki)$dataSkalowania = Sys.time()
  if (zapisz) {
    nazwaObiektu = paste0("s", rok, "Skalowanie")
    assign(nazwaObiektu, wyniki)
    save(list = nazwaObiektu, file = paste0(nazwaObiektu, ".RData"))
  }
  return(wyniki)
}
