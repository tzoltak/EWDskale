#' @title Procedury skalowania egzaminow
#' @description
#' Funkcja przeprowadza skalowanie wyników sprawdzianu.
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
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
#' @return
#' lista klasy \code{listaWynikowSkalowania} z jednym elementem "s",
#' klasy \code{wynikiSkalowania}, będącym listą o elementach:
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
#'   \item{\code{usunieteKryteria} wektor tekstowy z nazwami (pseudo)kryteriów, które
#'         zostały usunięte podczas skalowania wzorcowego;}
#' }
#' @seealso \code{\link[EWDskalowanie]{skaluj}},
#' \code{\link[EWDskalowanie]{procedura_1k_1w}},
#' \code{\link{sprawdz_wyniki_skalowania}}
#' @importFrom stats var
#' @import EWDdane
#' @importFrom EWDskalowanie procedura_1k_1w skaluj
#' @export
skaluj_spr = function(rok, processors = 2, opis = "skalowanie do EWD",
                      katalogSurowe = "../../dane surowe",
                      katalogWyskalowane = "../../dane wyskalowane",
                      zapisz = TRUE, skala = NULL, proba = -1,
                      src = NULL) {
  stopifnot(is.numeric(rok), length(rok) == 1,
            is.numeric(processors), length(processors) == 1,
            is.character(opis), length(opis) == 1,
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.character(katalogWyskalowane), length(katalogWyskalowane) == 1,
            is.logical(zapisz), length(zapisz) == 1,
            is.null(skala) | is.numeric(skala) | is.character(skala),
            is.numeric(proba), length(proba) == 1,
            dplyr::is.src(src) | is.null(src))
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
    skala = paste0("^ewd;s;", rok)
  } else if (is.character(skala)) {
    if (!grepl("^ewd;s;", skala)) {
      warning("Skala, której opis ma pasować do wyrażenia '", skala,
              "' raczej nie odnosi się do sprawdzianu!", immediate. = TRUE)
    }
  }
  parametry =
    suppressMessages(pobierz_parametry_skalowania(skala, doPrezentacji = TRUE,
                                                  parametryzacja = "mplus",
                                                  src = src))
  if (nrow(parametry) == 0) {
    if (is.character(skala)) {
      stop("Nie znaleziono skali o opisie pasującym do wyrażenia '", skala,
           "', która byłaby oznaczona jako 'do prezentacji'.")
    } else {
      stop("Nie znaleziono skali o id_skali = ", skala,
           ", która byłaby oznaczona jako 'do prezentacji'.")
    }
  } else if (nrow(parametry) > 1) {
    stop("Znaleziono wiele skal lub skalowań oznaczonych jako 'do prezentacji', ",
         "których opis pasuje do wyrażenia '", skala, "'.")
  }
  rodzajEgzaminu = parametry$rodzaj_egzaminu
  idSkali = parametry$id_skali
  opis = parametry$opis_skali
  skalowanie = parametry$skalowanie
  parametry = parametry$parametry[[1]]
  rzetelnoscEmpiryczna = attributes(parametry)$"r EAP"$wartosc

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
  if (!is.data.frame(parametry)) {
    daneWzorcowe = filter(dane, .data$populacja_wy & !.data$pomin_szkole)
    if (!all(is.na(dane$laur_s))) {
      daneWzorcowe = filter(dane, !.data$laur_s)
    }
    daneWzorcowe = daneWzorcowe[, maskaZmienne]
    if (proba > 0) {
      daneWzorcowe = daneWzorcowe[sample(nrow(daneWzorcowe), proba), ]
    }
    # skalowanie wzorcowe
    message("\n### Skalowanie wzorcowe ###\n")
    opisWzorcowe = procedura_1k_1w(zmienneKryteria, "s", processors = processors)
    sprWzorcowe = skaluj(daneWzorcowe, opisWzorcowe, "id_obserwacji",
                         tytul = tytulWzorcowe,
                         zmienneDolaczaneDoOszacowan = "id_testu")
    # obliczanie rzetelności empirycznej
    rzetelnoscEmpiryczna =
      sprWzorcowe[[1]][[length(sprWzorcowe[[1]])]]$zapis[["s"]]
    rzetelnoscEmpiryczna = var(rzetelnoscEmpiryczna)

    wartosciZakotwiczone =
      sprWzorcowe[[1]][[length(sprWzorcowe[[1]])]]$parametry$surowe
    # Kiedyś tak robiliśmy, ale to wpływa tylko na to, do czego i o ile są
    # ściągane EAPY, co wolał bym mieć dobrze określone, bez względu na dane.
    #wartosciZakotwiczone =
    #  wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
    zmienneKryteriaPoUsuwaniu =
      wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
    rm(sprWzorcowe, daneWzorcowe)
    message("\n### Obliczanie oszacowań dla wszystkich zdających ###\n")
  } else {
    # w przeciwnym wypadku podstawiamy zapisane w bazie parametry
    # i sprawdzamy, czy ktoś już ma zapisane oszacowania
    wartosciZakotwiczone = as.data.frame(parametry)  # pozbywamy się "tbl_df-owatości"
    zmienneKryteriaPoUsuwaniu =
      zmienneKryteria[zmienneKryteria %in% unique(wartosciZakotwiczone$zmienna2)]

    daneWyskalowane = wczytaj_wyniki_wyskalowane(katalogWyskalowane,
                                                 rodzajEgzaminu, rok, idSkali)
    lPrzed = nrow(dane)
    dane = suppressMessages(anti_join(dane, daneWyskalowane))
    lPo = nrow(dane)
    if (lPo == 0) {
      message("\n### Brak zdających, dla których trzeba by obliczyć oszacowania. ###\n")
      wyniki = list(s = NULL)
      class(wyniki) = c("listaWynikowSkalowania", class(wyniki))
      return(wyniki)
    } else if (lPo < lPrzed) {
      message("\n### Obliczanie oszacowań dla ", format(lPo, big.mark = "'"),
              " zdających, ###\n    którzy ich jeszcze nie mają.\n")
    } else {
      message("\n### Obliczanie oszacowań dla wszystkich zdających ###\n")
    }
  }
  dane = dane[, maskaZmienne]
  if (proba > 0) {
    dane = dane[sample(nrow(dane), proba), ]
  }
  # skalowanie dla oszacowań
  opisWszyscy = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, "s",
                                 wartosciZakotwiczone, processors = processors)
  sprWszyscy = skaluj(dane, opisWszyscy, "id_obserwacji", tytul = tytulWszyscy,
                      zmienneDolaczaneDoOszacowan = "id_testu")

  oszacowania = sprWszyscy[[1]][[length(sprWszyscy[[1]])]]$zapis
  rm(sprWszyscy, dane)
  # koniec
  wyniki = list(
    skalowania = data.frame(skalowanie = skalowanie, opis = opis,
                      estymacja = "MML (Mplus)", id_skali = idSkali,
                      do_prezentacji = FALSE, data = Sys.Date(),
                      stringsAsFactors = FALSE),
    skalowania_grupy = data.frame(id_skali = idSkali, skalowanie = skalowanie,
                                  grupa = "", stringsAsFactors = FALSE),
    skalowania_elementy = NULL,
    skalowania_obserwacje =
      data.frame(id_skali = idSkali, skalowanie = skalowanie,
                 oszacowania[, c("id_obserwacji", "id_testu")],
                 estymacja = "EAP", nr_pv = -1,
                 wynik = oszacowania$s / sqrt(rzetelnoscEmpiryczna),
                 bs = oszacowania$s_se / sqrt(rzetelnoscEmpiryczna),
                 grupa = "", stringsAsFactors = FALSE),
    usunieteKryteria =
      zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)]
  )
  if (!is.data.frame(parametry)) {
    wyniki[["skalowania_elementy"]] =
      zmien_parametry_na_do_bazy(wartosciZakotwiczone, idSkali, skalowanie,
                                 rzetelnoscEmpiryczna)
  }
  class(wyniki) = c("wynikiSkalowania", class(wyniki))
  attributes(wyniki)$dataSkalowania = Sys.time()
  wyniki = list("s" = wyniki)
  class(wyniki) = c("listaWynikowSkalowania", class(wyniki))
  if (zapisz) {
    nazwaObiektu = paste0("s", rok, "Skalowanie")
    assign(nazwaObiektu, wyniki)
    save(list = nazwaObiektu, file = paste0(nazwaObiektu, ".RData"))
  }
  return(wyniki)
}
