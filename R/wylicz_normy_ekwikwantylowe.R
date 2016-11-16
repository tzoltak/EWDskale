#' @title Obliczanie norm ekwikwantylowych
#' @description
#' Funkcja służy do obliczenia norm ekwikwantylowych na podstawie wyników
#' egzaminów pobranych wcześniej przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}.
#' @details
#' Nominalnie działa dla wszystkich egzaminów, ale ponieważ w obecnej wersji nie
#' udostępnia grupowania, więc w odniesieniu do matury jest mało użyteczna
#' (niemniej obecnie normalizacja ekwikwantylowa nie jest wykorzystywana
#' w odniesieniu do wyników matury).
#'
#' Dodatkowe uproszczenie polega na tym, że w zwracanym data frame'ie kolumna
#' \code{skalowanie} jest zawsze wypełniona wartością \code{1}, co w ogólności
#' nie zawsze musi być słuszne.
#' @param egzamin ciąg znaków: "sprawdzian" | "egzamin gimnazjalny" | "matura"
#' @param rok liczba całkowita - rok, dla którego mają zostać obliczone normy
#' @param katalogZDanymi ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się pliki .RData z wynikami surowymi egzaminów i danymi kontekstowymi
#' o uczniach i szkołach ściągnięte wcześniej przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param ... ew. parametry przekazywane do funkcji
#' \code{\link[ZPD]{normy_ekwikwantylowe}}
#' @return data table
#' @importFrom stats setNames na.omit
#' @import EWDdane
#' @import ZPD
#' @export
wylicz_normy_ekwikwantylowe = function(egzamin, rok, katalogZDanymi, ...) {
  stopifnot(is.character(egzamin), length(egzamin) == 1,
            is.numeric(rok), length(rok) == 1,
            is.character(katalogZDanymi), length(katalogZDanymi) == 1)
  stopifnot(egzamin %in% c("sprawdzian", "egzamin gimnazjalny", "matura"),
            dir.exists(katalogZDanymi),
            rok >= 2002)

  katalogRoboczy = getwd()
  on.exit(setwd(katalogRoboczy))
  setwd(katalogZDanymi)
  plikDane = paste0(egzamin, " ", rok, ".RData")
  if (!file.exists(plikDane)) {
    stop("Plik '", plikDane, "' nie istnieje.")
  }
  if (egzamin == "sprawdzian") {
    # sprawdzian normalizuje się z wyników "wszystkie jak są"
    dane = suppressWarnings(EWDdane:::wczytaj_wyniki_egzaminu(plikDane))
  } else {
    # a inne egzaminy na podstawie populacji z wykluczeniami
    plikKontekstowe = paste0(egzamin, "-kontekstowe.RData")
    if (!file.exists(plikKontekstowe)) {
      stop("Plik '", plikKontekstowe, "' nie istnieje.")
    }
    daneKontekstowe = EWDdane:::wczytaj_dane_kontekstowe(plikKontekstowe, TRUE, rok)
    dane = suppressWarnings(EWDdane:::wczytaj_wyniki_egzaminu(plikDane, daneKontekstowe))
  }
  skaleTesty = attributes(dane)$skaleTesty

  zmienneDoNormalizacji = names(dane)[grep("_suma$", names(dane))]
  normy = setNames(vector(mode = "list", length = length(zmienneDoNormalizacji)),
                   zmienneDoNormalizacji)
  for (i in zmienneDoNormalizacji) {
    message("Zmienna '", i, "'.")
    normalizacja = normy_ekwikwantylowe(as.matrix(dane[, i]), ...)
    idTestow = na.omit(as.matrix(unique(dane[, sub("^(.*)_suma$", "id_testu_\\1", i)])))
    maska = lapply(skaleTesty, function(x, idTestow) {return(all(idTestow %in% x))},
                   idTestow = idTestow)
    idSkali = as.numeric(names(skaleTesty))[unlist(maska)]
    if (length(idSkali) == 0) {
      stop("Musisz najpierw utworzyć skalowania o opisie 'normalizacja",
           "ekwikwantylowa EWD' w tablicy 'skalowania'.")
    }
    normy[names(normy) == i][[1]] =
      data.frame(id_skali = idSkali, skalowanie = 1, grupa = '',
                 wartosc = as.numeric(names(normalizacja)),
                 wartosc_zr = as.vector(normalizacja),
                 stringsAsFactors = FALSE)
  }
  normy = bind_rows(normy)
  return(normy)
}
