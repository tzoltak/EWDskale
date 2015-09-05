#' @title Wczytywanie wynikow egzaminow zapisanych na dysku
#' @description
#' Funkcja wczytuje wyniki surowe egzaminu, zapisane wcześniej na dysku
#' funkcją \code{\link[EWDdane]{pobierz_wyniki_surowe}} z pakietu EWDdane.
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param rodzajEgzaminu ciąg znaków
#' @param czescEgzaminu ciąg znaków
#' @param rok liczba naturalna
#' @param idSkali liczba naturalna - id_skali w bazie dla skali, która ma zostać
#' zastosowana do wyników surowych
#' @param kryteria opcjonalnie wektor tekstowy z nazwami wszystkich
#' kryteriów oceny, jakie powinna zawierać dana część/ci egzaminu
#' @return data frame (data table)
#' @import ZPD
wczytaj_wyniki_surowe = function(katalogDane, rodzajEgzaminu, czescEgzaminu,
                                   rok, idSkali, kryteria = NULL) {
  stopifnot(is.character(katalogDane), length(katalogDane) == 1,
            is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
            is.numeric(rok), length(rok) == 1,
            is.numeric(idSkali), length(idSkali) == 1)
  stopifnot(dir.exists(katalogDane),
            rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura"))

  katalogDane = paste0(sub("/$", "", katalogDane), "/")
  plikDane = paste0(katalogDane, rodzajEgzaminu, " ", rok, ".RData")
  if (!file.exists(plikDane)) {
    stop("Nie można wczytać danych z pliku '", plikDane, "'. Plik nie istnieje.")
  }
  obiekty = load(plikDane)
  src = polacz()
  idTestu = pobierz_skale(src) %>%
    filter_(~id_skali == idSkali) %>%
    select_(~id_testu) %>%
    collect() %>%
    as.matrix() %>%
    as.vector()
  for (i in obiekty) {
    temp = zastosuj_skale(get(i), src, idSkali)
    # Jeśli skala ma przypisanych wiele testów, to (przynajmniej dla skal EWD)
    # każdy zdający pisał tylko jeden test i nie trzeba usuwać informacji o id_testu.
    # Jeśli skala ma przypisany jeden test, to zbiera wyniki wielu części egzaminu
    # i trzeba usunąć z danych "pierwotne" id_testu tych poszczególnych części
    # a zastąpić je id_testu specjalnie stworzonego wcześniej testu, powiązanego
    # ze skalą (co dzieje się kawałek dalej).
    if (length(idTestu) == 1) {
      temp = select_(temp, ~-id_testu)
    }
    maska1 = grepl("^[kp]_[[:digit:]]+$", names(temp))
    if (!is.null(kryteria)) {
      maska2 = all(names(temp)[maska1] %in% kryteria)
    } else {
      maska2 = TRUE
    }
    if (any(maska1) & all(maska2) &
        all(c("wynikiSurowe", "czescEgzaminu") %in% class(get(i)))) {
      if (!exists("dane")) {
        assign("dane", temp)
      } else {
        dane = suppressMessages(full_join(get("dane"), temp))
        if (all(kryteria %in% names(dane)) & !is.null(kryteria)) {
          break
        }
      }
    }
  }
  rozlacz(src)
  if (length(idTestu) == 1) {
    dane = mutate_(dane, dots.= setNames(list(~idTestu),
                                         "id_testu"))
  }
  if (!exists("dane")) {
    stop("W pliku '", plikDane, "' nie ma obiektu, który zawierałby wyniki ",
         "wszystkich (pseudo)kryteriów oceny części '", czescEgzaminu,
         "' egzaminu '", rodzajEgzaminu, "'.")
  }
  rm(list = c(obiekty, "obiekty", "temp"))
  # wczytywanie danych kontekstowych i filtrowanie populacji "wzorcowej"
  plikDane = paste0(katalogDane, rodzajEgzaminu, "-kontekstowe.RData")
  if (!file.exists(plikDane)) {
    stop("Nie można wczytać danych z pliku '", plikDane, "'. Plik nie istnieje.")
  }
  obiekty = load(plikDane)
  maska = grepl("^[[:alpha:]]Kontekstowe$", obiekty)
  if (!any(maska)) {
    stop("W pliku '", plikDane, "' brak obiektu zawierającego dane kontekstowe.")
  } else if (!("daneKontekstowe" %in% class(get(obiekty[maska])))) {
    stop("W pliku '", plikDane, "' brak obiektu zawierającego dane kontekstowe.")
  }
  daneKontekstowe = get(obiekty[maska])
  rm(list = c(obiekty, "obiekty", "maska"))
  dane = suppressMessages(inner_join(daneKontekstowe, dane))
  return(dane)
}
#' @title Wczytywanie wynikow egzaminow zapisanych na dysku
#' @description
#' Funkcja wczytuje wyskalowane wyniki egzaminu, zapisane wcześniej na dysku
#' funkcją \code{\link[EWDdane]{pobierz_wyniki_wyskalowane}} z pakietu EWDdane.
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wyskalowanymi wynikami egzaminów, pobranymi przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_wyskalowane}}
#' @param rodzajEgzaminu ciąg znaków
#' @param idSkali liczba naturalna
#' @return data frame (data table)
#' @import dplyr
wczytaj_wyniki_wyskalowane = function(katalogDane, rodzajEgzaminu, idSkali) {
  stopifnot(is.character(katalogDane), length(katalogDane) == 1,
            is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
            is.numeric(idSkali), length(idSkali) == 1)
  stopifnot(dir.exists(katalogDane),
            rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura"))

  katalogDane = paste0(sub("/$", "", katalogDane), "/")
  plikDane = paste0(katalogDane, rodzajEgzaminu, ".RData")
  if (!file.exists(plikDane)) {
    stop("Nie można wczytać danych z pliku '", plikDane, "'. Plik nie istnieje.")
  }
  obiekty = load(plikDane)
  obiekty = obiekty[grep("^.Wyskalowane$", obiekty)]
  if (length(obiekty) > 1) {
    stop("Plik '", plikDane, "' zawiera zbyt wiele obiektów o nazwach pasujących ",
         "do wyrażenia '.Wyskalowane'.")
  }
  if (is.null(get(obiekty))) {
    return(as.data.frame(matrix(nrow = 0, ncol = 1,
                                dimnames = list(NULL, "id_obserwacji"))))
  } else {
    return(filter_(get(obiekty), ~id_skali == idSkali))
  }
}
