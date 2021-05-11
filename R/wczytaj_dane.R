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
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
#' @details
#' Funkcja co do zasady dołącza do wyników egzaminu dane kontekstowe, zawężając
#' grupę zwracanych obserwacji do tych, dla których te dane istnieją. Jeśli
#' jednak stwierdzi, że w efekcie usunięci zostaliby wszyscy, to zwróci wyniki,
#' bez dołączania do nich danych kontekstowych. Dojdzie do tego w szczególności
#' przy wczytywaniu wyników egzaminu gimnazjalnego z lat wcześniejszych niż
#' 2006 r., gdyż są to dane CKE i dane pobierane funkcją
#' \code{\link[EWDdane]{pobierz_dane_kontekstowe}} ich nie obejmują.
#' @return data frame (data table)
#' @importFrom stats setNames
#' @import ZPD
wczytaj_wyniki_surowe = function(katalogDane, rodzajEgzaminu, czescEgzaminu,
                                 rok, idSkali, kryteria = NULL, src = NULL) {
  stopifnot(is.character(katalogDane), length(katalogDane) == 1,
            is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
            is.numeric(rok), length(rok) == 1,
            is.numeric(idSkali), length(idSkali) == 1,
            dplyr::is.src(src) | is.null(src))
  stopifnot(dir.exists(katalogDane),
            rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura"))
  if (is.null(src)) {
    src = ZPD::polacz()
  }

  katalogDane = paste0(sub("/$", "", katalogDane), "/")
  plikDane = paste0(katalogDane, rodzajEgzaminu, " ", rok, ".RData")
  if (!file.exists(plikDane)) {
    stop("Nie można wczytać danych z pliku '", plikDane, "'. Plik nie istnieje.")
  }
  obiekty = load(plikDane)
  idTestu = suppressMessages(
    pobierz_skale(src, doPrezentacji = NA) %>%
      filter(.data$id_skali == idSkali) %>%
      select(.data$id_testu) %>%
      semi_join(pobierz_testy(src) %>% filter(!.data$czy_egzamin)) %>%
      collect() %>%
      as.matrix() %>%
      as.vector() %>%
      unique()
  )
  if (length(idTestu) > 1) {
    stop("Jeśli skala jest powiązana z testem nie będącym 'atomową' częścią egzaminu, ",
         "to musi być powiązana z tylko jednym takim testem.")
  }
  for (i in obiekty) {
    temp = suppressMessages(zastosuj_skale(get(i), src, idSkali))
    # Jeśli skala ma przypisanych test który nie jest częścią egzaminu, to
    # trzeba usunąć z danych "pierwotne" id_testu poszczególnych części egzaminu
    # i zastąpić je id_testu właśnie tego testu (co dzieje się kawałek dalej).
    if (length(idTestu) == 1) {
      temp = temp[, !(names(temp) %in% "id_testu"), drop = FALSE]
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
  if (length(idTestu) == 1) {
    dane$id_testu =  idTestu
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
  temp = suppressMessages(inner_join(daneKontekstowe, dane))
  if (nrow(temp) > 0) {
    dane = temp
  } else {
    warning("Nie udało się przyłączyć zbioru danych kontekstowych do zbioru z wynikami surowymi.",
            immediate. = TRUE)
  }
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
#' @param rok liczba naturalna
#' @param idSkali liczba naturalna
#' @return data frame (data table)
#' @import dplyr
wczytaj_wyniki_wyskalowane = function(katalogDane, rodzajEgzaminu, rok, idSkali) {
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
  obiekty = obiekty[grep("^.Wyskalowane$", obiekty)]
  if (length(obiekty) > 1) {
    stop("Plik '", plikDane, "' zawiera zbyt wiele obiektów o nazwach pasujących ",
         "do wyrażenia '.Wyskalowane'.")
  }
  if (is.null(get(obiekty))) {
    return(as.data.frame(matrix(nrow = 0, ncol = 1,
                                dimnames = list(NULL, "id_obserwacji"))))
  } else {
    return(filter(get(obiekty), .data$id_skali == idSkali))
  }
}
