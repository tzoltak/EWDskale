#' @title Laczenie kryteriow wedlug numerow zadan
#' @description
#' Funkcja łączy kryteria oceny poszczególnych zadań (wyróżnionych na podstawie numeracji
#' w arkuszu egzaminu) w pseudokryteria.
#' @param rodzajEgzaminu ciąg znaków - rodzaj egzaminu
#' @param czescEgzaminu ciąg znaków - część egzaminu
#' @param rokEgzaminu liczba całkowita - rok egzaminu
#' @param nieLacz wartość logiczna - czy nie łączyć kryteriów w pseudokryteria, a tylko
#' zwrócić kryteria z podanych części egzaminów?
#' @param zrodloDanychODBC żródło ODBC
#' @return Funkcja zwraca data frame, który można uzyć jako argument \code{elementy}
#' funkcji \code{edytuj_skale()} z pakietu \code{ZPD}.
#' @examples
#' \dontrun{
#' rodzajEgzaminu = "matura"
#' czescEgzaminu = "informatyka podstawowa"
#' rokEgzaminu = 2014
#' kryteria_do_polaczenia(rodzajEgzaminu, czescEgzaminu, rokEgzaminu)
#' }
#' @import plyr
#' @export
lacz_kryteria_zadan <- function(rodzajEgzaminu, czescEgzaminu, rokEgzaminu, nieLacz=FALSE, zrodloDanychODBC = "EWD"){
  stopifnot(
    is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    is.numeric(rokEgzaminu)     , length(rokEgzaminu) == 1,
    is.character(czescEgzaminu),
    is.logical(nieLacz), length(nieLacz) == 1,
    is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1 )
  # pobranie numerow kryteriow oraz ich opisow
  zapytanie = "SELECT DISTINCT id_kryterium, pytania.opis AS opis, typ, czesc_egzaminu
               FROM arkusze
                 JOIN testy USING (arkusz)
                 JOIN testy_kryteria USING (id_testu)
                 JOIN kryteria_oceny USING (id_kryterium)
                 JOIN pytania USING (id_pytania)
               WHERE rodzaj_egzaminu = ? AND czesc_egzaminu IN ( ? )
                 AND EXTRACT(YEAR from data_egzaminu) = ?
               ORDER BY id_kryterium"
  tryCatch({
      P = odbcConnect(as.character(zrodloDanychODBC))
      tablicaDanych = sqlExecute(P, zapytanie, data =data.frame(rodzajEgzaminu, czescEgzaminu, rokEgzaminu)  , fetch = TRUE, stringsAsFactors = FALSE)
    },
    error = stop,
    finally = odbcClose(P)
  )
  if (nrow(tablicaDanych) == 0) stop("Nie znaleziono żadnych kryteriów - sprawdź podane argumenty.")
  # takie sprawdzenie, na wszelki wypadek
  maskaCzesc = gsub("^matura;([^;]+);.*$", "\\1", tablicaDanych$opis) != tablicaDanych$czesc_egzaminu
  if (any(maskaCzesc)) {
    warning("W kryteriach o podanych opisach zmieniono nazwę części egzaminu:\n",
            paste0(tablicaDanych$opis, " -> ", tablicaDanych$czesc_egzaminu, "\n"))
  }
  if (!nieLacz) {
    # usuniecie z opisów '_numer'
    tablicaDanych$opis = gsub("_[[:digit:]_]+$", "", tablicaDanych$opis)
    # wskazanie duplikatów opisów
    opisyDoPolaczenia = with(tablicaDanych, {
      unique(opis[duplicated(opis) & !(typ == "rozprawka" & grepl("j. polski", opis))])
    })
    pseudokryteria = vector(mode="list", length=length(opisyDoPolaczenia))
    for(i in seq_along(opisyDoPolaczenia)){
      pseudokryteria[[i]] = with(tablicaDanych, {id_kryterium[opis == opisyDoPolaczenia[i]]})
      pseudokryteria[[i]] = as.list(pseudokryteria[[i]])
      names(pseudokryteria[[i]]) = paste0("id_kryterium_", 1:length(pseudokryteria[[i]]))
      pseudokryteria[[i]]$opis = paste0("ewd;", opisyDoPolaczenia[i])
    }
    pseudokryteria = ldply(pseudokryteria, as.data.frame)
  } else {
    opisyDoPolaczenia = NULL
    pseudokryteria = NULL
  }
  # łączenie
  tablicaDanych = subset(tablicaDanych, !(tablicaDanych$opis %in% opisyDoPolaczenia))
  ret = rbind.fill(list(
    tablicaDanych[, "id_kryterium", drop=FALSE],
    pseudokryteria))
  if (!("opis" %in% names(ret))) ret = cbind(ret, opis = NA)
  ret = cbind(ret, id_pseudokryterium=NA, id_skrotu = NA)
  # estetyka
  maskaKolumny = grep("^id_kryterium", names(ret))
  ret = ret[, c(names(ret)[maskaKolumny], names(ret)[-maskaKolumny])]
  ret = ret[order(apply(ret[, maskaKolumny, drop=FALSE], 1, min, na.rm=TRUE)), ]

  return(ret)
}
