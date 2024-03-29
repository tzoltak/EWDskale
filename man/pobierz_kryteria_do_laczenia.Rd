% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_kryteria_do_laczenia.R
\name{pobierz_kryteria_do_laczenia}
\alias{pobierz_kryteria_do_laczenia}
\title{Laczenie kryteriow}
\usage{
pobierz_kryteria_do_laczenia(skale, nf = FALSE, src = NULL)
}
\arguments{
\item{skale}{wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
regularnym identyfikującymi skale po kolumnie 'opis'}

\item{nf}{opcjonalnie wartość logiczna (domyślnie FALSE) - czy w przypadku
matury (od 2015 r.) zaznaczać sufiksem dopisywanym do \code{czesc_egzaminu},
czy kryterium pochodzi z arkusza w "starej", czy w "nowej" formule egzaminu?}

\item{src}{NULL połączenie z bazą danych IBE zwracane przez funkcję
\code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
automatycznego nawiązania połączenia.}
}
\value{
data table
}
\description{
Na podstawie informacji pozwalających zidentyfikować skale funkcja
przygotowuje obiekt z kryteriami oceny, który obrabiają potem funkcje
\code{\link{lacz_kryteria_z_nr_zadan}} lub
\code{\link{lacz_kryteria_z_korelacji}}.
}
