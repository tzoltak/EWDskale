% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lacz_kryteria_z_nr_zadan.R
\name{lacz_kryteria_z_nr_zadan}
\alias{lacz_kryteria_z_nr_zadan}
\title{Laczenie kryteriow na podstawie numerow zadan}
\usage{
lacz_kryteria_z_nr_zadan(skale, src = NULL)
}
\arguments{
\item{skale}{wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
regularnym identyfikującymi skale po kolumnie 'opis'}

\item{src}{NULL połączenie z bazą danych IBE zwracane przez funkcję
\code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
automatycznego nawiązania połączenia.}
}
\value{
lista, której każdy element jest dwuelemntową listą, zawierającą id
skali oraz data frame, której można użyć jako argument \code{elementy}
funkcji \code{\link[ZPDzapis]{edytuj_skale}}
}
\description{
Funkcja łączy, w ramach podanych skal, kryteria oceny poszczególnych zadań
(wyróżnionych na podstawie numeracji w arkuszu egzaminu) w pseudokryteria.

Jeśli w ramach skali są już zdefiniowane jakieś pseudokryteria, to nie
będą one w ogóle uwzględniane w procesie łączenia.
}
