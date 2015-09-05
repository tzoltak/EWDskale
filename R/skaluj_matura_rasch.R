#' @title Procedury skalowania egzaminow.
#' @description
#' Funkcja przeprowadza skalowanie matury z matematyki łącznie na poziomach podstawowym
#' i rozszerzonym, z wykorzystaniem wielogrupowego modelu Rascha (na potrzeby maturalnego
#' Kalkulatora EWD). Grupy definiowane są przez wybór poziomu rozszerzonego i typ szkoły
#' (LO/T).
#' @param daneWzorcowe lista data frame'ów zawierających dane do skalowania wzorcowego
#' @param daneWszyscy lista data frame'ów zawierających zawierający dane wszystkich
#' zdających (do wyliczenia oszacowań umiejętności na podstawe parametrów ze skalowania
#' wzorcowego)
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @details
#' Argumenty (poza ostatnim) muszą być listami (mogą być jednoelementowe), których
#' elementy mają nazwy ze zbioru: \code{"matematyka podstawowa", "matematyka rozszerzona"}
#' i są data frame'ami zawierającymi dane z wynikami odpowiednich części matury.
#'
#' Schemat przekodowania sum punktów na oszacowania umiejętności wyliczany jest na
#' podstawie danych wzorcowych, przy pomocy funkcji \code{\link{przewidywanie_rasch}},
#' a następnie na jego podstawie przypisywane są wartości przewidywane wszystkim zdającym.
#'
#' Ponieważ różni zdający rozwiązywali różne zestawy zadań, a do tego model jest
#' wielogrupowy, funkcja nie wylicza (i nie zwraca) wartości rzetleności empirycznej.
#'
#' \bold{Model skalowania:} jednowymiarowy, wielogrupowy model Rascha (Rasch/Rasch Graded
#' Response), z grupami zdefiniowanymi przez typ szkoły (LO/T) i pisanie lub nie poziomu
#' rozszerzonego.
#' @return
#' lista, której każdy element opisuje wyniki dla innego typu szkół (spośród występujących
#' w danych) i składa się z elementów:
#' \itemize{
#'   \item{\code{usunieteKryteria} lista wektorów tekstowych z nazwami (pseudo)kryteriów,
#'    które zostały usunięte podczas skalowania wzorcowego.}
#'   \item{\code{parametry} lista data frame'ów z wyestymowanymi parametrami modeli w ich
#'         ostatecznej postaci (tj. takiej, jak w ostatnim kroku skalowania wzorcowego
#'         i w jedynym kroku skalowania na wszystkich zdających).}
#'   \item{\code{oszacowania} lista data frame'ów zawierających id_obserwacji i wyliczone
#'         oszacowania umiejętności dla wszystkich zdających. \bold{Wyniki zwracane są
#'         w formie wystandaryzowanej do (0, 1) na danych wzorocwych.}}
#'   \item{\code{mapowanie} data frame zawierający wzorzec przekodowania
#'         wyników surowych na wyniki wyskalowane.}
#'   \item{\code{odsUtraconejWariancji} odsetek wariancji przewidywania mierzonej cechy
#'         utracony w wyniku uśrednienia oszacowań w funkcji sumy punktów.}
#'   \item{\code{grupy} data frame zawierający mapowanie wartości zmiennych definiujących
#'         podział na grupy (\code{typ_szkoly} i \code{pr}) na wartości technicznej
#'         zmiennej grupującej (\code{gr_tmp1}), wykorzystywanej w estymacji przez funkcję
#'         \code{\link{skaluj}}.}
#' }
#' @seealso \code{\link{skaluj}}, \code{\link{procedura_1k_1w}},
#' \code{\link{przewidywanie_rasch}}, \code{\link{sprawdz_wyniki_skalowania}}
#' @importFrom EWDskalowanie skaluj
#' @export
skaluj_matura_rasch = function(daneWzorcowe, daneWszyscy, processors=2) {
  stopifnot(length(daneWzorcowe) == length(daneWszyscy))
  stopifnot(!is.null(names(daneWzorcowe)), !is.null(names(daneWszyscy)))
  stopifnot(all(names(daneWzorcowe) %in% c("matematyka podstawowa", "matematyka rozszerzona")),
            all(names(daneWszyscy ) %in% c("matematyka podstawowa", "matematyka rozszerzona")))
#   # wyciągnimy rok do nazw plików i podpisów
#   lata = lapply(daneWzorcowe, function(x) {return(unique(x$rok))})
#   if (length(unique(unlist(lata))) != 1) {
#     stop("Dane pochodzą z różnych lat.")
#   } else {
#     rok = lata[[1]]
#   }
#   sufiksy = list(
#     "matematyka podstawowa"  = "pp",
#     "matematyka rozszerzona" = "pr")
#   for (i in 1:length(daneWzorcowe)) {
#     # sprawdzanie, czy zgadzają sie zestawy (pseudo)kryteriów
#     zmienneKryteria = list(
#       wzorcowe = names(daneWzorcowe[[i]])[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe[[i]]))],
#       wszyscy  = names(daneWszyscy[[i]] )[grepl("^[kp]_[[:digit:]]+$", names(daneWszyscy[[i]] ))])
#     if (!all(zmienneKryteria$wzorcowe %in% zmienneKryteria$wszyscy) |
#           !all(zmienneKryteria$wszyscy %in% zmienneKryteria$wzorcowe)) {
#       stop("Niezgodność zestawu zmiennych do skalowania pomiędzy ", i, ". elementami argumentów 'daneWzorcowe' i 'daneWszyscy'")
#     }
#     # wyrzucamy wszystko, co niepotrzebne do skalowania (rypanie po dysku zajmuje potem cenny czas)
#     zmienneKryteria = zmienneKryteria[[1]]
#     daneWzorcowe[[i]] = daneWzorcowe[[i]][, c("id_obserwacji", "id_testu", "typ_szkoly", zmienneKryteria)]
#     daneWszyscy[[i]]  =  daneWszyscy[[i]][, c("id_obserwacji", "id_testu", "typ_szkoly", zmienneKryteria)]
#     # i dopisujemy do "id_testu" sufiks, żeby mieć szansę połączyć dane
#     names(daneWzorcowe[[i]]) = sub("^(id_testu)$", paste0("\\1_", sufiksy[[names(daneWzorcowe)[i]]]),
#                                    names(daneWzorcowe[[i]]))
#     names( daneWszyscy[[i]]) = sub("^(id_testu)$", paste0("\\1_", sufiksy[[names(daneWszyscy )[i]]]),
#                                    names( daneWszyscy[[i]]))
#     # oraz dodajemy kolumny pozwalające łatwo identyfikować, czy ktoś pisał daną część
#     daneWzorcowe[[i]] = cbind(daneWzorcowe[[i]], temp = TRUE)
#     names(daneWzorcowe[[i]]) = sub("temp", sufiksy[[names(daneWzorcowe)[i]]], names(daneWzorcowe[[i]]))
#      daneWszyscy[[i]] = cbind( daneWszyscy[[i]], temp = TRUE)
#     names( daneWszyscy[[i]]) = sub("temp", sufiksy[[names( daneWszyscy)[i]]], names( daneWszyscy[[i]]))
#   }
#   # łączenie
#   poziomy = lapply(daneWzorcowe, function(x) {return(names(x)[grep("^[kp]_", names(x))])})
#   names(poziomy) = sufiksy
#   daneWzorcowe = list(mat_rsch = suppressMessages(join_all(daneWzorcowe, type="full")))
#   if (any(duplicated(daneWzorcowe$id_obserwacji))) stop("Zduplikowane wartości 'id_obserwacji' w 'daneWzorcowe' (prawdopodobnie w konsekwencji sprzecznych informacji o typie szkoły pomiędzy częściami egzaminu).")
#   daneWszyscy  = list(mat_rsch = suppressMessages(join_all(daneWszyscy , type="full")))
#   if (any(duplicated( daneWszyscy$id_obserwacji))) stop("Zduplikowane wartości 'id_obserwacji' w 'daneWszyscy' (prawdopodobnie w konsekwencji sprzecznych informacji o typie szkoły pomiędzy częściami egzaminu).")
#   # skalowanie jako takie
#   wyniki = setNames(vector(mode="list", length=length(daneWzorcowe)), names(daneWzorcowe))
#   for (i in 1:length(daneWzorcowe)) {
#     # jeszcze trochę porządku po łączeniu
#     daneWzorcowe[[i]][, unlist(sufiksy)] = lapply(daneWzorcowe[[i]][, unlist(sufiksy)],
#                                                  function(x) {return(!is.na(x))})
#      daneWszyscy[[i]][, unlist(sufiksy)] = lapply( daneWszyscy[[i]][, unlist(sufiksy)],
#                                             function(x) {return(!is.na(x))})
#     daneWzorcowe[[i]] = daneWzorcowe[[i]][daneWzorcowe[[i]]$pp, ]
#      daneWszyscy[[i]] =  daneWszyscy[[i]][ daneWszyscy[[i]]$pp, ]
#     # samo skalowanie
#     tytulWzorcowe = paste0(names(daneWzorcowe)[i], rok, " wzor")
#     zmienneKryteria = names(daneWzorcowe[[i]])[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe[[i]]))]
#
#     message("### Skalowanie wzorcowe ", names(daneWzorcowe)[i], " ###\n")
#     zmGrupujace = c("typ_szkoly", "pr")
#     opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(daneWzorcowe)[i],
#                                    wieleGrup = zmGrupujace,
#                                    rasch = TRUE, processors = processors)
#     matWzorcowe   = skaluj(daneWzorcowe[[i]], opisWzorcowe, "id_obserwacji", tytul = tytulWzorcowe,
#                           zmienneDolaczaneDoOszacowan = names(daneWzorcowe[[i]])[grepl("^id_testu", names(daneWzorcowe[[i]]))])
#     # wyliczanie schematu przekodowania suma - oszacowania
#     sumy = lapply(poziomy,
#                   function(x, dane) {
#                     maska = apply(!is.na(dane[, x]), 1, any)
#                     return(cbind(dane[maska, c("id_obserwacji", "typ_szkoly")],
#                                  suma = rowSums(dane[maska, x], na.rm=TRUE)))
#                   },
#                   dane = daneWzorcowe[[i]])
#     for(j in 1:length(sumy)) {
#       names(sumy[[j]]) = sub("^suma$", paste0("suma_", names(sumy)[j]), names(sumy[[j]]))
#     }
#     sumy = suppressMessages(join_all(sumy))
#     oszacowania = sort(unique(daneWzorcowe[[i]]$typ_szkoly))
#     wyniki[[i]] = oszacowania = setNames(as.list(oszacowania), oszacowania)
#     for (j in 1:length(oszacowania)) {
#       temp = matWzorcowe[[1]][[length(matWzorcowe[[1]])]]$zapis[, c("id_obserwacji", "typ_szkoly",
#                                                                     names(daneWzorcowe)[i])]
#       oszacowania[[j]] = przewidywanie_rasch(sumy[sumy$typ_szkoly == oszacowania[[j]], names(sumy) != "typ_szkoly"],
#                                              temp[temp$typ_szkoly == oszacowania[[j]], names(temp) != "typ_szkoly"],
#                                              max=c(suma_pp=50, suma_pr=50))
#       # i wystandaryzujmy
#       sr = mean(oszacowania[[j]]$przewidywania[, names(daneWzorcowe)[i]])
#       os =   sd(oszacowania[[j]]$przewidywania[, names(daneWzorcowe)[i]])
#       oszacowania[[j]]$przewidywania[, names(daneWzorcowe)[i]] =
#         (oszacowania[[j]]$przewidywania[, names(daneWzorcowe)[i]] - sr) / os
#       oszacowania[[j]]$mapowanie[, names(daneWzorcowe)[i]] =
#         (oszacowania[[j]]$mapowanie[, names(daneWzorcowe)[i]] - sr) / os
#     }
#     # rzeczy do zwrócenia "globalne"
#     wartosciZakotwiczone = matWzorcowe[[1]][[length(matWzorcowe[[1]])]]$parametry$surowe
#     wartosciZakotwiczone = wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
#     zmienneKryteriaPoUsuwaniu = wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
#     usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)]
#     grupy = unique(matWzorcowe[[1]][[length(matWzorcowe[[1]])]]$zapis[, c("gr_tmp1", zmGrupujace)])
#     grupy = grupy[order(grupy$gr_tmp1), ]
#     message("### Wyliczanie oszacowań dla wszystkich zdających ", names(daneWzorcowe)[i], " ###\n")
#     sumy = lapply(poziomy,
#                   function(x, dane) {
#                     maska = apply(!is.na(dane[, x]), 1, any)
#                     return(cbind(dane[maska, c("id_obserwacji", "typ_szkoly")],
#                                  suma = rowSums(dane[maska, x], na.rm=TRUE)))
#                   },
#                   dane = daneWszyscy[[i]])
#     for(j in 1:length(sumy)) {
#       names(sumy[[j]]) = sub("^suma$", paste0("suma_", names(sumy)[j]), names(sumy[[j]]))
#     }
#     sumy = suppressMessages(join_all(sumy))
#     sumy = cbind(sumy, suma = rowSums(sumy[, grep("^suma_", names(sumy))], na.rm=TRUE))
#     sumy[, grep("^suma_", names(sumy))] = lapply(sumy[, grep("^suma_", names(sumy))],
#                                                  function(x) {return(!is.na(x))})
#     for (j in 1:length(wyniki[[i]])) {
#       temp = suppressMessages(join(sumy[sumy$typ_szkoly %in% names(wyniki[[i]])[j], ],
#                                    oszacowania[[names(wyniki[[i]])[j]]]$mapowanie))
#       wyniki[[i]][[j]] = list(
#         usunieteKryteria = usunieteKryteria,
#         parametry = wartosciZakotwiczone,
#         oszacowania = temp[, c("id_obserwacji", names(daneWzorcowe)[i])],
#         mapowanie = oszacowania[[j]]$mapowanie,
#         grupy = grupy
#       )
#     }
#   }
#   return(wyniki)
}
