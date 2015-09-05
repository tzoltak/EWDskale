#' @title Procedury skalowania egzaminow.
#' @description
#' Funkcja przeprowadza skalowanie części mat.-przyr. egzaminu gimnazjalnego z użyciem
#' modelu Rascha (na potrzeby maturalnego Kalkulatora EWD).
#' @param daneWzorcowe lista data frame'ów zawierających dane do skalowania wzorcowego
#' @param daneWszyscy lista data frame'ów zawierających zawierający dane wszystkich
#' zdających (do wyliczenia oszacowań umiejętności na podstawe parametrów ze skalowania
#' wzorcowego)
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @details
#' Argumenty (poza ostatnim) muszą być listami (mogą być jednoelementowe), których
#' elementy mają nazwy ze zbioru: \code{"gm", "gm_p", "gm_m"} i są data frame'ami
#' zawierającymi dane z wynikami odpowiednich części egzaminu gimnazjalnego.
#'
#' W przypadku nowej struktury egzaminu gimnazjalnego wystarczy podać wyniki testów,
#' dane do wyskalowania części jako całości zostaną połączone automatycznie, wewnątrz
#' funkcji.
#'
#' Schemat przekodowania sum punktów na oszacowania umiejętności wyliczany jest na
#' podstawie danych wzorcowych, przy pomocy funkcji \code{\link{przewidywanie_rasch}},
#' a następnie na jego podstawie przypisywane są wartości przewidywane wszystkim zdającym.
#' @return
#' lista z elementami:
#' \itemize{
#'   \item{\code{usunieteKryteria} lista wektorów tekstowych z nazwami (pseudo)kryteriów,
#'    które zostały usunięte podczas skalowania wzorcowego.}
#'   \item{\code{parametry} lista data frame'ów z wyestymowanymi parametrami modeli w ich
#'         ostatecznej postaci (tj. takiej, jak w ostatnim kroku skalowania wzorcowego
#'         i w jedynym kroku skalowania na wszystkich zdających).}
#'   \item{\code{oszacowania} lista data frame'ów zawierających id_obserwacji i wyliczone
#'         oszacowania umiejętności dla wszystkich zdających.}
#'   \item{\code{rzetelnoscEmpiryczna} rzetelność wyliczona na podstawie oszacowań ze
#'         skalowania wzorcowego (jako wariancja oszacowań EAP).}
#'   \item{\code{mapowanie} data frame zawierający wzorzec przekodowania
#'         wyników surowych na wyniki wyskalowane.}
#'   \item{\code{odsUtraconejWariancji} odsetek wariancji przewidywania mierzonej cechy
#'         utracony w wyniku uśrednienia oszacowań w funkcji sumy punktów.}
#' }
#' @seealso \code{\link{skaluj}}, \code{\link{procedura_1k_1w}}, \code{\link{przewidywanie_rasch}}
#' @importFrom EWDskalowanie skaluj
#' @export
skaluj_egz_gimn_rasch = function(daneWzorcowe, daneWszyscy, processors=2) {
  stopifnot(length(daneWzorcowe) == length(daneWszyscy))
  stopifnot(!is.null(names(daneWzorcowe)), !is.null(names(daneWszyscy)))
  stopifnot(all(names(daneWzorcowe) %in% c("gm_r", "gm_pr", "gm_mr")),
            all(names(daneWszyscy ) %in% c("gm_r", "gm_pr", "gm_mr")))
#   # wyciągnimy rok do nazw plików i podpisów
#   lata = lapply(daneWzorcowe, function(x) {return(unique(x$rok))})
#   if (length(unique(unlist(lata))) != 1) {
#     stop("Dane pochodzą z różnych lat.")
#   } else {
#     rok = lata[[1]]
#   }
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
#     daneWzorcowe[[i]] = daneWzorcowe[[i]][, c("id_obserwacji", "id_testu", zmienneKryteria)]
#     daneWszyscy[[i]]  =  daneWszyscy[[i]][, c("id_obserwacji", "id_testu", zmienneKryteria)]
#     # i dopisujemy do "id_testu" sufiks, żeby mieć szansę połączyć dane z nowej formuły
#     names(daneWzorcowe[[i]]) = sub("^(id_testu)$", paste0("\\1_", names(daneWzorcowe)[i]),
#                                    names(daneWzorcowe[[i]]))
#     names( daneWszyscy[[i]]) = sub("^(id_testu)$", paste0("\\1_", names(daneWszyscy )[i]),
#                                    names( daneWszyscy[[i]]))
#   }
#
#   # ew. dopisywanie części zbierających po dwa testy z nowej formuły
#   if (all(c("gm_pr", "gm_mr") %in% names(daneWzorcowe)) & !("gm_r" %in% names(daneWzorcowe))) {
#     daneWzorcowe$gm = merge(daneWzorcowe$gm_pr, daneWzorcowe$gm_mr)
#      daneWszyscy$gm = merge( daneWszyscy$gm_pr,  daneWszyscy$gm_mr)
#     daneWzorcowe = daneWzorcowe[!(names(daneWzorcowe %in% c("gm_pr", "gm_mr")))]
#   }
#   # skalowanie jako takie
#   wyniki = setNames(vector(mode="list", length=length(daneWzorcowe)), names(daneWzorcowe))
#   for (i in 1:length(daneWzorcowe)) {
#     tytulWzorcowe = paste0(names(daneWzorcowe)[i], rok, " wzor")
#     zmienneKryteria = names(daneWzorcowe[[i]])[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe[[i]]))]
#
#     message("### Skalowanie wzorcowe ", names(daneWzorcowe)[i], " ###\n")
#     opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(daneWzorcowe)[i],
#                                    rasch = TRUE, processors = processors)
#     egWzorcowe   = skaluj(daneWzorcowe[[i]], opisWzorcowe, "id_obserwacji", tytul = tytulWzorcowe,
#                           zmienneDolaczaneDoOszacowan = names(daneWzorcowe[[i]])[grepl("^id_testu", names(daneWzorcowe[[i]]))])
#     # wyliczanie schematu przekodowania suma - oszacowania
#     suma = daneWzorcowe[[i]][, grep("^[kp]_", names(daneWzorcowe[[i]]))]
#     suma = cbind(daneWzorcowe[[i]][, "id_obserwacji", drop=FALSE],
#                  sumaGm = rowSums(suma, na.rm=TRUE))
#     oszacowania = egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$zapis[, c("id_obserwacji",
#                                                                        names(daneWzorcowe)[i])]
#     oszacowania = przewidywanie_rasch(suma, oszacowania, max=c(sumaGm=50))
#     # wyliczanie rzetelności empirycznej
#     rzetelnoscEmpiryczna = var(oszacowania$przewidywania[, names(daneWzorcowe)[i]])
#
#     message("### Wyliczanie oszacowań dla wszystkich zdających ", names(daneWzorcowe)[i], " ###\n")
#     suma = daneWszyscy[[i]][, grep("^[kp]_", names(daneWszyscy[[i]]))]
#     suma = cbind(daneWszyscy[[i]][, "id_obserwacji", drop=FALSE],
#                  suma = rowSums(suma, na.rm=TRUE))
#     suma = cbind(suma, sumaGm = !is.na(suma$suma))
#     suma = suppressMessages(join(suma, oszacowania$mapowanie))
#     suma = suma[, c("id_obserwacji", names(daneWzorcowe)[i])]
#     # rzeczy do zwrócenia
#     wartosciZakotwiczone = egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$parametry$surowe
#     wartosciZakotwiczone = wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
#     zmienneKryteriaPoUsuwaniu = wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
#     usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)]
#
#     wyniki[[i]] = list(
#       usunieteKryteria = usunieteKryteria,
#       parametry = wartosciZakotwiczone,
#       oszacowania = suma,
#       rzetelnoscEmpiryczna = rzetelnoscEmpiryczna,
#       mapowanie = oszacowania$mapowanie,
#       odsUtraconejWariancji = oszacowania$odsUtraconejWariancji
#     )
#   }
#   return(wyniki)
}
