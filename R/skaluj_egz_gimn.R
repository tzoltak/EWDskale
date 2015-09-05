#' @title Procedury skalowania egzaminow.
#' @description
#' Funkcja przeprowadza skalowanie wyników egzaminu gimnazjalnego.
#'
#' Argumenty (poza ostatnim) muszą być listami, których elementy mają nazwy ze zbioru:
#' \code{"gh", "gh_h", "gh_p", "gm", "gm_p", "gm_m"} i są data frame'ami zawierającymi
#' dane z wynikami odpowiednich części egzaminu gimnazjalnego.
#'
#' W przypadku nowej struktury egzaminu gimnazjalnego wystarczy podać wyniki testów,
#' dane do wyskalowania części jako całości zostaną połączone automatycznie, wewnątrz
#' funkcji.
#' @param daneWzorcowe lista data frame'ów zawierających dane do skalowania wzorcowego
#' @param daneWszyscy lista data frame'ów zawierających zawierający dane wszystkich
#' zdających (do wyliczenia oszacowań umiejętności na podstawe parametrów ze skalowania
#' wzorcowego)
#' @param processors liczba rdzeni do wykorzystania przy estymacji
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
#' }
#' @seealso \code{\link{skaluj}}, \code{\link{procedura_1k_1w}},
#' \code{\link{sprawdz_wyniki_skalowania}}
#' @importFrom EWDskalowanie skaluj
#' @export
skaluj_egz_gimn = function(daneWzorcowe, daneWszyscy, processors=2) {
  stopifnot(length(daneWzorcowe) == length(daneWszyscy))
  stopifnot(!is.null(names(daneWzorcowe)), !is.null(names(daneWszyscy)))
  stopifnot(all(names(daneWzorcowe) %in% c("gh", "gh_h", "gh_p", "gm", "gm_p", "gm_m")),
            all(names(daneWszyscy ) %in% c("gh", "gh_h", "gh_p", "gm", "gm_p", "gm_m")))
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
#   if (all(c("gh_h", "gh_p") %in% names(daneWzorcowe)) & !("gh" %in% names(daneWzorcowe))) {
#     daneWzorcowe$gh = merge(daneWzorcowe$gh_h, daneWzorcowe$gh_p)
#      daneWszyscy$gh = merge( daneWszyscy$gh_h,  daneWszyscy$gh_p)
#   }
#   if (all(c("gm_p", "gm_m") %in% names(daneWzorcowe)) & !("gm" %in% names(daneWzorcowe))) {
#     daneWzorcowe$gm = merge(daneWzorcowe$gm_p, daneWzorcowe$gm_m)
#      daneWszyscy$gm = merge( daneWszyscy$gm_p,  daneWszyscy$gm_m)
#   }
#   # przemieszczanie gh i gm na koniec list z danymi, żeby można było do nich wprowadzić poprawki (wyrzucić (pseudo)kryteria) na podstawie skalowania testów składowych
#   kolejnosc = c(grep("^g[hm]_", names(daneWzorcowe)), grep("^g[hm]$", names(daneWzorcowe)))
#   daneWzorcowe = daneWzorcowe[kolejnosc]
#    daneWszyscy =  daneWszyscy[kolejnosc]
#   # skalowanie jako takie
#   wyniki = setNames(vector(mode="list", length=length(daneWzorcowe)), names(daneWzorcowe))
#   for (i in 1:length(daneWzorcowe)) {
#     tytulWzorcowe = paste0(names(daneWzorcowe)[i], rok, " wzor")
#     tytulWszyscy  = paste0(names(daneWzorcowe)[i], rok, " wszyscy")
#     zmienneKryteria = names(daneWzorcowe[[i]])[grepl("^[kp]_[[:digit:]]+$", names(daneWzorcowe[[i]]))]
#
#     # sztuczka, żeby przy skalowaniu gh i gm w nowej formule już nie usuwał (pseudo)kryteriów
#     if ( ((names(daneWzorcowe)[i] == "gh") & all(c("gh_h", "gh_p") %in% names(daneWzorcowe))) |
#            ((names(daneWzorcowe)[i] == "gm") & all(c("gm_p", "gm_m") %in% names(daneWzorcowe))) ) {
#       # dajemy tu data frame, żeby nie było usuwania kryteriów, ale wtedy trzeba zadać w nim wartość oczekiwaną i wariancję
#       wartosciZakotwiczone = data.frame(typ = c("mean", "variance"),
#                                         zmienna1 = names(daneWzorcowe)[i], zmienna2 = "",
#                                         wartosc=c(0, 1), stringsAsFactors=FALSE)
#     } else {
#       wartosciZakotwiczone = NULL
#     }
#     message("### Skalowanie wzorcowe ", names(daneWzorcowe)[i], " ###\n")
#     opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(daneWzorcowe)[i],
#                                    wartosciZakotwiczone, processors=processors)
#     egWzorcowe   = skaluj(daneWzorcowe[[i]], opisWzorcowe, "id_obserwacji", tytul = tytulWzorcowe,
#                           zmienneDolaczaneDoOszacowan = names(daneWzorcowe[[i]])[grepl("^id_testu", names(daneWzorcowe[[i]]))])
#     # wyliczanie rzetelności empirycznej
#     rzetelnoscEmpiryczna = egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$zapis[[names(daneWzorcowe)[i]]]
#     rzetelnoscEmpiryczna = var(rzetelnoscEmpiryczna)
#
#     message("### Wyliczanie oszacowań dla wszystkich zdających ", names(daneWzorcowe)[i], " ###\n")
#     wartosciZakotwiczone = egWzorcowe[[1]][[length(egWzorcowe[[1]])]]$parametry$surowe
#     wartosciZakotwiczone = wartosciZakotwiczone[!(wartosciZakotwiczone$typ %in% c("mean", "variance")), ]
#     zmienneKryteriaPoUsuwaniu = wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
#     if ( ((names(daneWzorcowe)[i] == "gh") & all(c("gh_h", "gh_p") %in% names(daneWzorcowe))) ) {
#       usunieteKryteria = c(wyniki$gh_h$usunieteKryteria, wyniki$gh_p$usunieteKryteria)
#     } else if ( (names(daneWzorcowe)[i] == "gm") & all(c("gm_p", "gm_m") %in% names(daneWzorcowe))) {
#       usunieteKryteria = c(wyniki$gm_p$usunieteKryteria, wyniki$gm_m$usunieteKryteria)
#     } else {
#       usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)]
#     }
#
#     opisWszyscy  = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, names(daneWzorcowe)[i],
#                                    wartosciZakotwiczone, processors=processors)
#     egWszyscy    = skaluj(daneWszyscy[[i]] , opisWszyscy , "id_obserwacji", tytul = tytulWszyscy,
#                           zmienneDolaczaneDoOszacowan = names(daneWszyscy[[i]])[grepl("^id_testu", names(daneWszyscy[[i]]))])
#
#     wyniki[[i]] = list(
#       usunieteKryteria = usunieteKryteria,
#       parametry = wartosciZakotwiczone,
#       oszacowania = egWszyscy[[1]][[length(egWszyscy[[1]])]]$zapis,
#       rzetelnoscEmpiryczna = rzetelnoscEmpiryczna
#     )
#     # ew. wyrzucanie (pseudo)kryteriów z gh i gm na podstawie tego, co wyszło w poszczególnych testach
#     if ( (names(daneWzorcowe)[i] %in% c("gh_h", "gh_p")) & ("gh" %in% names(daneWzorcowe)) ) {
#       daneWzorcowe$gh = daneWzorcowe$gh[, !(names(daneWzorcowe$gh) %in% wyniki[[i]]$usunieteKryteria)]
#       daneWzorcowe$gh = daneWzorcowe$gh[, !(names(daneWzorcowe$gh) %in% wyniki[[i]]$usunieteKryteria)]
#     }
#     if ( (names(daneWzorcowe)[i] %in% c("gm_p", "gm_m")) & ("gm" %in% names(daneWzorcowe)) ) {
#       daneWzorcowe$gm = daneWzorcowe$gm[, !(names(daneWzorcowe$gm) %in% wyniki[[i]]$usunieteKryteria)]
#       daneWzorcowe$gm = daneWzorcowe$gm[, !(names(daneWzorcowe$gm) %in% wyniki[[i]]$usunieteKryteria)]
#     }
#   }
#   return(wyniki)
}
