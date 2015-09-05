#' @title Procedury skalowania egzaminow.
#' @description
#' Funkcja przeprowadza skalowanie matury na potrzeby trzyletnich wskaźników maturalnych.
#' Domyślnie wyliczne są cztery wskaźniki: 1) humanistyczny (j. polski, historia, WOS),
#' 2) matematyczno-przyrodniczy (matematyka, biologia, chemia, fizyka, geografia,
#' informatyka), 3) polonistyczny (j. polski), 4) matematyczny (matematyka).
#' @param daneWzorcowe lista data frame'ów zawierających dane do skalowania wzorcowego
#' @param daneWszyscy lista data frame'ów zawierających zawierający dane wszystkich
#' zdających (do wyliczenia oszacowań umiejętności na podstawe parametrów ze skalowania
#' wzorcowego)
#' @param czesciEgzaminu opcjonalnie lista wektorów tekstowych zawierających nazwy
#' elementów argumnetów \code{daneWzorcowe} i \code{daneWszyscy}, które mają być
#' uwzględnione przy wyliczaniu poszczególnych wskaźników wyników matury (każdy element
#' listy opisuje jeden wskaźnik); jeśli \code{NULL}, wyliczone zostaną cztery domyślne
#' wskaźniki
#' @param sufiksy opcjonalnie lista ciągów znaków definiująca sufiksy odpowiadające
#' poszczególnym częściom egzaminu (elementom argumentów \code{daneWzorcowe}
#' i \code{daneWszyscy}); nazwa elementu listy definiuje część egzaminu, wartość
#' elementu to sufiks
#' @param processors liczba rdzeni do wykorzystania przy estymacji
#' @details
#' Argumenty (poza ostatnim) muszą być listami (mogą być jednoelementowe), których
#' elementy są data frame'ami zawierającymi dane z wynikami odpowiednich części matury.
#'
#' \bold{Model skalowania:} jednowymiarowe, wielogrupowe modele 2PL/SGR, z grupami
#' zdefiniowanymi przez: 1) w przypadku modeli dla j. polskiego i matematyki: typ szkoły
#' (LO/T) i pisanie lub nie poziomu rozszerzonego lub 2) w przypadku modeli obejmujących
#' wiele przedmiotów: typ szkoły (LO/T).
#'
#' \bold{Uwaga:} dopisywanie parametrów selekcji związanych z wyborem tematu wypracowania
#' (polski PP i PR, historia PR, WOS PR) jest naklepane ostrym brutem i jest raczej
#' nieodporne na zmiany w formule matury w zakresie możliwości wyboru tematu wypracowań.
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
#'   \item{\code{grupy} data frame zawierający mapowanie wartości zmiennych definiujących
#'         podział na grupy na wartości technicznej zmiennej grupującej (\code{gr_tmp1}),
#'         wykorzystywanej w estymacji przez funkcję \code{\link{skaluj}}.}
#' }
#' @seealso \code{\link{skaluj}}, \code{\link{procedura_1k_1w}},
#' \code{\link{sprawdz_wyniki_skalowania}}
#' @importFrom EWDskalowanie skaluj
#' @export
skaluj_matura = function(daneWzorcowe, daneWszyscy, czesciEgzaminu=NULL, sufiksy=NULL, processors=2) {
  stopifnot(length(daneWzorcowe) == length(daneWszyscy),
            is.list(czesciEgzaminu) | is.null(czesciEgzaminu),
            is.list(sufiksy       ) | is.null(sufiksy       ))
#   if (!is.null(czesciEgzaminu)) stopifnot(all(unlist(lapply(czesciEgzaminu, is.character))))
#   if (is.null(czesciEgzaminu)) {
#     czesciEgzaminu = list(
#       mp = paste(rep("j. polski", 2), c("podstawowa", "rozszerzona")),
#       mm = paste(rep("matematyka", 2), c("podstawowa", "rozszerzona")),
#       mh = paste(rep(c("j. polski", "historia", "WOS"), each=2),
#                  c("podstawowa", "rozszerzona")),
#       mmp = paste(rep(c("matematyka", "biologia", "chemia", "fizyka", "geografia",
#                         "informatyka"), each=2),
#                   c("podstawowa", "rozszerzona"))
#     )
#   }
#   stopifnot(!is.null(names(daneWzorcowe)), !is.null(names(daneWszyscy)))
#   stopifnot(all(names(daneWzorcowe) %in% unlist(czesciEgzaminu)),
#             all(names(daneWszyscy ) %in% unlist(czesciEgzaminu)))
#   sufiksyDomyslne = list(
#     "j. polski podstawowa"    = "jpo_p",
#     "j. polski rozszerzona"   = "jpo_r",
#     "historia podstawowa"     = "his_p",
#     "historia rozszerzona"    = "his_r",
#     "WOS podstawowa"          = "wos_p",
#     "WOS rozszerzona"         = "wos_r",
#     "matematyka podstawowa"   = "mat_p",
#     "matematyka rozszerzona"  = "mat_r",
#     "biologia podstawowa"     = "bio_p",
#     "biologia rozszerzona"    = "bio_r",
#     "chemia podstawowa"       = "che_p",
#     "chemia rozszerzona"      = "che_r",
#     "fizyka podstawowa"       = "fiz_p",
#     "fizyka rozszerzona"      = "fiz_r",
#     "geografia podstawowa"    = "geo_p",
#     "geografia rozszerzona"   = "geo_r",
#     "informatyka podstawowa"  = "inf_p",
#     "informatyka rozszerzona" = "inf_r")
#   if (!is.null(sufiksy)) {
#     stopifnot(all(unlist(lapply(sufiksy, is.character))),
#               all(unlist(lapply(sufiksy, length)) == 1),
#               !is.null(names(sufiksy)))
#     sufiksy = unlist(list(sufiksy, sufiksyDomyslne), recursive=FALSE)
#     sufiksy = sufiksy[!duplicated(names(sufiksy))]
#   } else {
#     sufiksy = sufiksyDomyslne
#   }
#   zduplSufiksy = duplicated(unlist((sufiksy)))
#   if (any(zduplSufiksy)) stop("W argumencie 'sufiksy' podano zduplikowane wartości:\n  ",
#                               paste0(sufiksy[zduplSufiksy], collapse=",\n  "))
#   stopifnot(all(names(daneWzorcowe) %in% names(sufiksy)),
#             all(names(daneWszyscy ) %in% names(sufiksy)))
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
#     daneWzorcowe[[i]] = daneWzorcowe[[i]][, c("id_obserwacji", "id_testu", "typ_szkoly", zmienneKryteria)]
#     daneWszyscy[[i]]  =  daneWszyscy[[i]][, c("id_obserwacji", "id_testu", "typ_szkoly", zmienneKryteria)]
#     # i dopisujemy do "id_testu" sufiks, żeby mieć szansę połączyć dane
#     names(daneWzorcowe[[i]]) = sub("^(id_testu)$", paste0("\\1_", sufiksy[[names(daneWzorcowe)[i]]]),
#                                    names(daneWzorcowe[[i]]))
#     names( daneWszyscy[[i]]) = sub("^(id_testu)$", paste0("\\1_", sufiksy[[names(daneWszyscy )[i]]]),
#                                    names( daneWszyscy[[i]]))
#     # oraz dodajemy kolumny do parametrów selekcji
#     if (!(names(daneWzorcowe)[i] %in% c("j. polski podstawowa", "matematyka podstawowa"))) {
#       daneWzorcowe[[i]] = cbind(daneWzorcowe[[i]], temp = 1)
#       names(daneWzorcowe[[i]]) = sub("temp", paste0("s_", sufiksy[[names(daneWzorcowe)[i]]]), names(daneWzorcowe[[i]]))
#        daneWszyscy[[i]] = cbind( daneWszyscy[[i]], temp = 1)
#       names( daneWszyscy[[i]]) = sub("temp", paste0("s_", sufiksy[[names(daneWzorcowe)[i]]]), names( daneWszyscy[[i]]))
#     }
#     # i parametry selekcji na tematy z polskiego (PP i PR), historii (PR) i WOSu (PR)
#     if (names(daneWzorcowe)[i] %in% c("j. polski podstawowa", "j. polski rozszerzona", "historia rozszerzona", "WOS rozszerzona")) {
#       # brudny brut - ładniej byłoby się połączyć z bazą i sprawdzić
#       # liczymy odsetki braków danych
#       odsBD = unlist(lapply(daneWzorcowe[[i]], function(x) {return(sum(is.na(x)) / length(x))}))
#       odsBD = odsBD[odsBD > 0.03]  # zostawiamy tylko te względnie duże
#       d = 6
#       while(length(unique(odsBD)) > 3) {  # zakładając, że tematy co do zasady są 3 (I, II i "nie wiemy, który")
#         odsBD = round(odsBD, d)  # brutalnie zaokrąglamy na okoliczność, gdyby w jakichś zmiennych były pojedyncze braki danych
#         d = d - 1
#       }
#       if (length(unique(odsBD)) < 2) {
#         warning("Nie udało się utworzyć parametrów selekcji dla wypracowania w części egzaminu: '",
#                 names(daneWzorcowe)[i], "'.", immediate.=TRUE)
#       } else {
#         nrTem = 2  # potrzebujemy o jeden mniej zmiennych niż tematów
#         for (j in sort(unique(odsBD))[-1]) {  # ustawiamy je w kolejności wg. czestości wybierania ("wybierania")  i za punkt odniesienia bierzemy najczęściej występujący
#           daneWzorcowe[[i]] = cbind(daneWzorcowe[[i]], temp = 0)
#           daneWzorcowe[[i]]$temp[apply(!is.na(daneWzorcowe[[i]][, names(odsBD)[odsBD == j], drop=FALSE]), 1, any)] = 1
#           names(daneWzorcowe[[i]]) = sub("temp", paste0("t", nrTem, "_", sufiksy[[ names(daneWzorcowe)[i] ]]), names(daneWzorcowe[[i]]))
#           daneWszyscy[[i]] = cbind(daneWszyscy[[i]], temp = 0)
#           daneWszyscy[[i]]$temp[apply(!is.na( daneWszyscy[[i]][, names(odsBD)[odsBD == j], drop=FALSE]), 1, any)] = 1
#           names(daneWszyscy[[i]] ) = sub("temp", paste0("t", nrTem, "_", sufiksy[[ names(daneWszyscy )[i] ]]), names( daneWszyscy[[i]]))
#           nrTem = nrTem + 1
#         }
#         message("W części egzaminu '", names(daneWszyscy )[i], "' utworzono ", nrTem - 2,
#                 " zmienne opisujące wybór tematu wypracowania.\n",
#                 "Rozkład wybieralności tematów w danych wzorcowych:")
#         sr = unlist(lapply(daneWzorcowe[[i]][, grep("^t[[:digit:]]+_", names(daneWzorcowe[[i]])), drop=FALSE], mean))
#         sr = paste0(format(100 * c(1 - sum(sr), sr), nsmall=1, digits=1), "%")
#         print(data.frame(temat = letters[1:(nrTem - 1)], "częstość" = sr, stringsAsFactors=FALSE, check.names=FALSE),
#               row.names=FALSE, quote=FALSE)
#       }
#     }
#   }
#   # łączenie
#   message("\nŁączenie części egzaminu:")
#   daneWzorcoweTemp = daneWszyscyTemp =
#     setNames(vector(mode="list", length=length(czesciEgzaminu)), names(czesciEgzaminu))
#   # pierwsze tempo - daneWzorcowe
#   for (i in 1:length(czesciEgzaminu)) {
#     message("  ", names(czesciEgzaminu)[i])
#     daneWzorcoweTemp[[i]] = suppressMessages(join_all(daneWzorcowe[czesciEgzaminu[[i]]], type="full"))
#     duplikaty = daneWzorcoweTemp[[i]]$id_obserwacji[duplicated(daneWzorcoweTemp[[i]]$id_obserwacji)]
#     if (length(duplikaty) > 0) {
#       warning("Wykryto i usunięto ", length(duplikaty), " zduplikowaną/e/ych wartości 'id_obserwacji' w 'daneWzorcowe' (prawdopodobnie w konsekwencji sprzecznych informacji o typie szkoły pomiędzy częściami egzaminu).", immediate.=TRUE)
#       daneWzorcoweTemp[[i]] = daneWzorcoweTemp[[i]][!(daneWzorcoweTemp[[i]]$id_obserwacji %in% duplikaty), ]
#     }
#     daneWszyscyTemp[[i]]  = suppressMessages(join_all( daneWszyscy[czesciEgzaminu[[i]]], type="full"))
#     duplikaty = daneWszyscyTemp[[i]]$id_obserwacji[duplicated(daneWszyscyTemp[[i]]$id_obserwacji)]
#     if (length(duplikaty) > 0) {
#       warning("Wykryto i usunięto ", length(duplikaty), " zduplikowaną/e/ych wartości 'id_obserwacji' w 'daneWszyscy' (prawdopodobnie w konsekwencji sprzecznych informacji o typie szkoły pomiędzy częściami egzaminu).", immediate.=TRUE)
#       daneWszyscyTemp[[i]] = daneWszyscyTemp[[i]][!(daneWszyscyTemp[[i]]$id_obserwacji %in% duplikaty), ]
#     }
#     # dopełnianie 0 parametrów selekcji
#     daneWzorcoweTemp[[i]][, grep("^s_", names(daneWzorcoweTemp[[i]]))] =
#       lapply(daneWzorcoweTemp[[i]][, grep("^s_", names(daneWzorcoweTemp[[i]])), drop=FALSE], function(x) {x[is.na(x)] = 0; return(x)})
#      daneWszyscyTemp[[i]][, grep("^s_", names(daneWszyscyTemp[[i]] ))] =
#       lapply( daneWszyscyTemp[[i]][, grep("^s_", names( daneWszyscyTemp[[i]])), drop=FALSE], function(x) {x[is.na(x)] = 0; return(x)})
#   }
#   daneWzorcowe = daneWzorcoweTemp
#   daneWszyscy  = daneWszyscyTemp
#   rm(list=c("daneWzorcoweTemp", "daneWszyscyTemp"))
#   # skalowanie jako takie
#   wyniki = setNames(vector(mode="list", length=length(daneWzorcowe)), names(daneWzorcowe))
#   for (i in 1:length(daneWzorcowe)) {
#     tytulWzorcowe = paste0(names(daneWzorcowe)[i], rok, " wzor")
#     tytulWszyscy  = paste0(names(daneWzorcowe)[i], rok, " wszyscy")
#     zmienneKryteria = names(daneWzorcowe[[i]])[grepl("^([kp]_[[:digit:]]+|(s|t[[:digit:]]+)_[[:alpha:]_]+)$", names(daneWzorcowe[[i]]))]
#     if (length(czesciEgzaminu[[i]]) > 2) {
#       zmGrupujace = "typ_szkoly"
#       nigdyNieUsuwaj="^(s|t[[:digit:]]+)_"
#     } else {
#       maskaSelekcja = grepl("^s_", zmienneKryteria)
#       zmGrupujace = c("typ_szkoly", zmienneKryteria[maskaSelekcja])
#       zmienneKryteria = zmienneKryteria[!maskaSelekcja]
#       nigdyNieUsuwaj="^t[[:digit:]]+_"
#     }
#
#     message("### Skalowanie wzorcowe ", names(daneWzorcowe)[i], " ###\n")
#     opisWzorcowe = procedura_1k_1w(zmienneKryteria, names(daneWzorcowe)[i],
#                                    wieleGrup = zmGrupujace, nigdyNieUsuwaj = "^(s|t[[:digit:]]+)_",
#                                    processors=processors)
#     mWzorcowe    = skaluj(daneWzorcowe[[i]], opisWzorcowe, "id_obserwacji", tytul = tytulWzorcowe,
#                           zmienneDolaczaneDoOszacowan = names(daneWzorcowe[[i]])[grepl("^id_testu", names(daneWzorcowe[[i]]))],
#                           bezWartosciStartowychParametrowTypu = "^threshold$")
#
#     message("### Wyliczanie oszacowań dla wszystkich zdających ", names(daneWzorcowe)[i], " ###\n")
#     wartosciZakotwiczone = mWzorcowe[[1]][[length(mWzorcowe[[1]])]]$parametry$surowe
#     wartosciZakotwiczone = wartosciZakotwiczone[!grepl("^(mean|variance)(|[.]gr[[:digit:]]+)$", wartosciZakotwiczone$typ), ]
#     zmienneKryteriaPoUsuwaniu = wartosciZakotwiczone$zmienna2[wartosciZakotwiczone$typ == "by"]
#     usunieteKryteria = zmienneKryteria[!(zmienneKryteria %in% zmienneKryteriaPoUsuwaniu)]
#     grupy = unique(mWzorcowe[[1]][[length(mWzorcowe[[1]])]]$zapis[, c("gr_tmp1", zmGrupujace)])
#     grupy = grupy[order(grupy$gr_tmp1), ]
#
#     daneWszyscy[[i]] = subset(daneWszyscy[[i]], !apply(is.na(daneWszyscy[[i]][, zmGrupujace, drop=FALSE]), 1, any))
#     opisWszyscy  = procedura_1k_1w(zmienneKryteriaPoUsuwaniu, names(daneWzorcowe)[i],
#                                    wartosciZakotwiczone, wieleGrup = zmGrupujace, processors=processors)
#     mWszyscy     = skaluj(daneWszyscy[[i]] , opisWszyscy , "id_obserwacji", tytul = tytulWszyscy,
#                           zmienneDolaczaneDoOszacowan = names(daneWszyscy[[i]])[grepl("^id_testu", names(daneWszyscy[[i]]))])
#
#     wyniki[[i]] = vector(mode="list", length=length(unique(daneWzorcowe[[i]]$typ_szkoly)))
#     names(wyniki[[i]]) = sort(unique(daneWzorcowe[[i]]$typ_szkoly))
#     for (j in 1:length(wyniki[[i]])) {
#       oszacowania = mWszyscy[[1]][[length(mWszyscy[[1]])]]$zapis
#       oszacowania = subset(oszacowania, get("typ_szkoly") == names(wyniki[[i]])[j])
#       oszacowania = oszacowania[, !(names(oszacowania) %in% c("gr_tmp1", zmGrupujace))]
#       # standaryzacja
#       oszacowaniaWzorcowe = mWzorcowe[[1]][[length(mWszyscy[[1]])]]$zapis
#       oszacowaniaWzorcowe = subset(oszacowaniaWzorcowe, get("typ_szkoly") == names(wyniki[[i]])[j])
#       sr = mean(oszacowaniaWzorcowe[, names(daneWzorcowe)[i]])
#       os = sd(oszacowaniaWzorcowe[, names(daneWzorcowe)[i]])
#       oszacowania[, names(daneWzorcowe)[i]] = (oszacowania[, names(daneWzorcowe)[i]] - sr) / os
#       # koniec standaryzacji
#       wyniki[[i]][[j]] = list(
#         usunieteKryteria = usunieteKryteria,
#         parametry = wartosciZakotwiczone,
#         oszacowania = oszacowania,
#         grupy = grupy
#       )
#     }
#   }
#   return(wyniki)
}
