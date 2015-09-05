#' @title Diagnostyka skalowania
#' @description
#' Funkcja wyrysowuje zestaw wykresów diagnostycznych pozwalających przyjrzeć się wynikom
#' uzyskanym z modelu skalowania.
#' @param model lista zawierająca wyniki skalowania, typowo pojedynczy element listy
#' będącej wynikiem działania funkcji \code{\link{skaluj_egz_gimn}},
#' \code{\link{skaluj_egz_gimn_rasch}}, \code{\link{skaluj_matura_rasch}} lub wynik
#' działania funkcji \code{\link{skaluj_spr}}
#' @param nazwa ciag znaków - nazwa konstruktu
#' @return funkcja nie zwraca żadnych wartości
#' @seealso \code{\link{skaluj_egz_gimn}}, \code{\link{skaluj_egz_gimn_rasch}},
#' \code{\link{skaluj_matura_rasch}}, \code{\link{skaluj_spr}}
#' @export
sprawdz_wyniki_skalowania = function(model, nazwa="") {
  stopifnot(is.list(model),
            is.character(nazwa), length(nazwa) == 1)
  stopifnot(all(c("parametry", "oszacowania") %in% names(model)))
  maska = !grepl("^id_|_se$", names(model$oszacowania))
  if (sum(maska) > 1) stop("Element 'oszacowania' może zawierać tylko jedną zmienną nie opisującą id lub błędów standardowych.")
  model$oszacowania = model$oszacowania[, maska]

#   parametry = model$parametry
#   dyskryminacje = subset(parametry[, c(            "zmienna2", "wartosc")],
#                          parametry$typ == "by")
#   names(dyskryminacje) = c("kryterium", "dyskryminacja")
#   trudnosci     = subset(parametry[, c("zmienna1", "zmienna2", "wartosc")],
#                          parametry$typ == "threshold")
#   names(trudnosci) = c("kryterium", "poziom", "trudnosc")
#   if (length(unique(dyskryminacje$dyskryminacja)) > 1) {
#     pos = with(dyskryminacje, {c(1, 3)[1 + as.numeric(dyskryminacja > 1)]})
#     ylim = c(0, max(c(2, max(dyskryminacje$dyskryminacja)*1.1)))
#     ylab = "dyskryminacja"
#     yaxt = "s"
#   } else {  # model Rascha
#     dyskrTemp = dyskryminacje$dyskryminacja[1]
#     ylim = dyskrTemp + c(-0.06, 0.06)
#     dyskryminacje$dyskryminacja = dyskrTemp + runif(nrow(dyskryminacje), -0.05, 0.05)
#     pos = c(1, 3)[1 + as.numeric(dyskryminacje$dyskryminacja > dyskrTemp)]
#     ylab = paste0("dyskryminacja = ", round(dyskrTemp, 2))
#     yaxt = "n"
#   }
#   trudnosciPoziomow = merge(dyskryminacje, trudnosci)
#   trudnosciPoziomow = merge(trudnosciPoziomow,
#                             aggregate(data.frame(liczba_poziomow =
#                                                    trudnosciPoziomow$poziom),
#                                       as.list(trudnosciPoziomow["kryterium"]),
#                                       function(x) {return(max(as.numeric(x)))}))
#   trudnosciPoziomow = within(trudnosciPoziomow,
#                              {etykieta=paste0(trudnosciPoziomow$kryterium, "$",
#                                               trudnosciPoziomow$poziom)})
#   trudnosciPoziomow = subset(trudnosciPoziomow, trudnosciPoziomow$liczba_poziomow > 1)
#   trudnosciKryteriow = merge(dyskryminacje,
#                              aggregate(trudnosci[, "trudnosc", drop=FALSE],
#                                        as.list(trudnosci[, "kryterium", drop=FALSE]),
#                                        mean))
#   trudnosciKryteriow = trudnosciKryteriow[order(trudnosciKryteriow$trudnosc), ]
#   parGraf = par(no.readonly=TRUE)
#   par(mar=c(4, 4, 3, 0), cex.main=0.8, cex.lab=0.8, cex.axis=0.8)
#   layout(matrix(c(1, 2), ncol=1))
#   zakresTrudnosci = range(trudnosciKryteriow$trudnosc, trudnosciPoziomow$trudnosc)
#   zakresUmiejetnosci = range(model$oszacowania)
#   zakresX = c(-1, 1)*max(abs(range(c(zakresTrudnosci, zakresUmiejetnosci, 3))))
#   with(trudnosciKryteriow,
#        plot(NA, NA,
#             xlim = zakresX,
#             ylim = ylim,
#             main = paste0(nazwa, "\nparametry (pseudo)kryteriów"),
#             xlab = "trudność", ylab = ylab,
#             yaxt = yaxt))
#   grid(col=grey(0.5))
#   for (k in c(0.2, 1, 2, 3)) abline(h=k, col=c(1,2)[1 + as.numeric(k > 1)],
#                                     lty=2, lwd=2)
#   for (k in c(-3, -2, 2, 3)) abline(v=k, col=c(1,2)[1 + as.numeric(abs(k) > 2)],
#                                     lty=2, lwd=2)
#   with(trudnosciKryteriow,
#        points(trudnosc, dyskryminacja,
#               cex = 1.2, pch = 21, col = 1, bg=hsv(1/3, 1, 1, 0.5)))
#   with(trudnosciKryteriow,
#        text(x = trudnosc, y = dyskryminacja,
#             labels = kryterium,
#             pos = pos,
#             offset = 0.3, cex = 0.6))
#   if (nrow(trudnosciPoziomow) > 0) {
#     with(trudnosciPoziomow,
#          points(trudnosc, dyskryminacja, pch=3, col=4))
#   }
#   if (length(model$usunieteKryteria) > 0 ) {
#     legend("bottomright", legend=model$usunieteKryteria,
#            title="usunięte (pseudo)ktryteria:", ncol=2, bg="white",
#            cex=0.6)
#   }
#   h = hist(model$oszacowania, seq(zakresX[1] - 0.05, zakresX[2] + 0.05, length.out=100),
#        xlim = zakresX, col=2,
#        main = paste0("rozkład oszacowań (wszyscy zdający, bez wykluczeń)\nn = ",
#                      prettyNum(length(model$oszacowania), width=6, big.mark="'")),
#        xlab="oszacowania poziomu umiejętności",
#        ylab="liczebność")
#   grid(col=grey(0.5))
#   srednia = mean(model$oszacowania)
#   mediana = median(model$oszacowania)
#   odchStd = sd(model$oszacowania)
#   abline(v=srednia, col=3, lty=2, lwd=2)
#   abline(v=median(model$oszacowania), col=4, lty=2, lwd=2)
#   legend("topright", col=c(3, 4, NA, NA), lty=2, lwd=2,
#          legend=c(
#            paste0("średnia ", format(round(srednia, 3), nsmall=3)),
#            paste0("mediana ", format(round(mediana, 3), nsmall=3)),
#            paste0("odch. stand. ", format(round(odchStd, 3), nsmall=3)),
#            ifelse("rzetelnoscEmpiryczna" %in% names(model),
#                    paste0("rzetelność emp. ",
#                           format(round(model$rzetelnoscEmpiryczna, 3),
#                                  nsmall=3)),
#                    "")),
#          title="parametry rozkładu", bg="white", cex=0.7)
#   # ew. różnice między grupami w modelu wielogrupowym
#   #   if (any(grepl("[.]gr[[:digit:]]+$", model$parametry$typ))) {
#   #     wartOcz =
#   #     wariancje =
#   #     y = seq(0, max(h$counts), length.out=nrow(wartOcz) + 2)[-1]
#   #     for (i in 1:nrow(wartOcz)) {
#   #       wartOczTemp =
#   #       odchStandTemp =
#   #       arrows(wartOczTemp - 2 * odchStandTemp, y[i], wartOczTemp, y[i],
#   #              0.1, 90, 3, lwd=2)
#   #       arrows(wartOczTemp + 2 * odchStandTemp, y[i], wartOczTemp, y[i],
#   #              0.1, 90, 3, lwd=2)
#   #       strona = ifelse ((wartOczTemp + 2 * odchStandTemp) >
#   #                          (zakresX[1] + 0.75 * (zakresX[2] - zakresX[1])),
#   #                        -1, 1)
#   #       text(wartOczTemp + 2 * strona * odchStandTemp, y[i], paste0("gr. ", i),
#   #            pos=3 + strona, font=2)
#   #     }
#   #   }
#   # ew. wyrysowywanie mapowań sum punktów na wyniki wyskalowane
#   if ("mapowanie" %in% names(model)) {
#     zmSumy = names(model$mapowanie)[grep("^suma_", names(model$mapowanie))]
#     names(model$mapowanie)[!grepl("^suma", names(model$mapowanie))] = "oszacowania"
#     # taka sztuczka, żeby mieć listę, po której będę się mógł wygodnie iterować pętlą
#     grupy = dlply(model$mapowanie, zmSumy, function(x) {return(x)})
#     # rysowanie
#     legend = vector(mode="character", length=length(grupy))
#     with(model$mapowanie,
#          plot(NA, NA,
#               xlim = c(0, 100),#range(suma),
#               ylim = range(oszacowania),
#               main = paste0(nazwa, "\nmapowanie procentów punktów na oszacowania IRT (Rasch)"),
#               xlab = "procent punktów", ylab = "oszacowania"))
#     grid(col=grey(0.5))
#     for (i in 1:length(grupy)) {
#       with(grupy[[i]], lines(100* suma / max(suma), oszacowania, lwd=2, col=i))
#       legend[i] = paste0(zmSumy, "=", grupy[[i]][1, zmSumy], collapse="; ")
#     }
#     legend("bottomright", lwd=2, col=1:length(grupy), legend = legend,
#            title = "grupy", bg = "white", cex = 0.7)
#     with(model$mapowanie,
#          plot(NA, NA,
#               xlim = c(0, max(model$mapowanie$suma)),#range(suma),
#               ylim = range(oszacowania),
#               main = paste0(nazwa, "\nmapowanie sum punktów na oszacowania IRT (Rasch)"),
#               xlab = "suma punktów", ylab = "oszacowania"))
#     grid(col=grey(0.5))
#     for (i in 1:length(grupy)) {
#       with(grupy[[i]], lines(suma, oszacowania, lwd=2, col=i))
#       legend[i] = paste0(zmSumy, "=", grupy[[i]][1, zmSumy], collapse="; ")
#     }
#     legend("bottomright", lwd=2, col=1:length(grupy), legend = legend,
#            title = "grupy", bg = "white", cex = 0.7)
#   }
#   # koniec
#   layout(1)
#   par(parGraf)
  invisible(NULL)
}
