#' @title Diagnostyka skalowania
#' @description
#' Funkcja wyrysowuje zestaw wykresów diagnostycznych pozwalających przyjrzeć
#' się wynikom uzyskanym z modelu skalowania.
#' @param nazwaPliku ciąg znaków - nazwa pliku .RData, zawierającego wyniki
#' skalowania (typowo zapisanego przez funkcję \code{\link{skaluj_spr}},
#' \code{\link{skaluj_egz_gimn}}, \code{\link{skaluj_egz_gimn_rasch}},
#' \code{\link{skaluj_matura}} lub \code{\link{skaluj_matura_rasch}}).
#' @return funkcja nie zwraca żadnych wartości
#' @export
sprawdz_wyniki_skalowania = function(nazwaPliku) {
  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1)
  stopifnot(file.exists(nazwaPliku))

  mapowanieNazw = list(
    "s" = "Sprawdzian",
    "gh"   = "Egz. gimn., część humanistyczna",
    "gh_h" = "Egz. gimn., test z historii i WOS",
    "gh_p" = "Egz. gimn., test z języka polskiego",
    "gm"   = "Egz. gimn., część mat.-przyr.",
    "gm_m" = "Egz. gimn., test z matematyki",
    "gm_p" = "Egz. gimn., test z przedm. przyr.",
    "m_h"  = "Matura, przedm. humanistyczne",
    "m_jp" = "Matura, język polski",
    "m_m"  = "Matura, matematyka",
    "m_mp" = "Matura, przedm. mat.-przyr."
  )
  rok = gsub("^.*[^[:digit:]]([[:digit:]]{2,4})Skalowanie.*$", "\\1", nazwaPliku)
  if (rok != "") {
    rok = paste0(" ", rok)
  }

  obiekty = load(nazwaPliku)
  for (i in obiekty) {
    x = get(i)
    rm(list = i)
    if (!("listaWynikowSkalowania" %in% class(x))) {
      next
    }
    lapply(x, sprawdz_wyniki_skalowania_konstruktu, rok = rok)
  }
  # koniec
  invisible(NULL)
}
#' @title Diagnostyka skalowania
#' @description
#' Funkcja wyrysowuje zestaw wykresów diagnostycznych pozwalających przyjrzeć
#' się wynikom uzyskanym z modelu skalowania.
#' @param model obiekt klasy \code{wynikiSkalowania}
#' @param rok ciąg znaków opisujący rok przeprowadzenia egzaminu
#' @return funkcja nie zwraca żadnych wartości
sprawdz_wyniki_skalowania_konstruktu = function(model, rok) {
  stopifnot(is.list(model), "wynikiSkalowania" %in% class(model),
            is.character(rok), length(rok) == 1)
  stopifnot("oszacowania" %in% names(model))

  mapowanieNazw = list(
    "s" = "Sprawdzian",
    "gh"   = "Egz. gimn., część humanistyczna",
    "gh_h" = "Egz. gimn., test z historii i WOS",
    "gh_p" = "Egz. gimn., test z języka polskiego",
    "gm"   = "Egz. gimn., część mat.-przyr.",
    "gm_m" = "Egz. gimn., test z matematyki",
    "gm_p" = "Egz. gimn., test z przedm. przyr.",
    "m_h"  = "Matura, przedm. humanistyczne",
    "m_jp" = "Matura, język polski",
    "m_m"  = "Matura, matematyka",
    "m_mp" = "Matura, przedm. mat.-przyr."
  )

  # przygotowywanie oszacowań
  maska = !grepl("^id_|_se$", names(model$oszacowania))
  model$oszacowania = model$oszacowania[, maska, drop = FALSE]

  # przygotowywanie parametrów zadań
  if (!is.null(model$parametry)) {
    parametry = model$parametry
    dyskryminacje = subset(parametry[, c("zmienna1", "zmienna2", "wartosc")],
                           parametry$typ == "by")
    names(dyskryminacje) = c("konstrukt", "kryterium", "dyskryminacja")
    trudnosci     = subset(parametry[, c("zmienna1", "zmienna2", "wartosc")],
                           parametry$typ == "threshold")
    names(trudnosci) = c("kryterium", "poziom", "trudnosc")

    trudnosciPoziomow = merge(dyskryminacje, trudnosci)
    trudnosciPoziomow =
      merge(trudnosciPoziomow,
            aggregate(data.frame(liczba_poziomow = trudnosciPoziomow$poziom),
                      as.list(trudnosciPoziomow["kryterium"]),
                      function(x) {return(max(as.numeric(x)))}))
    trudnosciPoziomow = within(trudnosciPoziomow, {
      etykieta = paste0(get("kryterium"), "$", get("poziom"))
    })
    trudnosciPoziomow = subset(trudnosciPoziomow,
                               trudnosciPoziomow$liczba_poziomow > 1)
    trudnosciKryteriow =
      merge(dyskryminacje,
            aggregate(trudnosci[, "trudnosc", drop = FALSE],
                      as.list(trudnosci[, "kryterium", drop = FALSE]),
                      mean))
    trudnosciKryteriow =
      trudnosciKryteriow[order(trudnosciKryteriow$trudnosc), ]
    rm(dyskryminacje, trudnosci, parametry)
  }

  # jedziemy po konstruktach
  parGraf = par(no.readonly = TRUE)
  par(mar = c(4, 4, 3, 0), cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  on.exit(par(parGraf))
  for (j in names(model$oszacowania)) {
    # nazwa na wykresy
    nazwa = j
    if (sub("R$", "", nazwa) %in% names(mapowanieNazw)) {
      nazwa = paste0(mapowanieNazw[[nazwa]],
                     ifelse(grepl("R$", "", nazwa), " - model Rascha", ""))
    }
    nazwa = paste0(nazwa, rok)
    # wykres parametrów
    if (!is.null(model$parametry)) {
      cat("\n\n")
      trudnosciPoziomow = subset(trudnosciPoziomow, get("konstrukt") == j)
      trudnosciKryteriow = subset(trudnosciKryteriow, get("konstrukt") == j)

      if (length(unique(trudnosciKryteriow$dyskryminacja)) > 1) {# nie-Rasch
        pos = c(1, 3)[1 + as.numeric(trudnosciKryteriow$dyskryminacja > 1)]
        ylim = c(0, max(c(2, max(trudnosciKryteriow$dyskryminacja) * 1.1)))
        ylab = "dyskryminacja"
        yaxt = "s"
      } else {# model Rascha
        dyskrTemp = trudnosciKryteriow$dyskryminacja[1]
        ylim = dyskrTemp + c(-0.06, 0.06)
        trudnosciKryteriow$dyskryminacja =
          dyskrTemp + runif(nrow(trudnosciKryteriow), -0.05, 0.05)
        pos = c(1, 3)[1 + as.numeric(trudnosciKryteriow$dyskryminacja > dyskrTemp)]
        ylab = paste0("dyskryminacja = ", round(dyskrTemp, 2))
        yaxt = "n"
      }
      zakresTrudnosci = range(trudnosciKryteriow$trudnosc,
                              trudnosciPoziomow$trudnosc)
      zakresUmiejetnosci = range(model$oszacowania[, j])
      zakresX = c(-1, 1) * max(abs(range(c(zakresTrudnosci,
                                           zakresUmiejetnosci, 3))))
      with(trudnosciKryteriow,
           plot(NA, NA,
                xlim = zakresX,
                ylim = ylim,
                main = paste0(nazwa, "\nparametry (pseudo)kryteriów"),
                xlab = "trudność", ylab = ylab,
                yaxt = yaxt))
      grid(col = grey(0.5))
      for (k in c(0.2, 1, 2, 3)) abline(h = k, lty = 2, lwd = 2,
                                        col = c(1,2)[1 + as.numeric(k > 1)])
      for (k in c(-3, -2, 2, 3)) abline(v = k, lty = 2, lwd = 2,
                                        col = c(1,2)[1 + as.numeric(abs(k) > 2)])
      with(trudnosciKryteriow,
           points(trudnosc, dyskryminacja,
                  cex = 1.2, pch = 21, col = 1, bg = hsv(1/3, 1, 1, 0.5)))
      with(trudnosciKryteriow,
           text(x = trudnosc, y = dyskryminacja,
                labels = kryterium,
                pos = pos,
                offset = 0.3, cex = 0.6))
      if (nrow(trudnosciPoziomow) > 0) {
        with(trudnosciPoziomow,
             points(trudnosc, dyskryminacja, pch = 3, col = 4))
      }
      if (length(model$usunieteKryteria) > 0 ) {
        legend("bottomright", legend = model$usunieteKryteria,
               title = "usunięte (pseudo)ktryteria:", ncol = 2, bg = "white",
               cex = 0.6)
      }
    } else {
      cat(nazwa, ", id_skali: ", model$idSkali, ", skalowanie: ",
          model$skalowanie, ":\nBrak informacji o Wartościach parametrów zadań. ",
          "Najprawdopodobniej zostały pobrane z bazy.", sep = "")
      zakresX = c(-1, 1) * max(c(abs(range(model$oszacowania[, j])), 3))
    }
    # histogram oszacowań
    zm = na.omit(model$oszacowania[, j])
    h = hist(zm,
             seq(zakresX[1] - 0.05, zakresX[2] + 0.05, length.out = 100),
             xlim = zakresX, col = 2,
             main = paste0(nazwa, "\nrozkład oszacowań (n = ",
                           prettyNum(length(zm), width = 6, big.mark = "'"), ")"),
             xlab = paste0("oszacowania poziomu umiejętności\nid_skali: ",
                           model$idSkali, ", skalowanie: ", model$skalowanie),
             ylab = "liczebność")
    grid(col = grey(0.5))
    srednia = mean(zm)
    mediana = median(zm)
    odchStd = sd(zm)
    abline(v = srednia, col = 3, lty = 2, lwd = 2)
    abline(v = mediana, col = 4, lty = 2, lwd = 2)
    legend("topright", col = c(3, 4, NA, NA), lty = 2, lwd = 2,
           legend = c(
             paste0("średnia ", format(round(srednia, 3), nsmall = 3)),
             paste0("mediana ", format(round(mediana, 3), nsmall = 3)),
             paste0("odch. stand. ", format(round(odchStd, 3), nsmall = 3)),
             ifelse(!is.null(model$rzetelnoscEmpiryczna),
                    paste0("rzetelność emp. ",
                           format(round(model$rzetelnoscEmpiryczna, 3),
                                  nsmall = 3)),
                    "")),
           title = "parametry rozkładu", bg = "white", cex = 0.7)
    #       # ew. różnice między grupami w modelu wielogrupowym
    #       if (any(grepl("[.]gr[[:digit:]]+$", model$parametry$typ))) {
    #         wartOcz =
    #           wariancje =
    #           y = seq(0, max(h$counts), length.out=nrow(wartOcz) + 2)[-1]
    #         for (i in 1:nrow(wartOcz)) {
    #           wartOczTemp =
    #             odchStandTemp =
    #             arrows(wartOczTemp - 2 * odchStandTemp, y[i], wartOczTemp, y[i],
    #                    0.1, 90, 3, lwd=2)
    #           arrows(wartOczTemp + 2 * odchStandTemp, y[i], wartOczTemp, y[i],
    #                  0.1, 90, 3, lwd=2)
    #           strona = ifelse ((wartOczTemp + 2 * odchStandTemp) >
    #                              (zakresX[1] + 0.75 * (zakresX[2] - zakresX[1])),
    #                            -1, 1)
    #           text(wartOczTemp + 2 * strona * odchStandTemp, y[i], paste0("gr. ", i),
    #                pos=3 + strona, font=2)
    #         }
    #       }
    #       # ew. wyrysowywanie mapowań sum punktów na wyniki wyskalowane
    #       if ("mapowanie" %in% names(model)) {
    #         zmSumy = names(model$mapowanie)[grep("^suma_", names(model$mapowanie))]
    #         names(model$mapowanie)[!grepl("^suma", names(model$mapowanie))] = "oszacowania"
    #         # taka sztuczka, żeby mieć listę, po której będę się mógł wygodnie iterować pętlą
    #         grupy = dlply(model$mapowanie, zmSumy, function(x) {return(x)})
    #         # rysowanie
    #         legend = vector(mode="character", length=length(grupy))
    #         with(model$mapowanie,
    #              plot(NA, NA,
    #                   xlim = c(0, 100),#range(suma),
    #                   ylim = range(oszacowania),
    #                   main = paste0(nazwa, "\nmapowanie procentów punktów na oszacowania IRT (Rasch)"),
    #                   xlab = "procent punktów", ylab = "oszacowania"))
    #         grid(col=grey(0.5))
    #         for (i in 1:length(grupy)) {
    #           with(grupy[[i]], lines(100* suma / max(suma), oszacowania, lwd=2, col=i))
    #           legend[i] = paste0(zmSumy, "=", grupy[[i]][1, zmSumy], collapse="; ")
    #         }
    #         legend("bottomright", lwd=2, col=1:length(grupy), legend = legend,
    #                title = "grupy", bg = "white", cex = 0.7)
    #         with(model$mapowanie,
    #              plot(NA, NA,
    #                   xlim = c(0, max(model$mapowanie$suma)),#range(suma),
    #                   ylim = range(oszacowania),
    #                   main = paste0(nazwa, "\nmapowanie sum punktów na oszacowania IRT (Rasch)"),
    #                   xlab = "suma punktów", ylab = "oszacowania"))
    #         grid(col=grey(0.5))
    #         for (i in 1:length(grupy)) {
    #           with(grupy[[i]], lines(suma, oszacowania, lwd=2, col=i))
    #           legend[i] = paste0(zmSumy, "=", grupy[[i]][1, zmSumy], collapse="; ")
    #         }
    #         legend("bottomright", lwd=2, col=1:length(grupy), legend = legend,
    #                title = "grupy", bg = "white", cex = 0.7)
    #       }
  }
  # koniec
  invisible(NULL)
}
