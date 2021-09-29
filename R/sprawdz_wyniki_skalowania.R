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

  obiekty = load(nazwaPliku)
  for (i in obiekty) {
    x = get(i)
    rm(list = i)
    if (!("listaWynikowSkalowania" %in% class(x))) {
      next
    }
    lapply(x, sprawdz_wyniki_skalowania_konstruktu)
  }
  # koniec
  invisible(NULL)
}
#' @title Diagnostyka skalowania
#' @description
#' Funkcja wyrysowuje zestaw wykresów diagnostycznych pozwalających przyjrzeć
#' się wynikom uzyskanym z modelu skalowania.
#' @param model obiekt klasy \code{wynikiSkalowania}
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
#' @return funkcja nie zwraca żadnych wartości
#' @importFrom stats na.omit setNames runif median sd
#' @importFrom graphics hist grid abline legend par
#' @importFrom grDevices grey
#' @import ZPD
sprawdz_wyniki_skalowania_konstruktu = function(model, src = NULL) {
  stopifnot(is.list(model), "wynikiSkalowania" %in% class(model),
            dplyr::is.src(src) | is.null(src))
  stopifnot("skalowania" %in% names(model), is.data.frame(model$skalowania),
            "skalowania_obserwacje" %in% names(model),
            is.data.frame(model$skalowania_obserwacje))
  stopifnot(nrow(model$skalowania) == 1)
  if (is.null(src)) {
    src = ZPD::polacz()
  }

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
  # tytul na wykresy
  tytul = pobierz_skale(src, doPrezentacji = NA) %>%
    filter(.data$id_skali == local(model$skalowania$id_skali)) %>%
    select("opis_skali", "rok") %>%
    collect() %>%
    distinct()
  rok = tytul$rok
  tytul = sub("^ewd;([^;]+);.*$", "\\1", tytul$opis_skali)
  if (sub("R$", "", tytul) %in% names(mapowanieNazw)) {
    tytul = paste0(mapowanieNazw[[tytul]],
                   ifelse(grepl("R$", "", tytul), " - model Rascha", ""))
  }
  tytul = paste0(tytul, " ", rok)
  cat("\n\n## ", tytul, ", id_skali: ", model$skalowania$id_skali, ", skalowanie: ",
      model$skalowania$skalowanie, ":\n", sep = "")

  # przygotowywanie oszacowań
  lBD = sum(is.na(model$skalowania_obserwacj$wynik))
  oszacowania = na.omit(model$skalowania_obserwacje$wynik)
  if (grepl("^Matura", tytul)) {
    pliki = c("../../dane surowe/matura-kontekstowe.RData",
              "../../../dane surowe/matura-kontekstowe.RData",
              "../matura-kontekstowe.RData", "matura-kontekstowe.RData")
    if (any(file.exists(pliki))) {
      obiekty = load(pliki[file.exists(pliki)][1])
      if ("mKontekstowe" %in% obiekty) {
        mKontekstowe = get("mKontekstowe")
        oszacowaniaGrupy = suppressMessages(
          mutate(model$skalowania_obserwacje, rok = rok) %>%
            left_join(mKontekstowe) %>%
            filter(.data$populacja_wy == TRUE, !.data$pomin_szkole) %>%
            mutate(grupa = c("T", "LO")[1 + as.numeric(grepl("LO", .data$grupa))]) %>%
            select("wynik", "grupa") %>%
            group_by(.data$grupa)
        )
      }
    }

  }

  # obsługa parametrów okna wykresu
  parGraf = par(no.readonly = TRUE)
  par(mar = c(4, 4, 3, 0), cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  on.exit(par(parGraf))

  # wykres rozrzutu trudność x dyskryminacja
  rzetelnoscEmpiryczna = NULL
  if (is.data.frame(model$skalowania_elementy)) {
    # przygotowywanie danych
    parametry = model$skalowania_elementy
    if ("r EAP" %in% parametry$parametr) {
      rzetelnoscEmpiryczna = parametry$wartosc[parametry$parametr %in% "r EAP"]
    }
    # dołączanie numerów (pseudo)kryteriów
    names(parametry) = sub("kolejnosc", "kolejnosc_w_skali", names(parametry))
    parametry = suppressMessages(
      pobierz_kryteria_oceny(src) %>%
        select("id_skali", "kolejnosc_w_skali", "kryterium") %>%
        filter(.data$id_skali == local(parametry$id_skali[1])) %>%
        collect() %>%
        distinct() %>%
        right_join(parametry)
    )
    # "normalizacja" nazw parametrów selekcji
    maskaSpecjalne = with(parametry, {is.na(kryterium) & !is.na(uwagi)})
    parametry$kryterium[maskaSpecjalne] = parametry$uwagi[maskaSpecjalne]
    # podział na typy parametrów, przekształcanie i łączenie
    dyskryminacje = filter(parametry, .data$parametr %in% c("a", "dyskryminacja")) %>%
      select("kryterium", "wartosc")
    names(dyskryminacje) = sub("wartosc", "dyskryminacja", names(dyskryminacje))
    trudnosciKryteriow = suppressMessages(
      filter(parametry, .data$parametr == "trudność") %>%
        select("kryterium", "wartosc") %>%
        left_join(dyskryminacje)
    )
    names(trudnosciKryteriow) = sub("wartosc", "trudnosc", names(trudnosciKryteriow))
    trudnosciPoziomow = suppressMessages(
      filter(parametry, grepl("^b[[:digit:]]+$", .data$parametr)) %>%
        select("kryterium", "parametr", "wartosc") %>%
        left_join(trudnosciKryteriow) %>%
        mutate(wartosc = .data$wartosc + .data$trudnosc,
               kryterium = paste0(.data$kryterium, "$",
                                  sub("^b", "", .data$parametr))) %>%
        select(-"trudnosc", -"parametr")
    )
    names(trudnosciPoziomow) = sub("wartosc", "trudnosc", names(trudnosciPoziomow))
    rm(dyskryminacje, parametry)

    # samo rysowanie
    cat("\n")
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
    zakresTrudnosci =
      range(trudnosciKryteriow$trudnosc[abs(trudnosciKryteriow$trudnosc) < 6],
            trudnosciPoziomow$trudnosc[abs(trudnosciPoziomow$trudnosc) < 6])
    if (any(abs(trudnosciKryteriow$trudnosc) > 6)) {
      cat("| kryterium | trudność | dyskryminacja |\n",
          "|:---|---:|---:|\n", sep = "")
      cat(with(filter(trudnosciKryteriow, abs(.data$trudnosc) > 6),
               paste0("| ", kryterium, " | ", round(trudnosc, 2),
                      " | ", round(dyskryminacja, 2), " |", collapse = "\n")))
      cat("\n\nTable: (Pseudo)kryteria o ekstremalnych wartościach trudności (nie przedstawione na wykresie)\n\n")
    }
    if (any(abs(trudnosciPoziomow$trudnosc) > 6)) {
      cat("| kryterium$poziom | trudność | dyskryminacja |\n",
          "|:---|---:|---:|\n", sep = "")
      cat(with(filter(trudnosciPoziomow, abs(.data$trudnosc) > 6),
               paste0("| ", kryterium, " | ", round(trudnosc, 2),
                      " | ", round(dyskryminacja, 2), " |", collapse = "\n")))
      cat("\n\nTable: Poziomy wykonania (pseudo) kryteriów o ekstremalnych wartościach trudności (nie przedstawione na wykresie)\n\n")
    }
    zakresUmiejetnosci = range(oszacowania)
    zakresX = c(-1, 1) * max(abs(range(c(zakresTrudnosci,
                                         zakresUmiejetnosci, 3))))
    with(trudnosciKryteriow,
         plot(NA, NA,
              xlim = zakresX,
              ylim = ylim,
              main = paste0(tytul, "\nparametry (pseudo)kryteriów"),
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
             title = "usunięte (pseudo)kryteria:", ncol = 2, bg = "white",
             cex = 0.6)
    }
  } else {
    cat("Brak informacji o Wartościach parametrów zadań. ",
        "Do skalowania były one (zapewne) pobrane z bazy.", sep = "")
    zakresX = c(-1, 1) * max(c(abs(range(oszacowania)), 3))
  }
  # histogram oszacowań
  h = hist(oszacowania,
           seq(zakresX[1] - 0.05, zakresX[2] + 0.05, length.out = 100),
           xlim = zakresX, col = 2,
           main = paste0(tytul, "\nrozkład oszacowań (n = ",
                         prettyNum(length(oszacowania), big.mark = "'"),
                         ifelse(lBD > 0,
                                paste0(" oraz ", prettyNum(lBD, big.mark = "'"),
                                       " b.d."),
                                ""),
                         ")"),
           xlab = paste0("oszacowania poziomu umiejętności\nid_skali: ",
                         model$skalowania$id_skali, ", skalowanie: ",
                         model$skalowania$skalowanie),
           ylab = "liczebność")
  grid(col = grey(0.5))
  srednia = mean(oszacowania)
  mediana = median(oszacowania)
  odchStd = sd(oszacowania)
  abline(v = srednia, col = 3, lty = 2, lwd = 2)
  abline(v = mediana, col = 4, lty = 2, lwd = 2)
  legend("topright", col = c(3, 4, NA, NA), lty = 2, lwd = 2,
         legend = c(
           paste0("średnia ", format(round(srednia, 3), nsmall = 3)),
           paste0("mediana ", format(round(mediana, 3), nsmall = 3)),
           paste0("odch. stand. ", format(round(odchStd, 3), nsmall = 3)),
           ifelse(!is.null(rzetelnoscEmpiryczna),
                  paste0("rzetelność emp. ",
                         format(round(rzetelnoscEmpiryczna, 3), nsmall = 3)),
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
  #                   main = paste0(tytul, "\nmapowanie procentów punktów na oszacowania IRT (Rasch)"),
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
  #                   main = paste0(tytul, "\nmapowanie sum punktów na oszacowania IRT (Rasch)"),
  #                   xlab = "suma punktów", ylab = "oszacowania"))
  #         grid(col=grey(0.5))
  #         for (i in 1:length(grupy)) {
  #           with(grupy[[i]], lines(suma, oszacowania, lwd=2, col=i))
  #           legend[i] = paste0(zmSumy, "=", grupy[[i]][1, zmSumy], collapse="; ")
  #         }
  #         legend("bottomright", lwd=2, col=1:length(grupy), legend = legend,
  #                title = "grupy", bg = "white", cex = 0.7)
  #       }
  if (exists("oszacowaniaGrupy", environment(), inherits = FALSE)) {
    cat("\n\n",
        "| grupa | średnia | odch. std. |\n",
        "|:---|---:|---:|\n", sep = "")
    cat(with(summarise(oszacowaniaGrupy,
              sr = format(round(mean(.data$wynik), 3), nsmall = 3),
              os = format(round(sd(.data$wynik), 3), nsmall = 3)),
             paste0("| ", grupa, " | ", sr, " | ", os, " |", collapse = "\n")))
    cat("\n\nTable: Średnie i odchylenia standardowe oszacowań umiejętności (grupa objęta skalowaniem wzorcowym)\n\n")
  }

  # koniec
  invisible(NULL)
}
