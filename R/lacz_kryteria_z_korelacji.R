#' @title Laczenie kryteriow na podstawie korelacji polichorycznych
#' @description
#' Funkcja łączy, w ramach podanych skal, kryteria oceny poszczególnych zadań
#' w pseudokryteria. Łączenie odbywa się na podstawie wartości korelacji
#' polichorycznych, wyliczanych przy pomocy funkcji
#' \code{\link[polycor]{polychor}} z pakietu \code{polycor}.
#' @param skale wektor liczbowy z id_skali lub ciąg znaków z wyrażeniem
#' regularnym identyfikującymi skale po kolumnie 'opis'
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param prog liczbowa z zakresu (0;1) - minimalna wartość korelacji, do której
#' ma być kontynuowane łączenie
#' @param tylkoWWiazkach wartość logiczna - czy dopuszczać łączenie kryteriów
#' tylko w ramach wiązek pytań/kryteriów?
#' @return lista, której każdy element jest dwuelemntową listą, zawierającą id
#' skali oraz data frame, której można użyć jako argument \code{elementy}
#' funkcji \code{\link[ZPDzapis]{edytuj_skale}}
#' @import ZPD
#' @export
lacz_kryteria_z_korelacji = function(skale, katalogDane = "dane surowe/",
                                     prog = 0.5, tylkoWWiazkach = TRUE) {
  stopifnot((is.numeric(skale) & length(skale) > 0) |
              (is.character(skale) & length(skale) == 1),
            is.character(katalogDane), length(katalogDane) == 1,
            is.numeric(prog), length(prog) == 1,
            tylkoWWiazkach %in% c(TRUE, FALSE), length(tylkoWWiazkach) == 1)
  stopifnot(prog > 0, prog < 1)
  if (!dir.exists(katalogDane)) {
    stop("Katalog '", katalogDane, "' nie istnieje.")
  }

  kryteria = pobierz_kryteria_do_laczenia(skale)
  temp = group_by_(kryteria, ~id_skali) %>%
    do_(.dots = list(~lacz_kryteria_z_korelacji_w_ramach_skali(.,
                                                               katalogDane,
                                                               prog,
                                                               tylkoWWiazkach)))
  class(temp) = append(class(temp), "wynikLaczKryteriaZKorelacji")
  return(temp)
}
#' @title Laczenie kryteriow na podstawie korelacji polichorycznych
#' @description Koń roboczy wywoływany przez \code{\link{lacz_kryteria_z_korelacji}}.
#' @param x data frame opisujący kryteria oceny w ramach skali
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param prog liczbowa z zakresu (0;1) - minimalna wartość korelacji, do której
#' ma być kontynuowane łączenie
#' @param tylkoWWiazkach wartość logiczna - czy dopuszczać łączenie kryteriów
#' tylko w ramach wiązek pytań/kryteriów?
#' @return data frame, której można uzyć jako argument \code{elementy}
#' funkcji \code{\link[ZPDzapis]{edytuj_skale}}
#' @import dplyr
#' @import reshape2
lacz_kryteria_z_korelacji_w_ramach_skali = function(x, katalogDane, prog,
                                                    tylkoWWiazkach) {
  stopifnot(tylkoWWiazkach %in% c(TRUE, FALSE), length(tylkoWWiazkach) == 1,
            is.numeric(prog), length(prog) == 1,
            is.character(katalogDane), length(katalogDane) == 1)
  stopifnot(prog > 0, prog < 1)
  stopifnot(dir.exists(katalogDane))

  message("Skala '", x$opis_skali[1], "', id_skali = ", x$id_skali[1], ":")
  x = group_by_(x, ~rodzaj_egzaminu, ~czesc_egzaminu, ~rok) %>%
    do_(.dots = setNames(list(~lacz_kryteria_z_korelacji_w_ramach_czesci_egz(.,
                                                                             katalogDane,
                                                                             prog,
                                                                             tylkoWWiazkach)),
                         "laczenia"))
  return(x)
}
#' @title Laczenie kryteriow na podstawie korelacji polichorycznych
#' @description Koń roboczy wywoływany przez
#' \code{\link{lacz_kryteria_z_korelacji_w_ramach_skali}}.
#' @param x data frame opisujący kryteria oceny w ramach skali
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' \code{\link[EWDdane]{pobierz_wyniki_surowe}}
#' @param prog liczbowa z zakresu (0;1) - minimalna wartość korelacji, do której
#' ma być kontynuowane łączenie
#' @param tylkoWWiazkach wartość logiczna - czy dopuszczać łączenie kryteriów
#' tylko w ramach wiązek pytań/kryteriów?
#' @return data frame
#' @import ZPD
#' @import dplyr
#' @import polycor
#' @import mirt
lacz_kryteria_z_korelacji_w_ramach_czesci_egz = function(x, katalogDane, prog,
                                                         tylkoWWiazkach) {
  stopifnot(tylkoWWiazkach %in% c(TRUE, FALSE), length(tylkoWWiazkach) == 1,
            is.numeric(prog), length(prog) == 1,
            is.character(katalogDane), length(katalogDane) == 1)
  stopifnot(prog > 0, prog < 1)
  stopifnot(dir.exists(katalogDane))

  message(" ", x$rodzaj_egzaminu[1], " ", x$rok[1],
          ifelse(x$czesc_egzaminu[1] != "", ", część ", ""),
          x$czesc_egzaminu[1], ":")
  # wczytywanie danych z wynikami egzaminu
  dane = wczytaj_wyniki_surowe(katalogDane, x$rodzaj_egzaminu[1],
                              x$czesc_egzaminu[1], x$rok[1], x$id_skali[1],
                              x$kryterium)
  dane = filter_(dane, ~populacja_wy & !pomin_szkole)
  dane = dane[, grep("^[kp]_", names(dane))]
  message("  Wczytano dane z wynikami egzaminu.")

  # przygotowanie obiektu tylko z interesującymi nas parami zmiennych
  pary = setNames(data.frame(t(combn(x$kryterium, 2)), stringsAsFactors = FALSE),
                  c("kryterium", "kryterium2"))
  temp = select_(x, ~kryterium, ~id_wiazki)
  pary = suppressMessages(left_join(pary, temp))
  names(temp) = paste0(names(temp), "2")
  pary = suppressMessages(left_join(pary, temp))
  if (tylkoWWiazkach) {
    pary = filter_(pary, ~id_wiazki == id_wiazki2)
    if (nrow(pary) == 0) {
      warning("Nie zdefiniowano żadnych wiązek.", immediate. = TRUE)
      return(list(laczenia = NULL, dyskryminacje = NULL))
    }
  }
  pary = cbind(pary, korelacja = NA)
  laczenia = matrix(NA, ncol = ncol(pary), nrow = nrow(pary)) %>%
    as.data.frame %>%
    setNames(names(pary))
  dyskryminacje = matrix(NA, ncol = ncol(dane), nrow = nrow(pary))
  colnames(dyskryminacje) = names(dane)
  rownames(dyskryminacje) = 0:(nrow(dyskryminacje) - 1)

  # wyliczanie dyskryminacji
  message("  Wyliczanie dyskryminacji w modelu jednowymiarowym ",
          "(może trochę potrwać...).")
  model = suppressMessages(mirt(dane, 1, TOL = 0.01, verbose = FALSE))
  dyskryminacjeTemp = unlist(lapply(coef(model), function(x) {return(x[1, 1])}))
  dyskryminacje[1, ] = dyskryminacjeTemp[grep("^[kp]_", names(dyskryminacjeTemp))]
  # wyliczanie korelacji
  message("  Wyliczanie ", nrow(pary), " korelacji polichorycznych ",
          "(może trochę potrwać...)")
  pb = txtProgressBar(0, nrow(pary), style = 3)
  for (i in 1:nrow(pary)) {
    pary$korelacja[i] = suppressWarnings(
      polychor(ftable(dane[, c(pary$kryterium[i], pary$kryterium2[i])]),
               ML = FALSE, std.err = FALSE))  # użycie ftable pozwala uniknąć konwersji danych na factory
    setTxtProgressBar(pb, i)
  }
  close(pb)
  if (!tylkoWWiazkach) {
    progTemp = mean(pary$korelacja, na.rm = TRUE)
    if (progTemp > prog) {
      prog = progTemp
      message(" Wartość progu została zwiększona do wartości średniej korelacji ",
              "w grupie wszystkich par zmiennych.")
    }
  }
  message(" Spośród wyliczonych korelacji ",
          sum(pary$korelacja > prog, na.rm = TRUE),
          " ma(ją) wartość powyżej progu równego ",
          format(prog, digits = 3, nsmall = 3),
          ".\n Łączenie kryteriów:")
  while (max(pary$korelacja, na.rm = TRUE) > prog) {
    wierszMax = which.max(pary$korelacja)
    k1 = pary$kryterium[wierszMax]
    k2 = pary$kryterium2[wierszMax]
    message("  połączono ", k1, " i ", k2, " przy korelacji ",
            format(pary$korelacja[wierszMax], digits = 3, nsmall = 3))
    laczenia[sum(!is.na(laczenia$korelacja)) + 1, ] = pary[wierszMax, ]
    dane[, pary$kryterium[wierszMax]] = rowSums(dane[, c(pary$kryterium[wierszMax],
                                                         pary$kryterium2[wierszMax])])
    pary$korelacja[pary$kryterium %in% k2 | pary$kryterium2 %in% k2] = NA
    maska = which(pary$kryterium %in% k1 & !is.na(pary$korelacja))
    maska = maska[maska != wierszMax]
    for (i in maska) {
      pary$korelacja[i] = suppressWarnings(
        polychor(ftable(dane[, c(pary$kryterium[i], pary$kryterium2[i])]),
                 ML = FALSE, std.err = FALSE))  # użycie ftable pozwala uniknąć konwersji danych na factory
    }
    # wyliczanie dyskryminacji
    maska = !(names(dane) %in% laczenia$kryterium2)
    if (sum(maska) <= 1) {
      next
    }
    model = suppressMessages(mirt(dane[, maska], 1, TOL = 0.01, verbose = FALSE))
    # to jest to samo, ale naklepane z wartościami startowymi - tyle że daje z 8% zysku szybkości
    #pars = mod2values(model)[, c("item", "class", "name", "value")]
    #maska = !(names(dane) %in% laczenia$kryterium2)
    #model = parsNowe = mirt(dane[, maska], 1, large = TRUE)
    #parsTemp = mirt(dane[, maska], 1, large = model, pars = "values")
    #pars = suppressWarnings(semi_join(pars, parsTemp,
    #                                  by = c("item", "class", "name")))
    #parsNowe = suppressWarnings(anti_join(parsTemp, pars,
    #                                      by = c("item", "class", "name")))
    #parsNowe = parsNowe[, c("item", "class", "name", "value")]
    #pars = suppressWarnings(left_join(parsTemp[, names(parsTemp) != "value"],
    #                                  rbind(pars, parsNowe),
    #                                  by = c("item", "class", "name")))
    #pars$item = factor(pars$item)
    #pars = pars[, c("group", "item", "class", "name", "parnum", "value",
    #                "lbound", "ubound", "est", "prior.type", "prior_1", "prior_2")]
    #model = mirt(dane[, maska], 1, large = model, pars = pars,
    #             TOL = 0.01, verbose = FALSE)
    dyskryminacjeTemp = unlist(lapply(coef(model), function(x) {return(x[1, 1])}))
    dyskryminacjeTemp = dyskryminacjeTemp[grep("^[kp]_", names(dyskryminacjeTemp))]
    dyskryminacje[sum(!is.na(laczenia$korelacja)) + 1,
                  colnames(dyskryminacje) %in% names(dyskryminacjeTemp)] =
      dyskryminacjeTemp
  }

  laczenia = laczenia[!is.na(laczenia[, 1]), ]
  dyskryminacje = dyskryminacje[1:(nrow(laczenia) + 1), ]
  return(list(laczenia = laczenia, dyskryminacje = dyskryminacje))
}
