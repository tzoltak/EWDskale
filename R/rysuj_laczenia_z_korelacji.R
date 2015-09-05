#' @title Laczenie kryteriow na podstawie korelacji polichorycznych
#' @description
#' Funkcja wizualizuje skutki łączeń kryteriów oceny, będących efektem użycia
#' funkcji \code{\link{lacz_kryteria_z_korelacji}} dla dyskryminacji zadań.
#' @param x obiekt klasy \code{wynikLaczKryteriaZKorelacji}
#' @param wielkoscTekstu bazowa wielkość tekstu w \code{pts}
#' @return data frame
#' @import dplyr
#' @export
rysuj_laczenia_z_korelacji = function(x, wielkoscTekstu = 1) {
  stopifnot("wynikLaczKryteriaZKorelacji" %in% class(x),
            is.numeric(wielkoscTekstu), length(wielkoscTekstu) == 1)
  stopifnot(is.finite(wielkoscTekstu), wielkoscTekstu > 0)
  x = do_(x, .dots = list(~rysuj_laczenia_z_korelacji_w_ramach_skali(., wielkoscTekstu)))
  class(x) = sub("wynikLaczKryteriaZKorelacji",
                 "wynikRysujLaczeniaZKorelacji", class(x))
  return(x)
}
#' @title Laczenie kryteriow na podstawie korelacji polichorycznych
#' @description
#' Koń roboczy dla \code{\link{rysuj_laczenia_z_korelacji}}.
#' @param x pojedynczy wiersz obiektu klasy \code{wynikLaczKryteriaZKorelacji}
#' @param wielkoscTekstu bazowa wielkość tekstu w \code{pts}
#' @return data frame
#' @import dplyr
#' @import ggplot2
rysuj_laczenia_z_korelacji_w_ramach_skali = function(x, wielkoscTekstu = 12) {
  stopifnot(is.numeric(wielkoscTekstu), length(wielkoscTekstu) == 1)
  stopifnot(is.finite(wielkoscTekstu), wielkoscTekstu > 0)

  egzamin = x[, names(x) %in% c("rodzaj_egzaminu", "czesc_egzaminu", "rok")]
  egzamin = mutate(egzamin, wykres = NA, elementy = NA)
  tytul = with(x, paste0(rodzaj_egzaminu, " ", rok, ", część ",
                         czesc_egzaminu, "\n", "id_skali: ", id_skali))
  with(x, cat(tytul, "\n", "zbadano ", nrow(laczenia[[1]]$laczenia),
                  " łączeń/nia/nie\n\n", sep = ""))
  if (nrow(x$laczenia[[1]]$laczenia) == 0) {
    return(egzamin)
  }
  x = x$laczenia[[1]]
  # rysowanie
  temp = melt(x$dyskryminacje, na.rm = TRUE)
  names(temp) = c("laczenie", "kryterium", "b")
  temp = within(temp, {korelacja = c(NA, x$laczenia$korelacja)[get("laczenie") + 1]})
  temp$laczenie =
    factor(temp$laczenie, levels = 0:nrow(x$laczenia),
           labels = c("bez łączenia",
                      with(x$laczenia, paste0(1:nrow(x$laczenia), ". ",
                                              kryterium, " <- ", kryterium2,
                                              "\nkor = ", round(korelacja, 3)))))
  wykres = ggplot(temp, aes(x = get("laczenie"), y = get("b"))) +
    geom_violin(adjust = 0.25, aes(fill = korelacja), alpha = 0.9) +
    geom_boxplot(alpha = 0, colour = "red", outlier.colour = "red") +
    scale_fill_continuous(limits = c(0.5, 1), high = "#132B43", low = "#56B1F7") +
    labs(title = tytul, x = "łączenie", y = "dyskryminacja") +
    coord_flip() +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.major = element_line(colour = "#404040", linetype = "dashed"),
          panel.grid.minor = element_line(colour = "#404040", linetype = "dotted"),
          axis.text.x = element_text(color = "#303030", size = rel(1.2)),
          axis.text.y = element_text(color = "#303030", size = rel(1.2)),
          text = element_text(size = wielkoscTekstu))
  plot(wykres)
  # przygotowywanie obiektów 'elementy' dla edytuj_skale()
  elementy = vector(mode = "list", length = nrow(x$dyskryminacje))
  elementy[[1]] =
    data.frame(id_kryterium = as.numeric(sub("^[kp]_", "",
                                             colnames(x$dyskryminacje))),
               polaczone = NA , id_pseudokryterium = NA,
               opis = NA, id_skrotu = NA)
  for (i in 2:nrow(x$dyskryminacje)) {
    laczoneKryteria = as.numeric(sub("^[kp]_", "",
                                     c(x$laczenia$kryterium[i - 1],
                                       x$laczenia$kryterium2[i - 1])))
    elementy[[i]] = subset(elementy[[i - 1]],
                           !(get("id_kryterium") %in% laczoneKryteria))
    maska = unlist(lapply(elementy[[i]]$polaczone,
                          function(x, y) {return(any(x %in% y))},
                          y = laczoneKryteria))
    if (any(maska)) {
      elementy[[i]]$polaczone[maska][[1]] =
        sort(unique(c(elementy[[i]]$polaczone[maska][[1]],
                      laczoneKryteria)))
    } else {
      polaczone = data.frame(polaczone = NA)
      polaczone$polaczone[1] = list(laczoneKryteria)
      elementy[[i]] = bind_rows(elementy[[i]], polaczone)
    }
  }
  elementy = lapply(elementy[-1], function(x, czescEgzaminu) {
    maska = unlist(lapply(x$polaczone, is.null))
    polaczone = bind_rows(lapply(x$polaczone[!maska], function(x) {
      return(data.frame(matrix(x, nrow = 1)))
    }))
    names(polaczone) = paste0("id_kryterium_", 1:ncol(polaczone))
    polaczone = cbind(polaczone,
                      opis = unlist(lapply(x$polaczone[!maska],
                                           paste0, collapse = ";")),
                      stringsAsFactors = FALSE)
    polaczone$opis = paste0(czescEgzaminu, ";", polaczone$opis)
    x = bind_rows(select_(x[maska, ], ~-polaczone), polaczone)
    maska = grepl("^id_kryterium", names(x))
    x = x[, c(names(x)[maska], names(x)[!maska])]
    return(x)
  }, czescEgzaminu = egzamin$czesc_egzaminu)
  # zwracanie
  x = egzamin
  x$wykres[1] = list(wykres)
  x$elementy[1] = list(elementy)
  return(x)
}