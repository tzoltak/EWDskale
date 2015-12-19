#' @title Oszacowania z modeli Rascha
#' @description
#' Funkcja przygotowuje przeliczenie z sumy punktów na przewidywanie poziomu
#' umiejętności z modelu IRT (żeby miało to większy sens powinna to być jakaś
#' odmiana modelu Rascha).
#' @param sumy data frame zawierający id_obserwacji i wyliczone sumy punktów
#' (może to być kilka kolumn z sumami z kilku różnych części egzaminu)
#' @param oszacowania data frame zawierający id_obserwacji i oszacowania
#' umiejętności z modelu
#' @param maks opcjonalnie wektor liczb zawierających maksymalne możliwe do
#' uzyskania wartości sum - jego elementy muszą nazywać się tak, jak kolumny
#' argumentu \code{sumy}, opisujące wartości sum punktów
#' @param span wartość liczbowa - parametr \code{span}, przekazywany do funkcji
#' \code{loess()} przy wygładzaniu i zapełnianiu dziur w przebiegu przekodowania
#' @details
#' W modelu Rascha liczba punktów w zasadzie powinna być statystyką dostateczną
#' dla oszacowania poziomu umiejętności bez względu na to, czy test składa się
#' wyłącznie z zadań ocenianych binarnie, czy też zawiera również zadania
#' o większej liczbie poziomów wykonania. Niestety, w Mplusie tak nie jest
#' i jeśli test zawiera zadania o większe liczbie poziomów wykonania, Mplus
#' będzie przypisywał zdającym nieco inne oszacowania w zależności od tego,
#' które konkretnie zadania rozwiązali poprawnie, a które niepoprawnie.
#'
#' Jeśli napodstawie wyników działania Mplusa chcemy uzyskać funkcję
#' przypisującą jednoznacznie oszacowanie sumie punktów, musimy w związku z tym
#' dokonać jakiegoś przybliżenia. Możliwe są dwa podejścia:
#' \itemize{
#'   \item{Wygenerować wszystkie możliwe wektory odpowiedzi, na podstawie
#'         wyestymowanych parametrów modelu wyliczyć odpowiadające im
#'         oszacowania oraz ich wiarygodność, a następnie agregować w ramach
#'         grup wyróżnionych ze względu na sumę punktów, z użyciem średniej
#'         ważonej wiarygodnością profilu (a więc wartością proporcjonalną do
#'         teoretycznej częśtości jego występowania).}
#'   \item{Użyć oszacowań wyliczonych na bardzo dużym zbiorze danych
#'         (rzeczywistym lub wygenerowanym metodą Monte-Carlo) i wyliczyć
#'         średnie (już nieważone) oszacowań w ramach grup o tej samej wartości
#'         sumy.}
#' }
#' Podejście pierwsze jest teoretycznie bardziej poprawne, ale znajduje
#' zastosowanie tylko dla dosyć krótkich testów, jako że liczba wszystkich
#' możliwych wektorów odpowiedzi rośnie wykładniczo wraz ze wzrostem liczby
#' zadań. Podejście drugie jest aplikowalne zawsze, ale jego sensowność silnie
#' zależy od iczby losowanych wyników (procedura Monte-Carlo) lub liczby
#' obserwacji i rozkładu wyników (wykorzystanie danych rzeczywistych).
#'
#' Niniejsza funkcja implementuje drugą procedurą, w oparciu o dane rzeczywiste
#' (czyli w zasadzie najbardziej ryzykowną). Stara się przy tym wykryć
#' i naprawić pewne problemy, które mogą się z nią wiązać:
#' \itemize{
#'   \item{niemonotoniczność przewidywań,}
#'   \item{dziury w rozkładzie sumy.}
#' }
#' Rozwiązanie obu problemów stara się uzyskać wygładzając i interpoulując
#' przewidywanie przy pomocy regresji nieparametrycznej (funkcją \code{loess()},
#' wywoływana z parametrem \code{span}). Jeśli przewidywanie wynikające
#' z regesji nieparametrycznej jest niemonotoniczne, funkcja będzie zwiększać
#' wartość parametru span o 0.05 tak długo, aż przewidywanie stanie się
#' monotoniczne (wtedy zwórcone zostanie ostrzeżenie) lub aż osiągnieta zostanie
#' wartość parametru \code{span} większa lub równa 1 (jeśli przewidywanie cały
#' czas będzie niemonotonicze, funkcja zwróci błąd).
#'
#' Jeśli minimalna wartość występująca w danych nie jest równa 0, to wartościom
#' niższym od najmniejszej występującej (w ramach danej grupy) przypisane
#' zostanie przewidywanie odpowiadające tej minimalnej wartości sumy,
#' występującej w danych (w ramach danej grupy).
#'
#' Analogicznie, jeśli podana została wartość parametru \code{maks}, to w ramach
#' każdej grupy ew. wartościom większym od maksymalnej wartości występującej
#' w danych (w ramach danej grupy) a nie większym od odpowiedniej wartości
#' parametru \code{maks} przypisane zostaną przewidywania odpowiadające tej
#' maksymalnej wartości sumy, występującej w danych (w ramach danej grupy).
#' @return lista o elementach:
#' \itemize{
#'   \item{\code{mapowanie} Data frame zawierający mapowanie sum na średnie
#'         oszacowania. Kolumny o nazwach takich, jak kolumny w argumencie
#'         \code{sumy} (nie przechowujące id obserwacji) przyjmują wartość
#'         \code{TRUE} jeśli dany wiersz dotyczy posiadających wynik
#'         z odpowiedniej części egzaminu lub wartość \code{FALSE}, jeśli dany
#'         wiersz dotyczy zdających, którzy nie posiadają wyniku z odpowiedniej
#'         części egzaminu. Kolumna \code{suma} opisuje sumę punktów łącznie
#'         z wszystkich części (z których ktoś posiada wyniki), a ostatnia
#'         kolumna zawiera wartość przewidywania.}
#'   \item{\code{przewidywania} Data frame zawierający zastosowanie ww.
#'         mapowania do danych, z którymi wywołana została funkcja.}
#'   \item{\code{odsUtraconejWariancji} Odsetek wariancji oszacowań cechy
#'         utracony w wyniku uśredniania.}
#' }
#' @import dplyr
#' @export
przewidywanie_rasch = function(sumy, oszacowania, maks = NULL, span = 0.2) {
  stopifnot(is.data.frame(sumy), is.data.frame(oszacowania),
            is.numeric(maks) | is.null(maks),
            is.numeric(span), length(span) == 1)
  stopifnot(all(span > 0))
  stopifnot(length(intersect(names(sumy), names(oszacowania))) > 0)
  stopifnot(ncol(oszacowania) == (length(intersect(names(sumy),
                                                   names(oszacowania))) + 1))
  stopifnot(!("suma" %in% names(sumy)), !("suma" %in% names(oszacowania)),
            !("oszacowania" %in% names(oszacowania)))

  zmId = intersect(names(sumy), names(oszacowania))
  zmSumy = setdiff(names(sumy), names(oszacowania))
  zmOszacowania = setdiff(names(oszacowania), names(sumy))
  stopifnot(all(names(maks) %in% zmSumy))

  vO = cov.wt(as.matrix(oszacowania[, zmOszacowania]), method = "ML")$cov[1, 1]
  # zamiana oddzielnych sum na łączną oraz oznaczenie, które części pisał
  sumy = cbind(sumy[, zmId, drop = FALSE],
               as.data.frame(lapply(sumy[, zmSumy, drop = FALSE], is.na)),
               suma = rowSums(sumy[, zmSumy, drop = FALSE], na.rm = TRUE))
  sumy[, zmSumy] = !sumy[, zmSumy]
  # łączenie z oszacowaniami i uśrednianie
  oszacowania = suppressMessages(inner_join(sumy, oszacowania))
  names(oszacowania) = sub(paste0("^", zmOszacowania, "$"), "oszacowania",
                           names(oszacowania))
  temp = group_by_(oszacowania,
                   .dots = lapply(as.list(paste0("~", c(zmSumy, "suma"))),
                                  formula)) %>%
    summarize_(.dots = setNames(list(~mean(oszacowania)), "oszacowania"))
  # dopisywanie maksów, żeby można z nich było potem skorzystać
  # zawikłane: dla każdej z grup zdających, wyróżnionych ze względu na zestaw
  # zdawanych części egzaminu, wyliczamy, ile mogli uzyskać maksymalnie, łacznie
  # ze wszystkich części, które pisali
  grupy = cbind(distinct(temp[, zmSumy]), maks = NA)
  for (i in 1:nrow(grupy)) {
    grupy$maks[1] =
      sum(maks[names(grupy)[c(unlist(grupy[i, -ncol(grupy)]), FALSE)]])
  }
  temp = suppressMessages(left_join(temp, grupy))
  # łatanie i wygładzanie
  mapowanie = vector(mode = "list", length = nrow(grupy))
  for (i in 1:nrow(grupy)) {
    x = suppressMessages(semi_join(temp, grupy[i, zmSumy, drop = FALSE]))
    if (nrow(x) == 1) {
      stop("W danych występuje grupa z tylko jednym wynikiem sumarycznym.")
    } else if (nrow(x) < 5 ) {
      warning("W danych występuje grupa z mniej niż 5 różnymi wynikami sumarycznymi.",
              immediate. = TRUE)
    }
    monot = FALSE
    spanTemp = span
    while (!monot) {
      if ( (span > 1) & (spanTemp != span)) {
        stop("Nie udało się uzyskać monotonicznego przewidywania.")
      }
      l = loess(oszacowania ~ suma, x, span = spanTemp)
      suma = seq(min(x$suma), max(x$suma))
      mapowanie[[i]] = data.frame(suma, p = predict(l, suma))
      monot = with(mapowanie[[i]], all(p[-1] >= p[-length(p)]))
      if (!monot) {
        spanTemp = spanTemp + 0.05
      }
    }
    if (spanTemp != span) {
      warning("Wartość parametru 'span' została zwiększona do ", spanTemp,
              ", aby uzyskać monotoniczne przewidywanie.", immediate. = TRUE)
    }
    # wypełnianie braków od 0
    if (min(mapowanie[[i]]$suma) > 0) {
      mapowanie[[i]] =
        rbind(data.frame(suma = 0:(min(mapowanie[[i]]$suma) - 1),
                         p = mapowanie[[i]]$p[1]),
              mapowanie[[i]])
    }
    # i do maksa
    if (max(mapowanie[[i]]$suma) < x$maks[1] & !is.na(x$maks[1])) {
      mapowanie[[i]] =
        rbind(mapowanie[[i]],
              data.frame(suma = (max(mapowanie[[i]]$suma) + 1):x$maks[1],
                         p = mapowanie[[i]]$p[nrow(mapowanie[[i]])]))
    }
    names(mapowanie[[i]]) = sub("^p$", zmOszacowania, names(mapowanie[[i]]))
    # brzydka sztuczka na łączenie
    mapowanie[[i]] = suppressMessages(
      left_join(cbind(grupy[i, zmSumy, drop = FALSE], nic = 1),
                cbind(mapowanie[[i]], nic = 1)) %>%
        select_(~-nic)
    )
  }
  mapowanie = bind_rows(mapowanie)
  rm(temp, x)
  # zastępowanie oszacowań wartościami zmapowanymi
  oszacowania = suppressMessages(
    inner_join(oszacowania[, setdiff(names(oszacowania), "oszacowania")],
               mapowanie))
  oszacowania = oszacowania[, c(zmId, zmOszacowania)]
  # oszacujmy, ile na tym uśrednianiu tracimy wariancji
  vP = cov.wt(as.matrix(oszacowania[, zmOszacowania]), method = "ML")$cov[1, 1]
  odsUtraconejWariancji = 1 - vP / vO
  message("W wyniku uśredniania utracono ",
          100 * round(odsUtraconejWariancji, 4),
          "% wariancji oszacowań umiejętności.")
  # koniec
  return(list(
    mapowanie = mapowanie,
    oszacowania = oszacowania,
    odsUtraconejWariancji = odsUtraconejWariancji))
}
