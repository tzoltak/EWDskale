#' @title Funkcja skalujaca i łącząca wiele części egzaminów
#' @description
#' Funkcja zapisuje do wskazanego folderu wyniki skalowania  
#' @param plik nazwa pliku csv zawierający tabelę z wynikami części egzaminu.
#' @param folderDane ścieżka do folderu, który zawiera plik, którego nazwa jest wskazana parametrem 'plik'. 
#' @param folderWyniki ścieżka do folderu, gdzie zostanie zapisany wynik skalowania i łączenia.
#' @param folderWykresy ścieżka do folderu, gdzie zostaną zapisane wykresy wyników skalowania.
#' @param odswiez zmienna boolowska określająca, czy należy powtórzyć obliczenia, jeżeli w katalogu znajduje się plik o nazwie wyniku algorytmu. 
#' @param czy2Konstr zmienna boolowska. Jeżeli wynosi TRUE to łączenie będzie odbywać się na podstawie wartości drugiego konstruktu.
#' W przeciwnym wypadku wybór kryteriów do łączenia będzie odbywał się na podstawie wartości korelacji polichorycznych.
#' @details
#' Funkcja do folderu folderWyniki zapisuje rezultat działania funkcji skaluj_laczenie_mirt lub skaluj_polichorycznie.
#' Dodatkowo do folderWykresy zapisuje też histogramy i wykresy gęstości. 
#' Nazwa pliku z wynikami skalowań i łączeń powstaje przez dodanie '.rkp' (korelacje polichoryczne) lub '.r2k' (dwa konstrukty)
#' do parametru plik. W sytuacjach egzaminu, który składa się z wielu części przez rozszerzeniem pliku doklejane są nazwy konstruktów,
#' które dla tego skalowania nie przyjmója wartości NA. Ma to miejsce na przykład dla WOS'u oraz j.polskiegod.
#' @return
#' Funkcja nic nie zwraca.
#' @export
skalowanieWieluCzesci <- function(plik, folderDane, folderWyniki, 
                                      folderWykresy, odswiez = FALSE, czy2Konstr = TRUE){

  zapiszLinie <- function(linia, sciezka){
#     Sys.sleep(0.25*runif(1))
#     plik = file(sciezka, "at")
#     message(paste0(linia, collapse=","))
#     writeLines(paste0(linia, collapse=","), plik)
#     close(plik)
    invisible(NULL)
  }
  
  message(paste0("Start obliczeń dla pliku: ", plik,"."))
  
  path = "/home/g.golonka/daneEgzaminy/laczenie/skalowanieDanychMirt.log"
  zapiszLinie(paste0("Start obliczeń dla pliku: ", plik), path)
  
  if(!grepl(".csv", plik)){
    return(FALSE)
  }

  sciezkaPliku = paste0(folderWyniki, plik)

  #Nie pozwalamy na spacje oraz myślniki w nazwach plików. Latex pozniej z tym sobie nie poradzi.
  if(czy2Konstr){
    plikDoZapisu = paste0(gsub("[ |-]+","_",plik), ".r2k")
  }else{
    plikDoZapisu = paste0(gsub("[ |-]+","_",plik), ".rkp")
  }
  
  obliczone = list.files(folderWyniki) 
  
  if(!odswiez){
    # sprawdzamy czy są pliki, które zaczyną się nazwą jak plik do zapisu.
    if(any(grepl(paste0("^", gsub(".r2k$|.rkp$", "", plikDoZapisu)), obliczone))){
      zapiszLinie(paste0("Koniec obliczeń dla pliku: ", plik, "(obliczenia wykonane wcześniej)."), path)
      return(TRUE)
    }
  }
  
  daneOrg = read.csv2(paste0(folderDane, plik))
  
  if("laureat" %in% names(daneOrg)){
    daneOrg = daneOrg[ !(!is.na(daneOrg$laureat) & daneOrg$laureat == 1), ]
  }
  
  if(nrow(daneOrg)==0){
    zapiszLinie(paste0("Koniec obliczeń dla pliku: ", plik, "(zero wierszy)."), path)
    return(FALSE)
  }
  
  maskiZmienne = grepl("^([[:alnum:]]+_)+[[:digit:]]+", names(daneOrg))
  dane = daneOrg[, maskiZmienne]
  
  # co robić z wynikami postaci 0.5 i z NA ?
  del = NULL
  for(ic in 1: ncol(dane)){
    if( all(is.na(dane[, ic])) ){
      del = c(del, ic)
    }
    
    if( max(dane[,ic], na.rm = TRUE) > 19){
      del = c(del, ic)
    }
    
  }
  if(!is.null(del)){
    dane = dane[, -del]
  }
  
  if(nrow(dane)==0){
    zapiszLinie(paste0("Koniec obliczeń dla pliku: ", plik, "(zero poprawnych wierszy)."), path)
    return(FALSE)
  }
  
  for(ic in 1: ncol(dane)){
    if(!any(!is.na(dane[, ic]) & round(dane[, ic])!=dane[, ic]  )){
      next
    }
    
    message("W pliku: ", plik," jest kryterium(", names(dane)[ic],") o wartościach 0.5.")
    dane[, ic] = as.numeric(factor(dane[, ic])) - 1
  }
  
  ## skalowanie dla  wypracowań dla plików WOS i j.polski
  if(grepl("WOS|polski",plik)){
    wspIWypr = detekcja_wypracowan(dane)
    
    for(iList in seq_along(wspIWypr$wypracowania)){
      indKolumny = c(wspIWypr$wspolne, wspIWypr$wypracowania[[iList]])
      daneSkalowanie = dane[!is.na( dane[, wspIWypr$wypracowania[[iList]][1]] ), indKolumny]
      
      if(czy2Konstr){
        plikDoZapisuMod = paste0(gsub(".r2k", "", plikDoZapisu), "_",
                                 paste(names(dane)[wspIWypr$wypracowania[[iList]]], collapse = "_"),".r2k")
      } else{
        plikDoZapisuMod = paste0(gsub(".rkp", "", plikDoZapisu), "_",
                                 paste(names(dane)[wspIWypr$wypracowania[[iList]]], collapse = "_"),".rkp")
      }
      
      laczenie_wykresy_zapis(daneSkalowanie, plikDoZapisuMod, folderWykresy, folderWyniki, czy2Konstr)
    }
    
    # zapisać dla zasady
    dwaKonstruktyRet = NULL
    # save(dwaKonstruktyRet, file = paste0(folderWyniki, plikDoZapisu))
    
  } else{
    laczenie_wykresy_zapis(dane, plikDoZapisu, folderWykresy, folderWyniki, czy2Konstr)
  }
  return(TRUE)
}
#' @title Detekcja wypracowań
#' @description
#' Funkcja zwraca listę, której pierwszy element 'wspolne' zawiera indeksy kolumn, 
#' które nie zawierają braków danych. Z kolei drugi element 'wypracowania' zawiera listę indeksów kolumn,
#' dla których braki danych występują dla tych samych obserwacji (wierszów).
#' @param dane ramka danych z wynikami.
#' @return
#' Lista.
detekcja_wypracowan <- function(dane)
{
  vec = apply(dane, 2, function(x) sum(is.na(x)))
  nrWspolnych = which(vec==0)
  nrWypracowan = which(vec!=0)
  
  oznaczone = NULL
  wypracowania = list()
  for(ind in seq_along(nrWypracowan)){
    
    if(nrWypracowan[ind] %in% oznaczone ){
      next
    }
    
    vecPart = apply(dane[!is.na(dane[,nrWypracowan[ind]]), ], 2, function(x) sum(is.na(x)))
    nrWypracowanPart = which(vecPart!=0)
    
    wypracowania[[length(wypracowania)+1]] = nrWypracowan[!nrWypracowan %in% nrWypracowanPart]
    oznaczone = c(oznaczone, nrWypracowan[!nrWypracowan %in% nrWypracowanPart])
  }
  
  ret = list(wspolne = nrWspolnych, wypracowania = wypracowania)
  return(ret)
}
#' @title Łączenie oraz zapis wykresów do plików
#' @description
#' Funkcja wykonuje łączenie, skalowania oraz zapisuje wyniki do plików. 
#' @param dane ramka danych z wynikami części egzaminu.
#' @param plikDoZapisu nazwa pliku, która posłuży do zbudowania nazw plików z wynikami skalowania
#' oraz plików png wykresów.
#' @param folderWykresy folder zapisu wykresów.
#' @param folderWyniki folder zapisu wyników skalowania.
#' @param czy2Konstr zmienna boolowska. Jeżeli wynosi TRUE to łączenie będzie odbywać się na podstawie wartości drugiego konstruktu.
#' @return
#' Funkcja nic nie zwraca.
laczenie_wykresy_zapis <- function(dane, plikDoZapisu, folderWykresy, folderWyniki, czy2Konstr){
  
  rysunki_skalowanie_mirt <-function(obj_podsum, dirPath, nazwaPliku){
    for(ind in 1:length(obj_podsum$theta)){
      theta = obj_podsum$theta[[ind]]
      
      dd = theta[, ncol(theta)]
      png(filename = paste0(dirPath, nazwaPliku,"_theta_", ind, ".png"), 
          width = 6, height = 6, units = 'in', res = 500)
      plot(density(dd), main = paste0("Iteracja ", ind), xlab="", ylab="")
      dev.off()
      
      png(filename = paste0(dirPath, nazwaPliku,"_theta_hist_", ind, ".png"), 
          width = 6, height = 6, units = 'in', res = 500)
      hist(dd, breaks = seq(min(dd),max(dd),length.out = 1223), main = paste0("Iteracja ", ind), 
           ylab = "", xlab="")
      dev.off()
    }
  }
  
  kryteria = as.numeric(gsub("^[[:alnum:]]+_", "", names(dane)))
  wiazki_pyt_kryt = pobierz_wiazki_pytania_kryteria(kryteria)
  
  if(czy2Konstr){
    dwaKonstruktyRet = skaluj_laczenie_mirt(dane, wiazki_pyt_kryt, prog = 0.3, 
                                            h2 = FALSE, czyLiczyc1 = TRUE)
  } else{
    dwaKonstruktyRet = skaluj_polichorycznie(dane, proceduraEG = NULL, 
                                             korelacjaWiazki = NULL, plikDoZapisu, 
                                             minMiara = 0.5)
  }
  
  if( length(dwaKonstruktyRet$wyniki) != 0 ){
    parametry = przygotuj_parametry_IRT(dwaKonstruktyRet)
    dwaKonstruktyRet$parametry = parametry
    
    rysunki_skalowanie_mirt(parametry, folderWykresy, plikDoZapisu)
  }
  
  save(dwaKonstruktyRet, file = paste0(folderWyniki, plikDoZapisu))
  return(invisible(TRUE))
}
#' @title Łączenie poziomów odpowiedzi
#' @description
#' Funkcja łączy najmniej liczny poziom z mniej licznym z poziomów sąsiednich.
#' Funkcja wykonuje łączenie do momentu osiągnięcia zadanej liczby poziomów.
#' @param vec ramka danych z wynikami części egzaminu.
#' @param poziomy liczba określająca, ile poziomów ma być w wynikowym wektorze.
#' @return
#' Funkcja zwraca factor o liczbie poziomów o jeden miejszą niż parametr 'vec'. 
polacz_nieliczne <- function(vec, poziomy = 5){
  
  polacz_nieliczny <- function(vec){
    vec = as.numeric(factor(vec)) - 1 
    tab = table(vec)
    len = length(tab)
    
    toPol1 = which.min(tab)
    
    toPol2 = NULL
    if( toPol1 - 1 > 0 ){
      toPol2 = toPol1 - 1
    }
    
    toPol2 = if( toPol1 + 1 <= length(tab)  & (is.null(toPol2) || tab[toPol1 + 1] < tab[toPol2])){
      toPol1 + 1
    } else{
      toPol2
    }
    
    ret = vec
    ret[!is.na(ret) & ret==names(tab)[toPol1]] = as.numeric(names(tab)[toPol2])
    return(ret)
  }
  
  n = length(table(vec))
  while(n > poziomy){
    vec = polacz_nieliczny(vec)
    n = n-1
  }
  return(vec)
}

