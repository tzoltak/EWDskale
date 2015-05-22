rm(list=ls())
library(ZPD)
library(snow)

pobierzIndeks <- function(ind, czesciEG, sciezkaKatalogu, odswiez = FALSE){
  require(ZPD)
  
  czescEgzaminu = as.character(czesciEG$czesc_egzaminu[ind])
  rodzajEgzaminu = as.character(czesciEG$rodzaj_egzaminu[ind])
  rokEgzaminu = czesciEG$rok[ind]
  
  nazwaPliku = paste0(sub(" ","_", rodzajEgzaminu), "_", rokEgzaminu , "_", czescEgzaminu, ".csv")
  sciezkaPliku = paste0(sciezkaKatalogu, nazwaPliku)
  
  pobranePliki = list.files(sciezkaKatalogu) 
  # jeżeli plik istnieje i zawiera wiersze to funkcja przerywa działanie.
  if( !odswiez && nazwaPliku %in% pobranePliki){
    nRow = nrow(read.csv2(sciezkaPliku, na=""))
    if(nRow > 0){
      return(TRUE)
    }
  }
  
  dane = try(pobierz_czesc_egzaminu(rodzajEgzaminu, czescEgzaminu, rokEgzaminu, czyEwd=TRUE, zrodloDanychODBC="EWD"), silent=TRUE)
  if ("try-error"%in%class(dane)) {
    return(FALSE);
  }
  
  write.csv2(dane, sciezkaPliku, row.names = FALSE, na="")
  return(TRUE)
}

###################################################################################################

zrodloDanychODBC = "EWD"
baza = odbcConnect(zrodloDanychODBC)
zapytanie="SELECT DISTINCT czesc_egzaminu, rodzaj_egzaminu, prefiks, extract(year from data_egzaminu) as rok
            FROM sl_egzaminy 
            JOIN sl_czesci_egzaminow USING (rodzaj_egzaminu, czesc_egzaminu)
            JOIN  arkusze USING (rodzaj_egzaminu, czesc_egzaminu, data_egzaminu)
            JOIN  testy USING (arkusz)
            WHERE ewd = true
            ORDER BY rodzaj_egzaminu, rok"
czesciEG  = sqlQuery(baza, zapytanie)
odbcClose(baza)

sciezkaKatalogu = "/home/g.golonka/daneEgzaminy/"
pobranePliki = list.files(sciezkaKatalogu)

cl <- makeCluster(rep("localhost", 4), type = "SOCK")
parLapply(cl, 1:nrow(czesciEG), pobierzIndeks, 
          czesciEG = czesciEG, 
          sciezkaKatalogu = sciezkaKatalogu, odswiez = FALSE)
stopCluster(cl)




######################Raport z pobierania##########################################

sciezkaKatalogu = "/home/g.golonka/daneEgzaminy/"
pobranePliki = list.files(sciezkaKatalogu, pattern =".csv$")

ret = NULL
for(iPlik in seq_along(pobranePliki)){
  sciezkaPliku = paste0(sciezkaKatalogu, pobranePliki[iPlik])
  nRow = nrow(read.csv2(sciezkaPliku, na=""))
  
  ret = rbind(ret, c(pobranePliki[iPlik], nRow))
}
colnames(ret) <- c("Plik", "Liczba wierszy") 
ret
write.csv2(ret, paste0("/home/g.golonka/daneEgzaminy/laczenie/", "RaportPobieranie.txt"), row.names = FALSE, na="")



# ret[order(ret[, 1]), ]


# cs <- 2
# class(cs) <- "ClearScreen"
# 
# print.ClearScreen <- function(x){
#   cat("\14")  
# }



















