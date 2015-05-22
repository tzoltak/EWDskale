rm(list=ls())
library(snow)
library(EWDskalowanie)

folderDane = "/home/g.golonka/daneEgzaminy/"
folderWyniki = "/home/g.golonka/daneEgzaminy/laczenie/korelacjePoli2/"
folderWykresy = "/home/g.golonka/daneEgzaminy/laczenie/korelacjePoli2/wykresy/"

danePliki = list.files(folderDane, pattern =".csv$")

cl <- makeCluster(12, type = "SOCK")
parLapply(cl, danePliki[ sample(seq_along(danePliki)) ], EWDskalowanie::skalowanieWieluCzesci, 
          folderDane = folderDane, 
          folderWyniki = folderWyniki, 
          folderWykresy = folderWykresy, czy2Konstr = FALSE)
stopCluster(cl)  

# wersja jednowątkowa:
#
# plik = danePliki[ sample(seq_along(danePliki)) ]
# 
# for(plik in  danePliki[ sample(seq_along(danePliki)) ] ){
#   skalowanieWieluCzesci(plik,folderDane = folderDane, 
#                         folderWyniki = folderWyniki, 
#                         folderWykresy = folderWykresy, czy2Konstr = FALSE, odswiez = FALSE)
# }






