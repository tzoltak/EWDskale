rm(list=ls())
library(EWDskalowanie)
library(mirt)
library(snow)

folderDane = "/home/g.golonka/daneEgzaminy/"
folderWyniki = "/home/g.golonka/daneEgzaminy/laczenie/dwaKonstrukty/"
folderWykresy = "/home/g.golonka/daneEgzaminy/laczenie/dwaKonstrukty/wykresy/"

danePliki = list.files(folderDane, pattern =".csv$")

cl <- makeCluster(12, type = "SOCK")
parLapply(cl, danePliki[ sample(seq_along(danePliki)) ], EWDskalowanie::skalowanieWieluCzesci, 
          folderDane = folderDane, 
          folderWyniki = folderWyniki, 
          folderWykresy = folderWykresy)
stopCluster(cl)