options(encoding = "UTF-8")
rm(list=ls())
library(ZPD)
library(foreign)
library(polycor)
library(RODBCext)
library(EWDskalowanie)
# sink("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Log/skalowanie_polichoryczne.log", split=TRUE)

# ścieżka do danych
daneOrg = read.csv2("/home/g.golonka/gimnazjum_dane/EG2008.csv")

dane = daneOrg # [1:15000, ]
names(dane)[names(dane)=="id_obserwacji"] = "id_obs"
maskiZmienne = grepl("^(g[h]_)[[:digit:]]{1,4}$|^id_obs$", names(dane))
dane = dane[, maskiZmienne]

daneKor = dane[,grepl("^(gh_)[[:digit:]]+", names(dane))]
kryteria = as.numeric(gsub("^[[:alnum:]]+_", "", names(daneKor)))

wiazki_pyt_kryt = pobierz_wiazki_pytania_kryteria(kryteria)
korelacjaWiazki = policz_korelacje_wiazki(daneKor, wiazki_pyt_kryt)

# skalowanie z użyciem korelacji polichorycznych do łączenia kryteriów.
proceduraEG = procedura_eg_hum(names(dane), parametryGH = NULL, processors = 6)
nazwaSkalowania = "EG_2008_hum"

skWynik2008 = skaluj_polichorycznie(dane, proceduraEG, korelacjaWiazki, "EG_2008_hum_2", ileKrokow = 14)
save(skWynik2008, file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/skWynik2008_pol")

cat("Koniec. \n")

