
require(mirt)
require(EWDskalowanie)
sink("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Log/mirtTempLaczenie.log", split=TRUE)

# ścieżka do danych
daneOrg = read.csv2("/home/g.golonka/gimnazjum_dane/EG2008.csv")
dane_zg = daneOrg
# dane_zg = daneOrg[1:5000,]
names(dane_zg)[names(dane_zg)=="id_obserwacji"] = "id_obs"
maskiZmienne_zg = grepl("^(g[h]_)[[:digit:]]{1,4}$", names(dane_zg))
dane_zg = dane_zg[, maskiZmienne_zg]
kryteria_zg = as.numeric(gsub("^[[:alnum:]]+_", "", names(dane_zg)))
wiazki_pyt_kryt_zg = pobierz_wiazki_pytania_kryteria(kryteria_zg)

mirtWyniki14 = skaluj_laczenie_mirt(dane_zg, wiazki_pyt_kryt_zg, prog=NULL, h2=FALSE, 
                                    czyLiczyc1 = TRUE, maxIter = 14)
save(mirtWyniki14, file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki14_BIC")

mirtWyniki03 = skaluj_laczenie_mirt(dane_zg, wiazki_pyt_kryt_zg, prog=0.3, h2=FALSE, 
                                    czyLiczyc1 = TRUE)
save(mirtWyniki03, file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki03_BIC")
mirtWyniki_h2_14 = skaluj_laczenie_mirt(dane_zg, wiazki_pyt_kryt_zg, prog=NULL, h2=TRUE,
                                        czyLiczyc1 = TRUE, maxIter = 14)
save(mirtWyniki_h2_14, file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_14_BIC")

mirtWyniki_h2_07 = skaluj_laczenie_mirt(dane_zg, wiazki_pyt_kryt_zg, prog=0.7, h2=TRUE, 
                                        czyLiczyc1 = TRUE)
save(mirtWyniki_h2_07, file = "/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki_h2_07_BIC")

cat("Koniec.")

# library(plyr)
# 
# load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/mirtWyniki14_BIC")
# 
# 
# names(mirtWyniki14) 
# (mirtWyniki14$wyniki) 
# 
# mirtWyniki14$polaczenia
# 
# model = mirtWyniki14$wyniki[[1]]
# theta = fscores(model, full.scores=TRUE, method="EAP")
# 
# param2PL = rbind.fill(lapply(coef(model)[names(coef(model)) != "GroupPars"], as.data.frame))
# 
# qqnorm(param2PL$a1)
# 
# dd = theta[, ncol(theta)]
# hist(dd,breaks= seq(min(dd)-0.01, max(dd)+0.01,length.out=1000))
# plot(density(dd))






