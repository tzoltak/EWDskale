# load(file="/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/skWynik2008")
load("/home/g.golonka/EWDgit/EWDskalowanie/Skrypty/Wyniki/skWynik2008_test")


skWynik2008$korelacjaWiazki

numerSkalowania = 2
names(skWynik2008$skalowania)[numerSkalowania]
wynikiSkalowania = skWynik2008$skalowania[[numerSkalowania]]$kalibracja1

pp = wynikiSkalowania$parametry$surowe
qqnorm(pp$wartosc[pp$typ=="threshold"])
qqnorm(pp$wartosc[pp$typ=="by"])

dd = wynikiSkalowania$zapis$gh
hist(dd,breaks= seq(min(dd)-0.01, max(dd)+0.01,length.out=1000))
plot(density(dd))










