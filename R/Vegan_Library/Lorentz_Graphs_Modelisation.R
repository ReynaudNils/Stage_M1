#Species
getwd()
setwd("C:/Users/nilsr")
library(vegan)
LorentzS <- read.csv('Lorentz_Species.csv')
LorentzS
rowSums(is.na(LorentzS))
L <- LorentzS[[2]]
L
length(L)
plot(L,xlab="Cumulative Number of Species from 100% to 0",ylab="Cumulative Number of Images")
title(main = "Lorentz Graph Species")

#Modélisation
LorentzSpecies <- radfit(L)
GLactuca
plot(GLactuca)
#radfit ne fonctionne pas sur l'ensemble des espèces
rad <- radfit(L[1:50])
rad2 <- radfit(L[50:200])
rad3 <- radfit(L[200:1081])
rad
plot(rad)
rad2
plot(rad2)
rad3
plot(rad3)
#Le problème vient de zipfbrot
R5 <- rad.zipfbrot(L)
#sur les premières espèces cela donne quelque chose
rad4 <- rad.zipfbrot(L[1:50])
rad4
plot(rad4)
rad5 <- rad.zipfbrot(L[50:200])
rad5
plot(rad5)
rad6 <- rad.zipfbrot(L[200:1081])
rad6
plot(rad6)
#Si on s'intéresse aux autre modèles, pas de souci
plot(L,xlab="Cumulative Number of Species",ylab="Cumulative Number of Images")
title(main = "Lorentz Graph Species, Fitting")
R <-rad.lognormal(L)
R
lines(R, col='purple')
R2 <- rad.null(L)
R2
lines(R2, col='red')
R3 <- rad.preempt(L)
R3
lines(R3, col='blue')
R4 <- rad.zipf(L)
R4
lines(R4, col='green')
R5 <- rad.zipfbrot(L)
legend("topright", legend=c("Log-Normal", "Brokenstick", "Preemption", "Zipf"),
       col=c("purple", "red", "blue", "green"), lty=1:2, cex=0.8)
