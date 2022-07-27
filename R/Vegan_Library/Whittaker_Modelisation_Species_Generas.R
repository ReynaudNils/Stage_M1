#Species
#With Abundance
getwd()
setwd("C:/Users/nilsr")
library(vegan)
Species <- read.csv('Especes.csv')
Species
rowSums(is.na(Species))
L <- Species[[2]]/100
L
plot(L,xlab="Species ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Species, y en log-scale")

#Modélisation
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
R5 <- radfit(L)
R5
plot(R5)
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
plot(L,xlab="Species ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Species, Fitting")
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
#R5 <- rad.zipfbrot(L)
legend("topright", legend=c("Log-Normal", "Brokenstick", "Preemption", "Zipf"),
       col=c("purple", "red", "blue", "green"), lty=1:2, cex=0.8)

#Generas
#Visualisation
Generas <- read.csv('Especes2.csv')
Generas
rowSums(is.na(Generas))
L2 <- Generas[[2]]
L2
plot(L2,xlab="Generas ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Generas, y en log-scale")
#Modelisation, radfit ne fonctionne encore pas sur tout G
GL <- radfit(L2)
G5 <- rad.zipfbrot(L2)
#En séparant différents domaines
radG <- radfit(L2[1:20])
radG2 <- radfit(L2[20:50])
radG3 <- radfit(L2[50:200])
radG4 <- radfit(L2[200:1081])
L2[1:20]
radG
plot(radG)
radG2
plot(radG2)
radG3
plot(radG3)
radG4
plot(radG4)
#Les autres modèles
plot(L2,xlab="Generas ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Generas, y en log-scale")
G <-rad.lognormal(L2)
G
lines(G, col='purple')
G2 <- rad.null(L2)
G2
lines(G2, col='red')
G3 <- rad.preempt(L2)
G3
lines(G3, col='blue')
G4 <- rad.zipf(L2)
G4
lines(G4, col='green')
legend("topright", legend=c("Log-Normal", "Brokenstick", "Preemption", "Zipf"),
       col=c("purple", "red", "blue", "green"), lty=1:2, cex=0.8)

#Genera_Lactuca
#Visualisation
Genera_Lactuca <- read.csv('Especes_Lactuca.csv')
Genera_Lactuca
rowSums(is.na(Genera_Lactuca))
L3 <- Genera_Lactuca[[2]]
L3
plot(L3,type='l',xlab="Species of Genera_Lactuca ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Lactuca")
#Modelisation
GLactuca <- radfit(L3)
GLactuca
plot(GLactuca)
#Autre méthode
#plot(L3,xlab="Species of Genera_Lactuca ranked in descending order",ylab="Number of Images", log="y")
#title(main = "Long-Tail Distribution of Genera_Lactuca, Fitting")
#GLactuca2 <-rad.lognormal(L3)
#GLactuca2
#lines(GLactuca2, col='purple')
#GLactuca3 <- rad.null(L3)
#GLactuca3
#lines(GLactuca3, col='red')
#GLactuca4 <- rad.preempt(L3)
#GLactuca4
#lines(GLactuca4, col='blue')
#GLactuca5 <- rad.zipf(L3)
#GLactuca5
#lines(GLactuca5, col='green')
#GLactuca6 <- rad.zipfbrot(L3)
#GLactuca6
#lines(GLactuca6, col='brown')
#legend("topright", legend=c("Log-Normal", "Brokenstick", "Preemption", "Zipf", "Mandelbrot"),
#       col=c("purple", "red", "blue", "green", "brown"), lty=1:2, cex=0.8)

#Genera_Pelargonium
#Visualisation
Genera_Pelargonium <- read.csv('Especes_Pelargonium.csv')
Genera_Pelargonium
rowSums(is.na(Genera_Pelargonium))
L4 <- Genera_Pelargonium[[2]]
L4
plot(L4,type='l',xlab="Species of Genera_Pelargonium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Pelargonium")
#Modelisation
GPelargonium <- radfit(L4)
GPelargonium
plot(GPelargonium)

#Genera_Cirsium
#Visualisation
Genera_Cirsium <- read.csv('Especes_Cirsium.csv')
Genera_Cirsium
rowSums(is.na(Genera_Cirsium))
L5 <- Genera_Cirsium[[2]]
L5
plot(L5,type='l',xlab="Species of Genera_Cirsium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Cirsium")
#Modelisation
GCirsium <- radfit(L5)
GCirsium
plot(GCirsium)

#Genera_Mercurialis
#Visualisation
Genera_Mercurialis <- read.csv('Especes_Mercurialis.csv')
Genera_Mercurialis
rowSums(is.na(Genera_Mercurialis))
L6 <- Genera_Mercurialis[[2]]
L6
plot(L6,type='l',xlab="Species of Genera_Mercurialis ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Mercurialis")
#Modelisation
GMercurialis <- radfit(L6)
GMercurialis
plot(GMercurialis)

#Genera_Phyllanthus
#Visualisation
Genera_Phyllanthus <- read.csv('Especes_Phyllanthus.csv')
Genera_Phyllanthus
rowSums(is.na(Genera_Phyllanthus))
L7 <- Genera_Phyllanthus[[2]]
L7
plot(L7,type='l',xlab="Species of Genera_Phyllanthus ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Phyllanthus")
#Modelisation
GPhyllanthus <- radfit(L7)
GPhyllanthus
plot(GPhyllanthus)
