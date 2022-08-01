getwd()
setwd('C:/Users/nilsr/Desktop/PRE/Stage_M1/Jupyter_Notebook/Donnees_CSV')
library(stats4)

N <- 59    # the size of my dataset
Genera_Sedum <- read.csv("Especes_Sedum.csv")    # my dataset file
Genera_Sedum
Species <- read.csv("Especes.csv")
Genera <- read.csv("Especes2.csv")
Species
Genera

#install.packages("dplyr")
#desc <- dplyr::desc
#library(dplyr)
Genera_Sedum$Rank<-rank( desc (Genera_Sedum$Images_Sedum))
Species$Rank<-rank( desc (Species$Images_Plante))
Genera$Rank<-rank( desc (Genera$Images_Genre))
Liste = list(Species$Rank, Genera$Rank, Genera_Sedum$Rank)
Liste2 = list(Species$Images_Plante, Genera$Images_Genre, Genera_Sedum$Images_Sedum)

x=Genera$Images_Genre
y=x/sum(x)
y
x2=Genera$Rank
x2
sum(x)

#Représentons graphiquement nos données
plot(x2, y, type='l')


#Définissons la fonction maximum de vraisemblance
Pdf_Mandelbrot <- function(parms){
  invC <- sum(1/(1:N + parms[1])^parms[2])
  return(1/(x2 + parms[1])^(parms[2])*(1/invC))
}
#D'abord juste le negloglikelihood
negloglik <- function(parms) {
  invC <- sum(1/(1:N + parms[1])^parms[2])
  return(-sum(log(1/((x2 + parms[1])^(parms[2])*invC))))
}
negloglik2 <- function(parms1, parms2) {
  invC <- sum(1/(1:N + parms1)^parms2)
  return(-sum(log(1/((x2 + parms1)^(parms2)*invC))))
}
  
#Ensuite avec le gradient en plus
negloglik3 <- function(parms) {
  invC <- sum(1/(1:N + parms[1])^parms[2])
  res <- -sum(log(1/((x2 + parms[1])^(parms[2])*invC)))
  tampon <- sum(-parms[2]/(1:N + parms[1])^(parms[2]+1))
  tampon2 <- sum(exp(-parms[2]*log(1:N + parms[1])))
  tampon3 <- sum(-log(1:N + parms[1])*exp(-parms[2]*log(1:N + parms[1])))
  A <- sum((parms[2]/(x2 + parms[1])) + (tampon/invC))
  B <- sum(log(x2 + parms[1]) + (tampon3/tampon2))
  attr(res, "gradient") <- c(A, B)
  res
}
negloglik4 <- function(parms1, parms2) {
  invC <- sum(1/(1:N + parms1)^parms2)
  res <- -sum(log(1/((x2 + parms1)^(parms2)*invC)))
  tampon <- sum(-parms2/(1:N + parms1)^(parms2+1))
  tampon2 <- sum(exp(-parms2*log(1:N + parms1)))
  tampon3 <- sum(-log(1:N + parms1)*exp(-parms2*log(1:N + parms1)))
  A <- sum((parms2/(x2 + parms1)) + (tampon/invC))
  B <- sum(log(x2 + parms1) + (tampon3/tampon2))
  attr(res, "gradient") <- c(A, B)
  res
}
  
#La fonction gradient
gradient <- function(parms) {
  invC <- sum(1/(1:N + parms[1])^parms[2])
  tampon <- sum(-parms[2]/(1:N + parms[1])^(parms[2]+1))
  tampon2 <- sum(exp(-parms[2]*log(1:N + parms[1])))
  tampon3 <- sum(-log(1:N + parms[1])*exp(-parms[2]*log(1:N + parms[1])))
  A <- sum((parms[2]/(x2 + parms[1])) + (tampon/invC))
  B <- sum(log(x2 + parms[1]) + (tampon3/tampon2))
  return(c(A, B))
}

#Enfin avec la hessienne en plus, à coder avec les bons termes
#negloglik5 <- function(parms) {
#  invC <- sum(1/(1:N + parms[1])^parms[2])
#  res <- -sum(log(1/((x + parms[1])^(parms[2])*invC)))
#  tampon <- sum(-parms[2]/(1:N + parms[1])^(parms[2]+1))
#  tampon2 <- sum(exp(-parms[2]*log(1:N + parms[1])))
#  tampon3 <- sum(-log(1:N + parms[1])*exp(-parms[2]*log(1:N + parms[1])))
#  A <- sum((parms[2]/(x + parms[1]))) + (tampon/invC)
#  B <- sum(log(x + parms[1])) + (tampon3/tampon2)
#  H <- matrix(1:4, nrow=2, byrow=TRUE)
#  H[1,1] <- -sum(parms[2]/(x + parms[1])^2)
#  H[2,2] <- 0
#  H[1,2] <- sum(1/(x + parms[1]))
#  H[2,1] <- sum(1/(x + parms[1]))
#  attr(res, "gradient") <- c(A, B)
#  attr(res, "hessian") <- H
#  res
#}
#negloglik6 <- function(parms1, parms2) {
#  invC <- sum(1/(1:N + parms1)^parms2)
#  res <- -sum(log(1/((x + parms1)^(parms2)*invC)))
#  invC <- sum(1/(1:N + parms1)^parms2)
#  tampon <- sum(-parms2/(1:N + parms1)^(parms2+1))
#  tampon2 <- sum(exp(-parms2*log(1:N + parms1)))
#  tampon3 <- sum(-log(1:N + parms1)*exp(-parms2*log(1:N + parms1)))
#  A <- sum((parms2/(x + parms1))) + (tampon/invC)
#  B <- sum(log(x + parms1)) + (tampon3/tampon2)
#  H <- matrix(1:4, nrow=2, byrow=TRUE)
#  H[1,1] <- -sum(parms2/(x + parms1)^2)
#  H[2,2] <- 0
#  H[1,2] <- sum(1/(x + parms1))
#  H[2,1] <- sum(1/(x + parms1))
#  attr(res, "gradient") <- c(A, B)
#  attr(res, "hessian") <- H
#  res
#}

#On teste les fonctions
plot(Pdf_Mandelbrot(c(20, 20)))
negloglik(parms = c(2, 4))
negloglik2(2, 4)
negloglik3(parms = c(2, 4))
negloglik4(2, 4)
gradient(c(2, 4))

#Essayons différentes méthodes d'optimisation avec mle
#D'abord la méthode par défaut, Nelder-Mead, qui ne donne rien
mle(negloglik2, start=list(parms1=2, parms2=4), method = "Nelder-Mead")
#Ensuite d'autres méthodes ne fonctionnant pas
mle(negloglik2, start=list(parms1=2, parms2=4), method = "BFGS")
mle(negloglik2, start=list(parms1=2, parms2=4), method = "CG")
mle(negloglik2, start=list(parms1=2, parms2=4), method = "L-BFGS-B")
#mle(negloglik2, start=list(parms1=2, parms2=4), gr = gradient, method = "SANN")

#mle(minuslogl = negloglik, par=list(parms1=2, parms2=4), gr = gradient, method = "BFGS")
#optim(negloklik, start=list(parms1=2, parms2=4), gr=gradient)
#nlm(function(p) negloglik(p), p=c(2, 4), gradtol=1e-9, steptol=1e-9)

#install.packages("ucminf")
#install.packages("numDeriv")
library(ucminf)
library(numDeriv)

#Let's use different general optimizers of R
#D'abord optim, où l'on peut préciser le gradient avec gr=...
#d'abord sans préciser le gradient, puis ensuite en le précisant
optim = optim(par = c(2, 4), fn = negloglik, lower = c(0,10), hessian = F, method='L-BFGS-B')
optim2 = optim(par = c(2, 4), fn = negloglik, gr = gradient, lower = c(0, 1e-12), hessian = F, method="L-BFGS-B")
optim
optim2
#Vérifions que le gradient est nul aux points optimaux
gradient(optim$par)
gradient(optim2$par)

#Représentons une Mandelbrot avec les coefficients obtenus
plot(Pdf_Mandelbrot(optim$par), ylim=c(0, max(y)), col='blue')
lines(x2, y, type='l', col='red')
#Ensuite nlm, où le gradient doit être précisé comme attribut de la fonction, on utilise donc negloglik3
nlm <- nlm(negloglik, p=c(20,20), gradtol = 1e-12, steptol = 1e-9)
nlm2 = nlm(f = negloglik3, p = c(2, 4), hessian = F, check.analyticals=F, print.level=2, gradtol = 1e-12, steptol = 1e-9)
#nlm3 = nlm(f = negloglik5, p = rep(2, 4), hessian = TRUE)
nlm
nlm2
#nlm3
#On obtient un meilleur résultat avec la fonction sans gradient...
gradient(nlm$estimate)
gradient(nlm2$estimate)

nlminb = nlminb(start = c(2, 4), objective = negloglik, gradient = gradient)
ucminf::ucminf(par=rep(2, 4), fn = negloglik, hessian = 1)
nlminb

#optimHess(par = c(2, 4), fn = negloglik)

