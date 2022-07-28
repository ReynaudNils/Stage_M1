#set.seed(123)

## set a N = 10
#N <- 10

## populate the negative log likelihood function
#negloglik <- function(parms, x=x) {
#  H <- sum(1/(1:N + parms[1])^parms[2])
#  -sum(log(1/(x + parms[1])^parms[2]/H))
#}

## generate pmf for sampling
#plot(d <- sapply(1:10, function(x) exp(-negloglik(c(5,5), x))))

## draw a random sample with n=1000
#x <- sample(1:10, 1000, replace = T, prob = d)
#x
## calculate ml estimates
#ml <- nlm(function(p) negloglik(p, x=x), p=c(20,20))

#getwd()
#setwd("C:/Users/nilsr")
#N <- 59    # the size of my dataset
#x <- read.csv("Especes_Sedum.csv")    # my dataset file
#negloglik <- function(parms, x=x) {
#       H <- sum(1/(1:N + parms[1])^parms[2])
#       -sum(log(1/(x + parms[1])^parms[2]/H))
#       }
#plot(d <- sapply(1:N, function(x) exp(-negloglik(c(5,5), x))))  # here I changed 10 to N (the size of my data)
#x <- sample(1:N, 1000, replace = T, prob = d)
#ml <- nlm(function(p) negloglik(p, x=x), p=c(20,20))

setwd('C:/Users/nilsr/Desktop/PRE/Stage_M1/Donnees_CSV')

N <- 59    # the size of my dataset
Genera_Sedum <- read.csv("Especes_Sedum.csv")    # my dataset file

x=Genera_Sedum$Images_Sedum
negloglik <- function(parms) {
  invC <- sum(1/(1:N + parms[1])^parms[2])
  return(-sum(log(1/((x + parms[1])^(parms[2])*invC))))
}
negloglik2 <- function(parms1, parms2) {
  invC <- sum(1/(1:N + parms1)^parms2)
  return(-sum(log(1/((x + parms1)^(parms2)*invC))))
}
negloglik3 <- function(parms) {
  invC <- sum(1/(1:N + parms[1])^parms[2])
  res <- -sum(log(1/((x + parms[1])^(parms[2])*invC)))
  A <- sum(parms[2]/x + parms[1])
  B <- sum(log(x + parms[1]))
  attr(res, "gradient") <- c(A, B)
  res
}
negloglik4 <- function(parms1, parms2) {
  invC <- sum(1/(1:N + parms1)^parms2)
  res <- -sum(log(1/((x + parms1)^(parms2)*invC)))
  A <- sum(parms2/x + parms1)
  B <- sum(log(x + parms1))
  attr(res, "gradient") <- c(A, B)
  res
}
negloglik5 <- function(parms) {
  invC <- sum(1/(1:N + parms[1])^parms[2])
  res <- -sum(log(1/((x + parms[1])^(parms[2])*invC)))
  A <- sum(parms[2]/x + parms[1])
  B <- sum(log(x + parms[1]))
  H <- matrix(1:4, nrow=2, byrow=TRUE)
  H[1,1] <- -sum(parms[2]/(x + parms[1])^2)
  H[2,2] <- 0
  H[1,2] <- sum(1/(x + parms[1]))
  H[2,1] <- sum(1/(x + parms[1]))
  attr(res, "gradient") <- c(A, B)
  attr(res, "hessian") <- H
  res
}
negloglik6 <- function(parms1, parms2) {
  invC <- sum(1/(1:N + parms1)^parms2)
  res <- -sum(log(1/((x + parms1)^(parms2)*invC)))
  A <- sum(parms2/x + parms1)
  B <- sum(log(x + parms1))
  H <- matrix(1:4, nrow=2, byrow=TRUE)
  H[1,1] <- -sum(parms2/(x + parms1)^2)
  H[2,2] <- 0
  H[1,2] <- sum(1/(x + parms1))
  H[2,1] <- sum(1/(x + parms1))
  attr(res, "gradient") <- c(A, B)
  attr(res, "hessian") <- H
  res
}
gradient <- function(parms) {
  invC <- sum(1/(1:N + parms[1])^parms[2])
  tampon <- sum(-parms[2]/(1:N + parms[1])^(parms[2]+1))
  tampon2 <- sum(exp(-parms[2]*log(1:N + parms[1])))
  tampon3 <- sum(-log(1:N + parms[1])*exp(-parms[2]*log(1:N + parms[1])))
  A <- sum((parms[2]/(x + parms[1]))) + (tampon/invC)
  B <- sum(log(x + parms[1])) + (tampon3/tampon2)
  return(c(A, B))
}
gradient(optim2$par)
library(stats4)
negloglik2(2, 4)
gradient(c(2, 4))

#Essayons différentes méthodes d'optimisation
#D'abord la méthode par défaut, Nelder-Mead, qui ne donne rien
mle(negloglik6, start=list(parms1=2, parms2=4), method = "Nelder-Mead")
#Ensuite d'autres méthodes ne fonctionnant pas
mle(negloglik6, start=list(parms1=2, parms2=4), method = "BFGS")
mle(negloglik6, start=list(parms1=2, parms2=4), method = "CG")
mle(negloglik6, start=list(parms1=2, parms2=4), method = "L-BFGS-B")
ml <- mle(negloglik6, start=list(parms1=2, parms2=4), gr = gradient, method = "SANN")

#mle(minuslogl = negloglik, par=list(parms1=2, parms2=4), gr = gradient, method = "BFGS")
#optim(negloklik, start=list(parms1=2, parms2=4), gr=gradient)
#nlm(function(p) negloglik(p), p=c(2, 4), gradtol=1e-9, steptol=1e-9)

install.packages("ucminf")
install.packages("numDeriv")
library(ucminf)
library(numDeriv)

#Let's use different geenral optimizers of R
optim = optim(par = c(2, 4), fn = negloglik, hessian = F)
optim2 = optim(par = c(2, 4), fn = negloglik, gr = gradient, hessian = F, method="BFGS")
nlm = nlm(f = negloglik3, p = rep(2, 4), hessian = F)
nlm2 = nlm(f = negloglik5, p = rep(2, 4), hessian = TRUE)
nlminb = nlminb(start = rep(2, 4), objective = negloglik3)
ucminf::ucminf(par=rep(2, 4), fn = negloglik, hessian = 1)
optim
optim2
nlm
nlm2
ml
optimHess(par = rep(2, 4), fn = negloglik







#negloglik(c(1,1), x=2 )
plot(d <- sapply(1:N, function(x) exp(-negloglik(5, 5))))  # here I changed 10 to N (the size of my data)
x <- sample(1:N, 1000, replace = T, prob = d)
ml <- nlm(function(p) negloglik(p, x=Genera_Sedum$Images_Sedum), p=c(20,20), gradtol = 1e-12, steptol = 1e-9)
ml