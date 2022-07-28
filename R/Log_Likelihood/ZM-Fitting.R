#On veut modéliser de la même manière pour chaque genre contenant un nombre suffisant d'espèces
#Modélisons le Genre Sedum
Genera_Sedum <- read.csv('Especes_Sedum.csv')
#install.packages("dplyr")
#desc <- dplyr::desc
#library(dplyr)
Genera_Acacia <- read.csv('Especes_Acacia.csv')
Genera_Trifolium <- read.csv('Especes_Trifolium.csv')
Genera_Hypericum <- read.csv('Especes_Hypericum.csv')
Genera_Ophrys <- read.csv('Especes_Ophrys.csv')
Genera_Anemone <- read.csv('Especes_Anemone.csv')
Genera_Cirsium <- read.csv('Especes_Cirsium.csv')
Genera_Pelargonium <- read.csv('Especes_Pelargonium.csv')
Genera_Peperomia <- read.csv('Especes_Peperomia.csv')
Genera_Lupinus <- read.csv('Especes_Lupinus.csv')
Genera_Dryopteris <- read.csv('Especes_Dryopteris.csv')
Genera_Lactuca <- read.csv('Especes_Lactuca.csv')
Genera_Tradescantia <- read.csv('Especes_Tradescantia.csv')
Genera_Alocasia <- read.csv('Especes_Alocasia.csv')
Genera_Papaver <- read.csv('Especes_Papaver.csv')
Genera_Crotalaria <- read.csv('Especes_Crotalaria.csv')
Genera_Phyllanthus <- read.csv('Especes_Phyllanthus.csv')
Genera_Dendrobium <- read.csv('Especes_Dendrobium.csv')
Genera_Anthurium <- read.csv('Especes_Anthurium.csv')
Genera_Lamium <- read.csv('Especes_Lamium.csv')

Genera_Sedum$Rank<-rank( desc (Genera_Sedum$Images_Sedum))
Genera_Acacia$Rank<-rank( desc (Genera_Acacia$Images_Acacia))
Genera_Trifolium$Rank<-rank( desc (Genera_Trifolium$Images_Trifolium))
Genera_Hypericum$Rank<-rank( desc (Genera_Hypericum$Images_Hypericum))
Genera_Ophrys$Rank<-rank( desc (Genera_Ophrys$Images_Ophrys))
Genera_Anemone$Rank<-rank( desc (Genera_Anemone$Images_Anemone))
Genera_Cirsium$Rank<-rank( desc (Genera_Cirsium$Images_Cirsium))
Genera_Pelargonium$Rank<-rank( desc (Genera_Pelargonium$Images_Pelargonium))
Genera_Peperomia$Rank<-rank( desc (Genera_Peperomia$Images_Peperomia))
Genera_Lupinus$Rank<-rank( desc (Genera_Lupinus$Images_Lupinus))
Genera_Dryopteris$Rank<-rank( desc (Genera_Dryopteris$Images_Dryopteris))
Genera_Lactuca$Rank<-rank( desc (Genera_Lactuca$Nombre_Images))
Genera_Tradescantia$Rank<-rank( desc (Genera_Tradescantia$Images_Tradescantia))
Genera_Alocasia$Rank<-rank( desc (Genera_Alocasia$Images_Alocasia))
Genera_Papaver$Rank<-rank( desc (Genera_Papaver$Images_Papaver))
Genera_Crotalaria$Rank<-rank( desc (Genera_Crotalaria$Images_Crotalaria))
Genera_Phyllanthus$Rank<-rank( desc (Genera_Phyllanthus$Images_Phyllanthus))
Genera_Dendrobium$Rank<-rank( desc (Genera_Dendrobium$Images_Dendrobium))
Genera_Anthurium$Rank<-rank( desc (Genera_Anthurium$Images_Anthurium))
Genera_Lamium$Rank<-rank( desc (Genera_Lamium$Images_Lamium))


Liste = list(Genera_Sedum$Rank, Genera_Acacia$Rank, Genera_Trifolium$Rank, Genera_Hypericum$Rank, Genera_Ophrys$Rank, Genera_Anemone$Rank, Genera_Cirsium$Rank, Genera_Pelargonium$Rank, Genera_Peperomia$Rank, Genera_Lupinus$Rank, Genera_Dryopteris$Rank, Genera_Lactuca$Rank, Genera_Tradescantia$Rank, Genera_Alocasia$Rank, Genera_Papaver$Rank, Genera_Crotalaria$Rank, Genera_Phyllanthus$Rank, Genera_Dendrobium$Rank, Genera_Anthurium$Rank, Genera_Lamium$Rank)
Liste2 = list(Genera_Sedum$Images_Sedum, Genera_Acacia$Images_Acacia, Genera_Trifolium$Images_Trifolium, Genera_Hypericum$Images_Hypericum, Genera_Ophrys$Images_Ophrys, Genera_Anemone$Images_Anemone, Genera_Cirsium$Images_Cirsium, Genera_Pelargonium$Images_Pelargonium, Genera_Peperomia$Images_Peperomia, Genera_Lupinus$Images_Lupinus, Genera_Dryopteris$Images_Dryopteris, Genera_Lactuca$Nombre_Images, Genera_Tradescantia$Images_Tradescantia, Genera_Alocasia$Images_Alocasia, Genera_Papaver$Images_Papaver, Genera_Crotalaria$Images_Crotalaria, Genera_Phyllanthus$Images_Phyllanthus, Genera_Dendrobium$Images_Dendrobium, Genera_Anthurium$Images_Anthurium, Genera_Lamium$Images_Lamium)
typeof(unlist(Liste[1]))
Genera_Sedum$Rank
typeof(unlist(Liste2[1]))
log(Genera_Sedum$Images_Sedum)
log(unlist(Liste2[1]))
Genera_Lamium

#R code
set.seed(8)
Liste3 = c()
Liste4 = c()
for (i in 1:20){
  q.hats <- c()
  beta.hats <- c()
  ss.hats <- c()
  
  for (bx in 1:10) {
    q.mc <- c()
    res.sq.mc <- c()
    
    for (b in 1:30) {
      q.b <- runif(1, 0, 100)
      q.mc <- append(q.mc, q.b)
      print(q.b)
      print(length(q.b))
      print(Liste)
      print(length(unlist(Liste[i])))
      res.sq.b <- sum( lm(log(unlist(Liste2[i])) ~ log(unlist(Liste[i]) +q.b))$residuals^2)
      res.sq.mc <- append(res.sq.mc, res.sq.b)
    }
    res.q.mc.dat <- data.frame(q.mc,res.sq.mc)
    q.hat <- res.q.mc.dat[which(res.q.mc.dat$res.sq.mc == min(res.q.mc.dat$res.sq.mc) ),]$q.mc
    beta.hat <- lm(log(unlist(Liste2[i])) ~ log(unlist(Liste[i]) + q.hat))$coefficients[2]
    ss.hat <- sum( lm(log(unlist(Liste2[i])) ~ log(unlist(Liste[i]) + q.hat))$residuals^2)
    q.hats <- append(q.hats, q.hat)
    beta.hats <- append(beta.hats, beta.hat)
    ss.hats <- append(ss.hats, ss.hat)
    qbeta.dat <- data.frame(q.hats,beta.hats)
  }
  fit <- lm( log(unlist(Liste2[i])) ~ log(unlist(Liste[i]) + mean(q.hats)))
  Liste3[[(length(Liste3) + 1)]] <- fit
  Liste4[[(length(Liste4) + 1)]] <- mean(q.hats)
}

for (i in 1:20){
  plot(unlist(Liste[i]) +unlist(Liste4[i]), unlist(Liste2[i]), type='l', log="xy")
  #title(main="Plot avec le q optimisé")
}

#q <- signif(mean(q.hats), 4)
#z <- signif(fit$coef[[2]], 4) * (-1)
#q
#z
#fit$coef
#plot(Genera_Sedum$Images_Sedum, log="xy")
#title(main = "Plot avec q=O")
#plot(Genera_Sedum$Rank +0.5, Genera_Sedum$Images_Sedum, log="xy", col="blue")
#plot(Genera_Sedum$Rank +5, Genera_Sedum$Images_Sedum, log="xy", col="red")
#plot(Genera_Sedum$Rank +10, Genera_Sedum$Images_Sedum, log="xy", col="green")
#plot(Genera_Sedum$Rank +50, Genera_Sedum$Images_Sedum, log="xy", col="brown")
#plot(Genera_Sedum$Rank +100, Genera_Sedum$Images_Sedum, log="xy", col="purple")
#plot(Genera_Sedum$Rank +mean(q.hats), Genera_Sedum$Images_Sedum, log="xy")
#title(main="Plot avec le q optimisé")
#plot(x, 8.69x)
setwd('C:/Users/nilsr/Bureau/PRE/Stage_M1/Donnees_CSV')

N <- 59    # the size of my dataset
Genera_Sedum <- read.csv("Especes_Sedum.csv")    # my dataset file

x=Genera_Sedum$Images_Sedum
negloglik <- function(parms1, parms2) {
  invC <- sum(1/(1:N + parms1)^parms2)
  return(-sum(log(1/((x + parms1)^(parms2)*invC))))
}

library(stats4)
negloglik(2, 4)
mle(negloglik, start=list(parms1=20, parms2=20), method = )
help(mle)
negloglik(c(1,1), x=2 )
plot(d <- sapply(1:N, function(x) exp(-negloglik(c(5,5), x))))  # here I changed 10 to N (the size of my data)
x <- sample(1:N, 1000, replace = T, prob = d)
ml <- nlm(function(p) negloglik(p, x=Genera_Sedum$Images_Sedum), p=c(20,20), gradtol = 1e-12, steptol = 1e-9)
ml
#x
#Species <- read.csv('Especes.csv')
#Species$Rank<-rank( desc (Species$Images_Plante))
#Species

## set a N = 10
#N <- length(Genera_Sedum$Images_Sedum)

## populate the negative log likelihood function
#negloglik <- function(parms, x=x) {
#H <- sum(1/(1:N + parms[1])^parms[2])
#  -sum(log(1/(x + parms[1])^parms[2]/H))
#}

#generate pmf for sampling
#plot(d <- sapply(1:10, function(x) exp(-negloglik(c(5,5), x))))

## draw a random sample with n=1000
#x <- sample(1:10, 1000, replace = T, prob = d)
#x
## calculate ml estimates
#ml <- nlm(function(p) negloglik(p, x=Genera_Sedum$Images_Sedum), p=c(20,20))
#ml$estimate
#plot(Genera_Sedum$Rank +ml$estimate[1], Genera_Sedum$Images_Sedum, log="xy")
#N
#somme = 0
#for (i in 1:N){
#  somme = somme + (1/(i+q)^z)
#}
#log(somme)
