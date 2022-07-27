#setwd('C:/Users/nilsr')
#install.packages("dplyr")
#desc <- dplyr::desc
#library(dplyr)
Species <- read.csv('Especes.csv')
Genera <- read.csv('Especes2.csv')
Species$Rank<-rank( desc (Species$Images_Plante))
Genera$Rank<-rank( desc (Genera$Images_Genre))

#R code Species
set.seed(8)

q.hats <- c()
beta.hats <- c()
ss.hats <- c()
  
for (bx in 1:10) {
  q.mc <- c()
  res.sq.mc <- c()
    
  for (b in 1:300) {
    q.b <- runif(1, 0, 100)
    q.mc <- append(q.mc, q.b)
    print(length(q.b))
    print(q.b)
    print(length(Species$Rank))
    #print(Species$Rank)
    res.sq.b <- sum( lm(log(Species$Images_Plante) ~ log(Species$Rank +q.b))$residuals^2)
    res.sq.mc <- append(res.sq.mc, res.sq.b)
  }
  res.q.mc.dat <- data.frame(q.mc,res.sq.mc)
  q.hat <- res.q.mc.dat[which(res.q.mc.dat$res.sq.mc == min(res.q.mc.dat$res.sq.mc) ),]$q.mc
  beta.hat <- lm(log(Species$Images_Plante) ~ log(Species$Rank + q.hat))$coefficients[2]
  ss.hat <- sum( lm(log(Species$Images_Plante) ~ log(Species$Rank + q.hat))$residuals^2)
  q.hats <- append(q.hats, q.hat)
  beta.hats <- append(beta.hats, beta.hat)
  ss.hats <- append(ss.hats, ss.hat)
  qbeta.dat <- data.frame(q.hats,beta.hats)
}
fit <- lm( log(Species$Images_Plante) ~ log(Species$Rank + mean(q.hats)))
fit
q <- signif(mean(q.hats), 4)
z <- signif(fit$coef[[2]], 4) * (-1)
q
z
plot(Species$Rank +mean(q.hats), Species$Images_Plante, type="l", log="xy")
title(main="Plot pour les espèces avec le q optimisé")

#R code Generas
set.seed(8)

q.hats2 <- c()
beta.hats2 <- c()
ss.hats2 <- c()

for (bx in 1:10) {
  q.mc2 <- c()
  res.sq.mc2 <- c()
  
  for (b in 1:300) {
    q.b2 <- runif(1, 0, 100)
    q.mc2 <- append(q.mc2, q.b2)
    res.sq.b2 <- sum( lm(log(Genera$Images_Genre) ~ log(Genera$Rank +q.b2))$residuals^2)
    res.sq.mc2 <- append(res.sq.mc2, res.sq.b2)
  }
  res.q.mc.dat2 <- data.frame(q.mc2,res.sq.mc2)
  q.hat2 <- res.q.mc.dat2[which(res.q.mc.dat2$res.sq.mc2 == min(res.q.mc.dat2$res.sq.mc2) ),]$q.mc2
  beta.hat2 <- lm(log(Genera$Images_Genre) ~ log(Genera$Rank + q.hat2))$coefficients[2]
  ss.hat2 <- sum( lm(log(Genera$Images_Genre) ~ log(Genera$Rank + q.hat2))$residuals^2)
  q.hats2 <- append(q.hats2, q.hat2)
  beta.hats2 <- append(beta.hats2, beta.hat2)
  ss.hats2 <- append(ss.hats2, ss.hat2)
  qbeta.dat2 <- data.frame(q.hats2,beta.hats2)
}
fit2 <- lm( log(Genera$Images_Genre) ~ log(Genera$Rank + mean(q.hats2)))
fit2
q2 <- signif(mean(q.hats2), 4)
z2 <- signif(fit2$coef[[2]], 4) * (-1)
q2
z2
plot(Genera$Rank +mean(q.hats2), Genera$Images_Genre, type="l", log="xy")
title(main="Plot pour les genresavec le q optimisé")
