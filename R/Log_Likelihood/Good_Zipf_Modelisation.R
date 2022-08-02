library(stats4)
getwd()
setwd('C:/Users/nilsr/Desktop/PRE/Stage_M1/Jupyter_Notebook/Donnees_CSV')
Data <- read.csv("Especes_Sedum.csv")
Data
fr <- unlist(Data['Images_Sedum'])
p <- fr/sum(fr)
typeof(p)
len <- length(p)
len

loglikzipf <- function(s,N) -s*log(1:N)-log(sum(1/(1:N)^s))

#Sum of Squares
opt.f <- function(s) sum((log(p)-loglikzipf(s,length(p)))^2)
opt <- optimize(opt.f, c(0, 10))

#Maximum likelihood estimation
negloglikzipf <- function(s) sum(fr*(s*log(1:len)+log(sum(1/(1:len)^s))))

fit <- mle(negloglikzipf,start=list(s=1))

s.sq <- opt$minimum
s.ll <- coef(fit)
opt
fit
plot(1:len,p,log="xy")
lines(1:len,exp(loglikzipf(s.sq,len)),col="red")
lines(1:len,exp(loglikzipf(s.ll,len)),col="blue")
title(main = "Comparaison et fitting Zipf avec Moindre Carrés et Max de Vraisemblance")
chisq.test(p, exp(loglikzipf(opt$minimum,length(p))))
