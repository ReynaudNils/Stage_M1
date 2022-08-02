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

loglikzipf <- function(parms,N) -parms[2]*log(1:N + parms[1])-log(sum(1/(1:N + parms[1])^parms[2]))

#Sum of Squares
opt.f <- function(parms) sum((log(p)-loglikzipf(parms,length(p)))^2)
opt <- optim(par = c(2, 4), opt.f, lower = c(0,0), method = "L-BFGS-B")

#Maximum likelihood estimation
negloglikzipf <- function(parms1, parms2) sum(fr*(parms2*log(1:len +parms1)+log(sum(1/(1:len + parms1)^parms2))))

fit <- mle(negloglikzipf,start=list(parms1=2, parms2=4))

s.sq <- opt$par
s.ll <- coef(fit)
typeof(s.sq)
s.sq[1]
s.ll[1]
opt
fit
plot(1:len,p, xlab="Species", ylab="Frequencies")
lines(1:len,exp(loglikzipf(c(s.sq[1], s.sq[2]),len)),col="red")
lines(1:len,exp(loglikzipf(c(s.ll[1], s.ll[2]),len)),col="blue")
title(main = "Comparaison et fitting Zipf avec Moindre Carrés et Max de Vraisemblance")
legend(x = "topright", legend = c("Moindre Carrés", "Max Vraisemblance"), col = c("red", "blue"), lty = 1, cex = 0.8)
chisq.test(p, exp(loglikzipf(opt$par,length(p))))
