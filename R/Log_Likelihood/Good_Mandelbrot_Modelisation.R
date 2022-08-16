library(stats4)
getwd()
setwd('C:/Users/nilsr/Desktop/PRE/Stage_M1/Jupyter_Notebook/Donnees_CSV')
#install.packages("dplyr")
#desc <- dplyr::desc
#library(dplyr)
Data <- read.csv("Dataset_Especes_ZM.csv")
Data
Data$Rank<-rank( desc (Data$Images_Classes))
Data
fr <- unlist(Data['Images_Classes'])
p <- fr/sum(fr)
typeof(p)
len <- length(p)
typeof(fr)
rank <- unlist(Data['Rank'])
rank

loglikzipf <- function(parms,N) -parms[2]*log(rank + parms[1])-log(sum(1/(1:N + parms[1])^parms[2]))

#Sum of Squares
opt.f <- function(parms) sum((log(p)-loglikzipf(parms,length(p)))^2)
opt <- optim(par = c(2, 4), opt.f, lower = c(0,0), method = "L-BFGS-B")

#Maximum likelihood estimation
negloglikzipf <- function(parms1, parms2) sum(fr*(parms2*log(rank +parms1)+log(sum(1/(1:len + parms1)^parms2))))

fit <- mle(negloglikzipf,start=list(parms1=2, parms2=4))

opt$par
coef(fit)
plot(1:len,p, xlab="Species of Genera Lupinus", ylab="Frequencies")
#lines(1:len,exp(loglikzipf(opt$par,len)),col="red")
lines(1:len,exp(loglikzipf(coef(fit),len)),col="blue")
#lines(1:len,exp(loglikzipf(c(q2,z2),len)),col="green")
title(main = "Fitting Zipf-Mandelbrot for Genera Lupinus")
legend(x = "topright", legend = c("Moindre Carrés", "Max Vraisemblance", "Log-Log MC"), col = c("red", "blue", "green"), lty = 1, cex = 0.8)
chisq.test(p, exp(loglikzipf(opt$par,length(p))))
chisq.test(p, exp(loglikzipf(coef(fit),length(p))))
chisq.test(p, exp(loglikzipf(c(unlist(Listeq[8]),unlist(Listez[8])), len)))
typeof(Listez[1])
Listez[1]
