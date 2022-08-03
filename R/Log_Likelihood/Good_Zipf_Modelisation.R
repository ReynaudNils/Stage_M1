library(stats4)
getwd()
setwd('C:/Users/nilsr/Desktop/PRE/Stage_M1/Jupyter_Notebook/Donnees_CSV')
#install.packages("dplyr")
#desc <- dplyr::desc
#library(dplyr)
Data <- read.csv("Especes2.csv")
Data$Rank<-rank( desc (Data$Images_Genre))
Data
fr <- unlist(Data['Images_Genre'])
p <- fr/sum(fr)
typeof(p)
len <- length(p)
len
rank <- unlist(Data['Rank'])
rank

loglikzipf <- function(s,N) -s*log(rank)-log(sum(1/(1:N)^s))

#Sum of Squares
opt.f <- function(s) sum((log(p)-loglikzipf(s,length(p)))^2)
opt <- optimize(opt.f, c(0, 10))

#Maximum likelihood estimation
negloglikzipf <- function(s) sum(fr*(s*log(rank)+log(sum(1/(1:len)^s))))

fit <- mle(negloglikzipf,start=list(s=1))

s.sq <- opt$minimum
s.ll <- coef(fit)
opt
fit
plot(1:len,p, xlab="Generas", ylab="Frequencies")
lines(1:len,exp(loglikzipf(opt$minimum,len)),col="red")
lines(1:len,exp(loglikzipf(coef(fit),len)),col="blue")
title(main = "Fitting Zipf for Generas")
legend(x = "topright", legend = c("Moindre Carrés", "Max Vraisemblance"), col = c("red", "blue"), lty = 1, cex = 0.8)
chisq.test(p, exp(loglikzipf(opt$minimum,length(p))))
chisq.test(p, exp(loglikzipf(coef(fit),length(p))))
