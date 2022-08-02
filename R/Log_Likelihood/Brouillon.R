library(stats4)
#Finding the data
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
fr_Acacia <- Genera_Acacia['Images_Acacia']
fr_Trifolium <- Genera_Trifolium['Images_Trifolium']
fr_Hypericum <- Genera_Hypericum['Images_Hypericum']
fr_Ophrys <- Genera_Ophrys['Images_Ophrys']
fr_Anemone <- Genera_Anemone['Images_Anemone']
fr_Cirsium <- Genera_Cirsium['Images_Cirsium']
fr_Pelargonium <- Genera_Pelargonium['Images_Pelargonium']
fr_Peperomia <- Genera_Peperomia['Images_Peperomia']
fr_Lupinus <- Genera_Lupinus['Images_Lupinus']
fr_Dryopteris <- Genera_Dryopteris['Images_Dryopteris']
p_Acacia <- fr_Acacia/sum(fr_Acacia)
p_Trifolium <- fr_Trifolium/sum(fr_Trifolium)
p_Hypericum <- fr_Hypericum/sum(fr_Hypericum)
p_Ophrys <- fr_Ophrys/sum(fr_Ophrys)
p_Anemone <- fr_Anemone/sum(fr_Anemone)
p_Cirsium <- fr_Cirsium/sum(fr_Cirsium)
p_Pelargonium <- fr_Pelargonium/sum(fr_Pelargonium)
p_Peperomia <- fr_Peperomia/sum(fr_Peperomia)
p_Lupinus <- fr_Lupinus/sum(fr_Lupinus)
p_Dryopteris <- fr_Dryopteris/sum(fr_Dryopteris)
Liste = list(fr_Acacia, fr_Trifolium, fr_Hypericum, fr_Ophrys, fr_Anemone, fr_Cirsium, fr_Pelargonium, fr_Peperomia, fr_Lupinus, fr_Dryopteris)
Liste2 = list(p_Acacia, p_Trifolium, p_Hypericum, p_Ophrys, p_Anemone, p_Cirsium, p_Pelargonium, p_Peperomia, p_Lupinus, p_Dryopteris)
Liste3 = c("Acacia", "Trifolium", "Hypericum", "Ophrys", "Anemone", "Cirsium", "Pelargonium", "Peperomia", "Lupinus", "Dryopteris")
unlist(Liste2[1])
Liste3[2]
#Loglikelihood
loglikzipf <- function(s,N) -s*log(1:N)-log(sum(1/(1:N)^s))

for (i in 1:10){
  print(Liste3[i])
  len <- length(unlist(Liste2[i]))
  #Sum of Squares
  opt.f <- function(s) sum((log(unlist(Liste2[i]))-loglikzipf(s,len)^2)
  opt <- optimize(opt.f, c(0, 10))
  print(opt)
  #Maximum likelihood estimation
  negloglikzipf <- function(s) sum(unlist(Liste[i])*(s*log(1:len+log(sum(1/(1:len^s)))))
  
  fit <- mle(negloglikzipf,start=list(s=1))
  #print(fit)
}