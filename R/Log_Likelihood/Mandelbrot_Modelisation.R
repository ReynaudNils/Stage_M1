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
N <- 59    # the size of my dataset
x <- read.csv("Especes_Sedum.csv")    # my dataset file
negloglik <- function(parms, x=x) {
       H <- sum(1/(1:N + parms[1])^parms[2])
       -sum(log(1/(x + parms[1])^parms[2]/H))
       }
plot(d <- sapply(1:N, function(x) exp(-negloglik(c(5,5), x))))  # here I changed 10 to N (the size of my data)
x <- sample(1:N, 1000, replace = T, prob = d)
ml <- nlm(function(p) negloglik(p, x=x), p=c(20,20))
