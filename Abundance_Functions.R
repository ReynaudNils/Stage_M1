library(vegan)
data(BCI)
#Fisher's log-series
k <- sample(nrow(BCI), 1)
fish <- fisherfit(BCI[k,])
fish
plot(fish)
#Preston
preston <- prestondistr(BCI[k,])
plot(preston)
print(BCI)
#Whittaker figures, ranked abundance figures
rad <- radfit(BCI[k,])
rad
plot(rad)
BCI[k,]
k
BCI
typeof(BCI[k,])
help(sample)
rad <- radfit(BCI[k,])
####
data(BCI)
BCI
BCI[1,]
mod <- rad.lognormal(BCI[5,])
mod
plot(mod)
mod <- radfit(BCI[1,])
## Standard plot overlaid for all models
## Pre-emption model is a line
plot(mod)
## log for both axes: Zipf model is a line
plot(mod, log = "xy")
## Lattice graphics separately for each model
radlattice(mod)
# Take a subset of BCI to save time and nerves
mod <- radfit(BCI[3:5,])
mod
plot(mod, pch=".")