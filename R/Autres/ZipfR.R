install.packages("zipfR", repos="http://R-Forge.R-project.org")
library(zipfR)
ItaRi.spc
summary(ItaRi.spc)
plot(ItaRi.spc)
plot(ItaRi.spc, log="x")
ItaRi.fzm <- lnre("fzm", ItaRi.spc, exact=FALSE)
summary(ItaRi.fzm)
