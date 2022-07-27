getwd()
setwd("C:/Users/nilsr")
library(vegan)

#Genera_Sedum
#Visualisation
Genera_Sedum <- read.csv('Especes_Sedum.csv')
Genera_Sedum
L <- Genera_Sedum[[2]]
plot(L,type='l',xlab="Species of Genera_Sedum ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Sedum")
#Modelisation
GSedum <- radfit(L)
GSedum
plot(GSedum)

#Genera_Acacia
#Visualisation
Genera_Acacia <- read.csv('Especes_Acacia.csv')
Genera_Acacia
L2 <- Genera_Acacia[[2]]
plot(L2,type='l',xlab="Species of Genera_Acacia ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Acacia")
#Modelisation
GAcacia <- radfit(L2)
GAcacia
plot(GAcacia)

#Genera_Trifolium
#Visualisation
Genera_Trifolium <- read.csv('Especes_Trifolium.csv')
Genera_Trifolium
L3 <- Genera_Trifolium[[2]]
plot(L3,type='l',xlab="Species of Genera_Trifolium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Trifolium")
#Modelisation
GTrifolium <- radfit(L3)
GTrifolium
plot(GTrifolium)

#Genera_Hypericum
#Visualisation
Genera_Hypericum <- read.csv('Especes_Hypericum.csv')
Genera_Hypericum
L4 <- Genera_Hypericum[[2]]
plot(L4,type='l',xlab="Species of Genera_Hypericum ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Hypericum")
#Modelisation
GHypericum <- radfit(L4)
GHypericum
plot(GHypericum)

#Genera_Ophrys
#Visualisation
Genera_Ophrys <- read.csv('Especes_Ophrys.csv')
Genera_Ophrys
L5 <- Genera_Ophrys[[2]]
plot(L5,type='l',xlab="Species of Genera_Hypericum ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Hypericum")
#Modelisation
GHypericum <- radfit(L5)
GHypericum
plot(GHypericum)

#Genera_Anemone
#Visualisation
Genera_Anemone <- read.csv('Especes_Anemone.csv')
Genera_Anemone
L6 <- Genera_Anemone[[2]]
plot(L6,type='l',xlab="Species of Genera_Anemone ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Anemone")
#Modelisation
GAnemone <- radfit(L6)
GAnemone
plot(GAnemone)

#Genera_Cirsium
#Visualisation
Genera_Cirsium <- read.csv('Especes_Cirsium.csv')
Genera_Cirsium
rowSums(is.na(Genera_Cirsium))
L7 <- Genera_Cirsium[[2]]
plot(L7,type='l',xlab="Species of Genera_Cirsium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Cirsium")
#Modelisation
GCirsium <- radfit(L7)
GCirsium
plot(GCirsium)

#Genera_Pelargonium
#Visualisation
Genera_Pelargonium <- read.csv('Especes_Pelargonium.csv')
Genera_Pelargonium
rowSums(is.na(Genera_Pelargonium))
L8 <- Genera_Pelargonium[[2]]
plot(L8,type='l',xlab="Species of Genera_Pelargonium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Pelargonium")
#Modelisation
GPelargonium <- radfit(L8)
GPelargonium
plot(GPelargonium)

#Genera_Peperomia
#Visualisation
Genera_Peperomia <- read.csv('Especes_Peperomia.csv')
Genera_Peperomia
L9 <- Genera_Peperomia[[2]]
plot(L9,type='l',xlab="Species of Genera_Peperomia ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Peperomia")
#Modelisation
GPeperomia <- radfit(L9)
GPeperomia
plot(GPeperomia)

#Genera_Lupinus
#Visualisation
Genera_Lupinus <- read.csv('Especes_Lupinus.csv')
Genera_Lupinus
L10 <- Genera_Lupinus[[2]]
plot(L10,type='l',xlab="Species of Genera_Lupinus ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Lupinus")
#Modelisation
GLupinus <- radfit(L10)
GLupinus
plot(GLupinus)

#Genera_Dryopteris
#Visualisation
Genera_Dryopteris <- read.csv('Especes_Dryopteris.csv')
Genera_Dryopteris
L11 <- Genera_Dryopteris[[2]]
plot(L11,type='l',xlab="Species of Genera_Dryopteris ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Dryopteris")
#Modelisation
GDryopteris <- radfit(L11)
GDryopteris
plot(GDryopteris)

#Genera_Lactuca
#Visualisation
Genera_Lactuca <- read.csv('Especes_Lactuca.csv')
Genera_Lactuca
rowSums(is.na(Genera_Lactuca))
L12 <- Genera_Lactuca[[2]]
plot(L12,type='l',xlab="Species of Genera_Lactuca ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Lactuca")
#Modelisation
GLactuca <- radfit(L12)
GLactuca
plot(GLactuca)

#Genera_Tradescantia
#Visualisation
Genera_Tradescantia <- read.csv('Especes_Tradescantia.csv')
Genera_Tradescantia
L13 <- Genera_Tradescantia[[2]]
plot(L13,type='l',xlab="Species of Genera_Tradescantia ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Tradescantia")
#Modelisation
GTradescantia <- radfit(L13)
GTradescantia
plot(GTradescantia)

#Genera_Alocasia
#Visualisation
Genera_Alocasia <- read.csv('Especes_Alocasia.csv')
Genera_Alocasia
L14 <- Genera_Alocasia[[2]]
plot(L14,type='l',xlab="Species of Genera_Alocasia ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Alocasia")
#Modelisation
GAlocasia <- radfit(L14)
GAlocasia
plot(GAlocasia)

#Genera_Papaver
#Visualisation
Genera_Papaver <- read.csv('Especes_Papaver.csv')
Genera_Papaver
L15 <- Genera_Papaver[[2]]
plot(L15,type='l',xlab="Species of Genera_Papaver ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Papaver")
#Modelisation
GPapaver <- radfit(L15)
GPapaver
plot(GPapaver)

#Genera_Crotalaria
#Visualisation
Genera_Crotalaria <- read.csv('Especes_Crotalaria.csv')
Genera_Crotalaria
L16 <- Genera_Crotalaria[[2]]
plot(L16,type='l',xlab="Species of Genera_Crotalaria ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Crotalaria")
#Modelisation
GCrotalaria <- radfit(L16)
GCrotalaria
plot(GCrotalaria)

#Genera_Phyllanthus
#Visualisation
Genera_Phyllanthus <- read.csv('Especes_Phyllanthus.csv')
Genera_Phyllanthus
rowSums(is.na(Genera_Phyllanthus))
L17 <- Genera_Phyllanthus[[2]]
plot(L17,type='l',xlab="Species of Genera_Phyllanthus ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Phyllanthus")
#Modelisation
GPhyllanthus <- radfit(L17)
GPhyllanthus
plot(GPhyllanthus)

#Genera_Dendrobium
#Visualisation
Genera_Dendrobium <- read.csv('Especes_Dendrobium.csv')
Genera_Dendrobium
rowSums(is.na(Genera_Dendrobium))
L18 <- Genera_Dendrobium[[2]]
plot(L18,type='l',xlab="Species of Genera_Dendrobium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Dendrobium")
#Modelisation
GDendrobium <- radfit(L18)
GDendrobium
plot(GDendrobium)

#Genera_Anthurium
#Visualisation
Genera_Anthurium <- read.csv('Especes_Anthurium.csv')
Genera_Anthurium
rowSums(is.na(Genera_Anthurium))
L19 <- Genera_Anthurium[[2]]
plot(L19,type='l',xlab="Species of Genera_Anthurium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Anthurium")
#Modelisation
GAnthurium <- radfit(L19)
GAnthurium
plot(GAnthurium)

#Genera_Lamium
#Visualisation
Genera_Lamium <- read.csv('Especes_Lamium.csv')
Genera_Lamium
rowSums(is.na(Genera_Lamium))
L20 <- Genera_Lamium[[2]]
plot(L20,type='l',xlab="Species of Genera_Lamium ranked in descending order",ylab="Number of Images", log="y")
title(main = "Long-Tail Distribution Genera_Lamium")
#Modelisation
GLamium <- radfit(L20)
GLamium
plot(GLamium)

