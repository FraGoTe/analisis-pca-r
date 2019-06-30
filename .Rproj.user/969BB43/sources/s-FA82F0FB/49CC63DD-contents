options(scipen=999)
options(digits=3)

setwd('~/Documents/Projects/Maestria/Multivariantes/Examen_Parcial_PCA/AnalisisPCA')

install.packages(c("FactoMineR", "factoextra"))

library("FactoMineR")
library("factoextra")

bancoCliente = read.csv("BancoCliente.csv", header = TRUE, row.names = 'ID_Cliente')
attach(bancoCliente)



res.pca <- PCA(bancoCliente, graph = FALSE)
print(res.pca)

fit <- princomp(na.omit(bancoCliente), cor = TRUE) # note the use of na.omit. If you're data still have missing values at this point (which it shouldn't), this should eliminate them.
summary(fit) # print summary of components


eig.val <- get_eigenvalue(res.pca)
eig.val


var <- get_pca_var(res.pca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

fviz_pca_var(res.pca, col.var = "black")

library("corrplot")

var <- get_pca_var(res.pca)
var
??CCA::cc
??get_pca_var
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

corrplot(var$cos2, is.corr=FALSE)


fviz_cos2(res.pca, choice = "var")

