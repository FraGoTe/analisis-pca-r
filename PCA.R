options(scipen=999)
options(digits=3)

setwd('~/Documents/Projects/Maestria/Multivariantes/Examen_Parcial_PCA/AnalisisPCA')
install.packages(c("FactoMineR", "factoextra"))

# Librerias PCA
library("FactoMineR")
library("factoextra")

# Importando datos del CSV
bancoCliente = read.csv("BancoCliente.csv", header = TRUE, row.names = 'ID_Cliente')
attach(bancoCliente)

# Aplicando PCA
res.pca <- PCA(bancoCliente, graph = FALSE)
# Revisamos los resultados para PCA
print(res.pca)

# obtenemos los eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val 

#        eigenvalue variance.percent cumulative.variance.percent
# Dim.1      2.8356           14.924                        14.9
# Dim.2      2.1830           11.490                        26.4
# Dim.3      1.8449            9.710                        36.1
# Dim.4      1.1506            6.056                        42.2
# Dim.5      1.1023            5.801                        48.0
# Dim.6      1.0626            5.593                        53.6
# Dim.7      1.0394            5.471                        59.0
# Dim.8      1.0111            5.322                        64.4
# Dim.9      0.9944            5.233                        69.6
# Dim.10     0.9836            5.177                        74.8
# Dim.11     0.9502            5.001                        79.8
# Dim.12     0.9400            4.947                        84.7
# Dim.13     0.9043            4.760                        89.5
# Dim.14     0.8516            4.482                        94.0
# Dim.15     0.7667            4.035                        98.0
# Dim.16     0.2323            1.223                        99.2
# Dim.17     0.0783            0.412                        99.6
# Dim.18     0.0434            0.229                        99.9
# Dim.19     0.0256            0.135                       100.0

# Tambien podemos obtener los eigenvalues de las componentes
res.pca$eig

#          eigenvalue percentage of variance cumulative percentage of variance
# comp 1      2.8356                 14.924                              14.9
# comp 2      2.1830                 11.490                              26.4
# comp 3      1.8449                  9.710                              36.1
# comp 4      1.1506                  6.056                              42.2
# comp 5      1.1023                  5.801                              48.0
# comp 6      1.0626                  5.593                              53.6
# comp 7      1.0394                  5.471                              59.0
# comp 8      1.0111                  5.322                              64.4
# comp 9      0.9944                  5.233                              69.6
# comp 10     0.9836                  5.177                              74.8
# comp 11     0.9502                  5.001                              79.8
# comp 12     0.9400                  4.947                              84.7
# comp 13     0.9043                  4.760                              89.5
# comp 14     0.8516                  4.482                              94.0
# comp 15     0.7667                  4.035                              98.0
# comp 16     0.2323                  1.223                              99.2
# comp 17     0.0783                  0.412                              99.6
# comp 18     0.0434                  0.229                              99.9
# comp 19     0.0256                  0.135                             100.0

# Imprimimos el Scree Plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 15), ncp = 19)

# Correlacion de Variables
fviz_pca_var(res.pca, col.var = "black", axes = c(1, 2))
fviz_pca_var(res.pca, col.var = "black", axes = c(1, 3))
fviz_pca_var(res.pca,  axes = c(1, 5))

# Correlacion entre variables / componentes
library("corrplot")
res.pca.var <- get_pca_var(res.pca)
corrplot(res.pca.var$cos2, is.corr = FALSE)

# Calidad de Representación por Dimensión
fviz_cos2(res.pca, choice = "var")

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1)

# Descripcion de la Dimension 1
res.desc <- dimdesc(res.pca, proba = 0.05)
res.desc$Dim.1

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Agrpando por labels
fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = res.pca$Genero, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             palette = "jco")
