install.packages("corrplot")
install.packages("factoextra")
install.packages("lattice")
library(lattice)
library(corrplot)
library("FactoMineR")
library(factoextra)
library(ggplot2)
library(tibble)

#donnees recettes-pays.data


setwd("D:/Bi/utc/SY09/Projet 1")
recette_pays <- read.table("donnees/recettes-pays.data", header = TRUE, sep = ",")


#Question 1 : Analyse exploratoire
str(recette_pays)
summary(recette_pays)
sapply(recette_pays[,-1],var) 
covariance <- cov(recette_pays[,-1])
summary(covariance)
boxplot(recette_pays[,-1])

#test de corrélation
correlation <- cor(recette_pays[,-1])
corrplot(correlation, method="circle", type="lower")
cor.mtest <- function(mat) {
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat<- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
                for (j in (i + 1):n) {
                        tmp <- cor.test(mat[, i], mat[, j])
                        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
                }
        }
        colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
        p.mat
}

p.mat<-cor.mtest(correlation)
x11()
corrplot(correlation, type="lower", order="hclust", p.mat = p.mat, sig.level = 0.05,  insig = "blank")


summary(correlation)
#Question 2:
        ##Analyse composants principales

res.pca <- PCA(recette_pays[,-1], graph = FALSE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_ca_biplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_pca_var(res.pca, col.var = "black")
fviz_pca_ind(res.pca, col.ind = "black")

        ##AFTD
library(MASS)
rec_matrice <- as.matrix(recette_pays[,-1])
rec_scaled <- scale(rec_matrice)
dissimilarite <- dist(rec_scaled, diag = TRUE)
mds <- cmdscale(dissimilarite, eig = TRUE)
plot(mds$points, main = "AFTD")
text(mds$points[,1],mds$points[,2], recette_pays[,1])
shepard <- Shepard(dissimilarite, mds$points)
plot(shepard)

#Question 3 : analyse ascendante hierarchique distance Manhattan
library(MASS)
rec_matrice <- as.matrix(recette_pays[,-1])
rec_scaled <- scale(rec_matrice)
dissimilarite <- dist(rec_scaled, method = "manhattan", diag = TRUE)
par(mfrow = c(3,2))
hc_cusine.wardd2 <- hclust(dissimilarite, method = "ward.D2")
plot(hc_cusine.wardd2, main = "Method : Ward D2")
hc_cusine.complete <- hclust(dissimilarite, method = "complete")
plot(hc_cusine.complete, main = "Method : Complete")
hc_cusine.average <- hclust(dissimilarite, method = "average")
plot(hc_cusine.average, main = "Method : Average")
hc_cusine.median <- hclust(dissimilarite, method = "median")
plot(hc_cusine.median, main = "Method : Median")
hc_cusine.centroid <- hclust(dissimilarite, method = "centroid")
plot(hc_cusine.centroid, main = "Method : Centroid")
hc_cusine.single <- hclust(dissimilarite, method = "single")
plot(hc_cusine.single, main = "Method : Single")

#Question 4 : K-means
library(cluster)
rec_matrice <- as.matrix(recette_pays[,-1])
rec_scaled <- scale(rec_matrice)
dissimilarite <- dist(rec_scaled)
rec_km <- kmeans(rec_scaled, 4)
clusplot(dissimilarite, rec_km$cluster, diss= TRUE, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmeans avec k = 4")
inertie_matrice <- matrix(0, nrow = 100, ncol = 10)
for (i in 1:10)
{
        for (j in 1:100)
        {
                inertie_matrice[j,i] <- kmeans(rec_scaled,i)$tot.withinss
        }
}

inertie <- apply(inertie_matrice,2,min)
plot(inertie, main = "Inertie intra-classe minimale en fonction de K", type = "l", xlab = "K", ylab = "Inertie intra-classe")
apply(inertie_matrice,2,var)

#Donnees recettes-echant.data

setwd("D:/Bi/utc/SY09/Projet 1")
recettes_echant <- read.table("donnees/recettes-echant.data", header = TRUE, sep = ",")

        #Question 6 : Analyse descriptive 
str(recettes_echant)
sapply(recettes_echant[,-1],var) 
covariance <- cov(recettes_echant[,-1])
summary(covariance)
correlation <- cor(recettes_echant[,-1])
summary(covariance)
barplot(apply(recettes_echant[,-1],2,sum))


        #Question 7 : 

###dissimilarite
par(mfrow = c(1,2))
rec <- t(as.matrix(recettes_echant[,-1]))
dissimilarite <- dist(rec, method = "binary", diag = TRUE)
hc_rec <- hclust(dissimilarite, method = "ward.D2")
plot(hc_rec)


#K-medoides
library(cluster)
kmedoides <- pam(dissimilarite, 6)
clusplot(as.matrix(dissimilarite), kmedoides$clustering, diss= TRUE, color = TRUE , shade = TRUE, labels = 2, lines = 0, main = "Kmedoides avec k = 4")
