#donnees recettes-pays.data


setwd("D:/Bi/utc/SY09/Projet 1")
recette_pays <- read.table("donnees/recettes-pays.data", header = TRUE, sep = ",")


#Question 1 : Analyse exploratoire
str(recette_pays)
summary(recette_pays)
sapply(recette_pays[,-1],var) 
covariance <- cov(recette_pays[,-1])
summary(covariance)
correlation <- cor(recette_pays[,-1])
summary(correlation)
boxplot(recette_pays[,-1])


#Question 2:
        ##Analyse composants principales
rec_matrice <- as.matrix(recette_pays[,-1])
Dp <- diag(x = 1/26, nrow = 26, ncol = 26)
M <- diag(x = 1, nrow = 50, ncol = 50)
rec_scaled <- scale(rec_matrice)
V <- t(rec_scaled)%*%Dp%*%rec_scaled
x <- eigen(V)
valeur_propre <- x$values
vecteur_propre <- x$vectors
positive <- valeur_propre > 0
valeur_propre <- valeur_propre[positive == TRUE]
vecteur_propre <- vecteur_propre[,1:length(valeur_propre)]
barplot(valeur_propre)
inertie_totale <- sum(valeur_propre)
pourcentage_iner_expliquee <- valeur_propre / inertie_totale
C <- rec_scaled%*%M%*%vecteur_propre
plot(C[,2]~C[,1])
text(C[,1],C[,2],recette_pays[,1])
plot(C[,3]~C[,1])
text(C[,1],C[,3],recette_pays[,1])
D <- cor(rec_scaled, C)
plot(-1:1, -1:1, type = "n", xlab = "Axe factoriel 1", ylab = "Axe factoriel 2")
text(D[, 1], D[, 2],colnames(rec_matrice))
abline(h = 0, v = 0, col = "blue")
curve(sqrt(1 - x^2), -1, 1, add = T, col = "blue")
curve(-sqrt(1 - x^2), -1, 1, add = T, col = "blue")

        ##AFTD
library(MASS)
rec_matrice <- as.matrix(recette_pays[,-1])
rec_scaled <- scale(rec_matrice)
dissimilarite <- dist(rec_scaled, diag = TRUE)
mds <- cmdscale(dissimilarite, eig = TRUE)
plot(mds$points)
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
###Transformation
rec_trans <- matrix(0, nrow = length(unique(recettes_echant$origin)), ncol = ncol(recettes_echant[,-1]))
for (i in 1:length(unique(rec_trans$origin)))
{
        for (j in 2:ncol(recettes_echant[,]))
        {
                count <- 0
                for (k in 1:nrow(recettes_echant))
                {
                        if (recettes_echant[k,1] == unique(rec_trans$origin)[i])
                                count <- count + recettes_echant[k,j]
                }
        }
}