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
recette_pays <- read.table("recettes-pays.data", header = TRUE, sep = ",", row.names = "origin")


#Question 1 : Analyse exploratoire
str(recette_pays)
summary(recette_pays)
sapply(recette_pays,var) 
covariance <- cov(recette_pays)
summary(covariance)

#test de corrélation
correlation <- cor(recette_pays)
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

#observation des boxplots selon toutes les variables
x11()
boxplot(recette_pays[,2:17], ylim=c(0,1), main="Boxplot 1")
boxplot(recette_pays[,17:28], ylim=c(0,1),main="Boxplot 2")
boxplot(recette_pays[,29:40],ylim=c(0,1), main="Boxplot 3")
boxplot(recette_pays[,41:50],ylim=c(0,1), main="Boxplot 4")
summary(correlation)

#Question 2:
        ##Analyse composants principales

res.pca <- PCA(recette_pays, graph = FALSE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_ca_biplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_pca_var(res.pca, col.var = "black")
fviz_pca_ind(res.pca, col.ind = "black")
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE )# Évite le chevauchement de texte
              
fviz_pca_ind(res.pca,
             geom.ind = "point",  scale(1,1),# colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Couleur des variables
                col.ind = "#696969"  # Couleur des individus
)
        ##AFTD
library(MASS)
rec_matrice <- as.matrix(recette_pays,diag=TRUE, upper=TRUE) # -1/2 Q* D^2*Q
rec_scaled <- scale(rec_matrice) #centrage de la matrice
dissimilarite <- dist(rec_scaled, diag = TRUE)
mds <- cmdscale(dissimilarite, eig = TRUE,k=5)

      ###Affichage des plots
x11()
par(mfrow=c(1,3))

plot(mds$points[,1]~mds$points[,2], col=rainbow(ncol(recette_pays)), xlab="Axe 1", ylab="Axe 2")
text(mds$points[,2],mds$points[,1],  row.names(recette_pays))
plot(mds3$points[,1]~mds3$points[,3], col=rainbow(ncol(recette_pays)), xlab="Axe 1", ylab="Axe 2")
text(mds3$points[,3],mds3$points[,1],  row.names(recette_pays))
plot(mds5[,1]~mds5[,5], col=rainbow(ncol(recette_pays)), xlab="Axe 1", ylab="Axe 2")
text(mds5$points[,5],mds5$points[,1],  row.names(recette_pays))


    ###observation des diagrammes de shepard pour différentes valeur de k
s2=Shepard(dissimilarite,mds2$points[,1:2])
s3=Shepard((dissimilarite),mds3$points[,c(1,3)])
s5=Shepard((dissimilarite),mds5$points[,c(1,5)]))

x11()
par(mfrow=c(1,4))
plot(s2, col=rainbow(ncol(recette_pays)), xlab="Dissimilarité", ylab="Distance", main="d=2")
abline(0, 1)
plot(s3, col=rainbow(ncol(recette_pays)), xlab="Dissimilarité", ylab="Distance", main="d=3")
abline(0, 1)
plot(s5, col=rainbow(ncol(recette_pays)), xlab="Dissimilarité", ylab="Distance", main="d=5")
abline(0, 1)

 
#Question 3 : analyse ascendante hierarchique distance Manhattan
library(MASS)
rec_matrice <- as.matrix(recette_pays)
rec_scaled <- scale(rec_matrice)
dissimilarite <- dist(rec_scaled, method = "manhattan", diag = TRUE) #problème: a t'on des distances euclidiennes ?
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
plot(hc_cusine.single, main = "Method : Single") #Nous avons tenté plusieurs méthodes pour explorer/ on garde la méthode de ward

#Question 4 : K-means
library(cluster)
rec_matrice <- as.matrix(recette_pays)
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
