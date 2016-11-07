# La classification hierarchique prend un tableau de distance en entrée (dissimilarité) différence fondamentale avec l'algo des k_mins qui prend un tableau individus variables
# On veut trouver des groupe homogène d'individus dans un ensemble connus
# AFTD factorisation de la matrice de distance --> representation des point
# sammon pas le meme critere --> on trouve des point qui minimise des distance entre les points de la representation initiale
# kruskal representation isotonique respecter l'ordre entre les distance

# Exercice 1
# question 1
data(iris)
resIris<-princomp(iris [1:4])
biplot(resIris)
legend(x="topright", c("setosa", "versicolor", "virginica"), col=c("red", "blue", "orange"), pch = 1)
plot(resIris$scores[,1], resIris$scores[,2], col=c("red","blue", "orange")[iris$Species])


# On constate qu'il y a deux groupes de points.
# On voit que la longeur et la largeur des pétal sont deux grandeurs corrélées.
# Avec les couleur on distingue 3 groupes
# On peut s'attendre à avoir 2 sous ensembles dans notre partition

# text permet de representer les données en indiquant les coordonnées en fonction de la localisation dans l'espace

# question 2:
 inte<-interaction(crabs2$sex,crabs2$sp)
plot(resCrabs$scores[,1], resCrabs$scores[,2], col=c("red","blue", "orange", "pink")[inte])
# On observe deux groupe sans les couleurs.
# Avec les couleurs en fonction d'une seule variable qualitative on observe deux groupes.
# En faisant en fonction du sexe et de l'espece on observe 4 groupes.

# question 3
AFTD4<-cmdscale(mut, k=4)
ge<-c("Man", "Monkey", "Dog", "Horse", "Donkey", "Pig", "Rabbit", "Kangaroo",
"Pekin Duck", "Pigeon", "Chicken", "King Penguin", "Snapping Turtle",
"Rattlesnake", "Tuna", "Screwworm Fly", "Moth", "Bakers Mould"
, "Bread Yeast", "Skin Fungus")
plot(Shepard(mut,AFTD4))
abline(0,1, col=c("red"))
text(AFTD4, labels=ge)

# Ici la matrice n'est une matrice de distance euclidienne --> valeur propre négative
# plusieurs solutions : mettre les vp neg à 0 --> on ne prend pas en compte ces vp
# sinon prendre la valeur abs des vp pour le calcul des pourcentage d'inertie

# Shepard : Regarde si les distances correspondent aux dissimilarités initiales. Si cela correspond,
# les couples de points sont sur la bissectrice.
# Si les points s'écartent, cela signifie que la dissimilarité initiale a été surestimée ou sousestimée.


# Pour k=2
valeurPropre$eig <- abs(valeurPropre$eig)
valeurPropre$eig*100/sum(valeurPropre$eig)
# Pourcentage d'inertie expliquée
#          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]      [,8]     [,9]    [,10]     [,11]
# [1,] 52.71368 16.01087 10.94417 6.715739 4.779126 3.843187 2.058288 0.6681795 0.462331 0.270726 0.1308259
#           [,12]      [,13]        [,14]      [,15]      [,16]     [,17]     [,18]     [,19]    [,20]
# [1,] 0.04086139 0.02003087 5.734725e-15 0.01698897 0.07418323 0.1414007 0.2407401 0.3536176 0.515055

plot(Shepard(mut, cmdscale(mut)), asp= 1)
plot(Shepard(mut, sammon(mut)$points), asp= 1)

# Exercice 2
library(dendextend)
color_space package

mutHclustW<-hclust(mut, method="ward.D2", members=NULL)
irisDist<-dist(iris[1:4], method = "euclidean", diag = FALSE, upper = FALSE)
irisHclustC<-hclust(irisDist, method="ward.D2", members=NULL)
classification<-diana(irisDist, diss = inherits(irisDist, "dist"))

irisDist<-dist(iris[1:4], method = "euclidean", diag = FALSE, upper = FALSE)
#hc_iris<-diana(irisDist, diss = inherits(irisDist, "dist"))
hc_iris <- hclust(irisDist, method = "complete")
#hc_iris <- ward.cluster(irisDist, peso = apply(X=myDF, MARGIN=1, FUN=sum) , plots = TRUE, h.clust = 1)
iris_species <- rev(levels(iris[,5]))
dend <- as.dendrogram(hc_iris)
dend <- rotate(dend, 1:150)
dend <- color_branches(dend, k=3)
labels_colors(dend) <-
    rainbow_hcl(3)[sort_levels_values(
       as.numeric(iris[,5])[order.dendrogram(dend)]
    )]
labels(dend) <- paste(as.character(iris[,5])[order.dendrogram(dend)],
                            "(",labels(dend),")",
                            sep = "")
dend <- hang.dendrogram(dend,hang_height=0.1)
dend <- set(dend, "labels_cex", 0.5)
 par(mar = c(3,3,3,7))
plot(dend,
 main = "Dendrogramme des données iris
 en hiérarchie descendante avec Diana",
 horiz =  FALSE,  nodePar = list(cex = .007))
legend("topright", legend = iris_species, fill = rainbow_hcl(3))



# Exercice 3

# Q1:
kmeans(iris[1:4],2)

# Q2:
kmeans(iris[1:4],3)

# Q3 :
# 3.a
classopt<- function(n, j) {
res<-matrix(0,100,9);
for(k in 2:10){
	for (i in 1:n) {
		 class<-kmeans(j,k);
		 Inertie<-class$tot.withinss
		 res[i, k-1]<-Inertie;
	}
}
res
}

# 3.b

 k<-apply(cl, 2, min)
k<-c(kmeans(iris[1:4], 1)$tot.withinss, k);
 plot(k, type="b")
# Afin de proposer un nombre de classe, on utilise la méthode du coude. Elle stipule que
# le nombre de classes optimal est obtenu lorsque l'inertie est faible et qu'elle est proche de
# l''inertie précédente.
# Dans notre cas, le nombre de classes optimal est de 3.  On peut choisir 2 ou 3.

# 4.
 table(iris$Species)

    # setosa versicolor  virginica
    #     50         50         50
# 50 individus par espèces.

# Pareil avec kmeans de 2 ou 3 :
table(class$cluster)

#  1  2
# 53 97
table(iris$Species,class$cluster)

#               1  2
#   setosa     50  0
#   versicolor  3 47
#   virginica   0 50

# Pour k = 3 :
class<-kmeans(iris[1:4], 3)
table(class$cluster)

#  1  2  3
# 50 38 62
table(iris$Species,class$cluster)

              1  2  3
  setosa     50  0  0
  versicolor  0  2 48
  virginica   0 36 14



# Données Crabs :
# 1.
repartition<- function(k, n, j) {
for (i in 1:n) {
	class<-kmeans(j,k);
	Rep<-table(interaction(crabs2$sp, crabs2$sex),class$cluster)
	print(Rep)
}
}
# Nope, on a des résultats différents.

classoptc<- function(n, j) {
res<-matrix(0,100,2);
for(k in 2:2){
	for (i in 1:n) {
		 class<-kmeans(j,k);
		 Inertie<-class$withinss
		 res[i, 1]<-Inertie[1];
		 res[i, 2]<-Inertie[2];
	}
}
res
}
tot.withinss
 table(classoptc(100, crabs2[1:4]))

# 0.258778261400495 0.356065187984229 0.357537399902646 0.358000279776913
#                81                15                 2                 1
# 0.364797665657082
#                 1
 classoptc(100, crabs2[1:4])
# withinss
table(classoptc(100, crabs2[1:4])[,1])

# 0.126809103539227 0.131969157861268 0.150537520424091 0.188195803387884
#                47                36                 9                 1
# 0.205527667560138
#                 7

# 2.
# Pour k = 4 :
class<-kmeans(crabs2[1:4], 4)
table(crabs2$sp,class$cluster)

  #    1  2  3  4
  # B  0 63 37  0
  # O 57  1  0 42
table(crabs2$sex,class$cluster)

  #    1  2  3  4
  # F 49 51  0  0
  # M  8 13 37 42
table(interaction(crabs2$sp, crabs2$sex),class$cluster)

  #      1  2  3  4
  # B.F  0 50  0  0
  # O.F 49  1  0  0
  # B.M  0 13 37  0
  # O.M  8  0  0 42

# Donnée de Mutation:

# 1
mut5<-cmdscale(mut, k=5)
kmeansMut<-kmeans(mut5, 3)
# Représentation des classes avec les centres.
plot(mut5, col=kmeansMut$cluster)
points(kmeansMut$centers, col=1:3, pch=23, cex=3)

# On obtient 3 groupe dont deux sont tres proche. Cela doit correpondre ˆ la répartition animaux/bactérie que tu as cité plus haut. On peut donc s'attendre ˆ avoir un nombre de classe optimal de 3'



repartition<- function(k, n, j) {
    + res<-matrix(0,400,k);
    + for (i in 1:n) {
        + class<-kmeans(j,k);
        + Rep<-table(mut5[,1],class$cluster)
        + 	for(y in 1:k){
            + 		for(j in 1:k){
                + 			res[i,j]<-Rep[y,j]
                + 		}
            + 	}
        + }
    + }



2
classoptc<- function(n, j) {
    res<-matrix(0,100,3);
    for(k in 3:3){
        for (i in 1:n) {
            class<-kmeans(j,k);
            Inertie<-class$withinss
            res[i, 1]<-Inertie[1];
            res[i, 2]<-Inertie[2];
            res[i, 3]<-Inertie[3];
        }
    }
    res
}
table(classoptc(100, mut5))
# 0 78.4376957709028 155.418166631632  630.52546633834 767.654341335668 770.248453269507  1511.1599617449
# 25               20               33               15                7               23               20
# 1516.20628358304 1535.86376128694   1954.066899835 2428.62790569729 2808.60116302543 2851.60116150072
# 7               15               33                2               77               23

mutmut<-apply(classopt(100,mut5),2,min)
mutmutFinal<-c(kmeans(mut5,1)$tot.withinss,mutmut)
plot(mutmutFinal, type="b")
# --> permet de choisir le nombre de classe pour représenter les données de mutations.


# Inertie Moyenne
> tata$tot.withinss
# [1] 3025.057
# Nb individus dans les clusters 2 3 15
# Inertie maximale
> tato$tot.withinss
# [1] 3491.282
# Nb individus dans les clusters 14 3 3
# Inertie minimale
> toto$tot.withinss
# [1] 2046.391
# Nb individus dans les clusters 17 2 1

sameline<-function(tab, cus) {
  for(i in 1:100){
    for(j in 1:3){
      for(c in 1:length(cus)) {
        if(tab[i][j]==cus[c][1]) {
          for(d in 1:3) {

          }
        }
      }
    }
  }
}
