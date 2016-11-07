donn <- read.table("Synth1-40.txt", header=F)
donn100 <- read.table("Synth1-100.txt", header=F)
donn500 <- read.table("Synth1-500.txt", header=F)
donn11000 <- read.table("Synth1-1000.txt", header=F)
donn21000 <- read.table("Synth2-1000.txt", header=F)

 ceuc.app <- function(Xapp, zapp)
{
mu<-matrix(0, 2, length(Xapp));
nbInd <- table(zapp);
for(i in 1:length(Xapp)){
sumXapp<-0;
sumYapp<-0;
	for(j in 1:length(Xapp[,1])) {
		if(zapp[j]==1){
			sumXapp<-sumXapp+Xapp[j,i];
		}
		else if(zapp[j]==2){
			sumYapp<-sumYapp+Xapp[j,i];
		}
	}
mu[1,i]<-sumXapp/nbInd[1];
mu[2,i]<-sumYapp/nbInd[2];
}
mu
}

#             [,1]       [,2]
# [1,]  0.11145950  1.8673620
# [2,] -0.06781616 -0.8345254


ceuc.val <- function(mu, Xtst)
{
V<-c();
d<-distXY(mu, Xtst);
V<-apply(d,2 ,which.min)
}

# 16 17 18 19 20 36 37 38 39 40
#  1  1  1  1  1  2  1  2  2  1

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{
	d<-distXY(as.matrix(Xapp), as.matrix(Xval));
	class<-matrix(0, dim(d)[2], length(nppv));
	Kopt<-matrix(0,2, length(nppv));
	comptMax<-0;
	#dsort<-c();
	for(k in 1:length(nppv)) { #Pour le nombre de voisins optimaux possibles
		#nppv de K voisins
		compt<-0;
		class<-kppv.val(Xapp, zapp, nppv[k], Xval);
		for(j in 1:length(class)) {
			if(class[j] == zval[j]) {
				compt<-compt+1;
			}
		}
			if(compt>=comptMax) {
			comptMax<-compt;
			Kopt[1,k]<- nppv[k];
			Kopt[2,k]<- compt;
			nbClass<-nppv[k];
		}
	}
	nbClass;
}

nppv = {1, 2 , 5, 6}

kppv.val <- function(Xapp, zapp, k, Xtst){
class<-c();
d<-distXY(as.matrix(Xapp), as.matrix(Xtst));
for(i in 1:length(Xtst[,1])){
	cpos<-c();
	for(j in 1:k){
			minpos<-which.min(d[,i]);
				d[minpos,i]<-NA;
				cpos<-cbind(cpos, zapp[minpos]);
	}
	freq<-table(cpos);
			if(length(freq)>1) {
				if(freq[1]>=freq[2]) {
					class<-cbind(class,1);
				}
				else {
					class<-cbind(class,2);
				}
			}
			else {
				class<-cbind(class,cpos[1]);
			}
}
class
}
# K = 1,3,5 en classes optimales

# 1.2 :
# 1.2.1 :
# µk : Espérance de la gaussienne de la classe k
# sigmak : matrice de covariance
# 40 :

#             [,1]       [,2]
# [1,] -0.04917257  1.9760054
# [2,]  0.10989991 -0.6960198

sigma40<-cov.wt(method="ML", X)
sigma40$cov
#            V1         V2
# V1  1.5028251 -0.2925628
# V2 -0.2925628  2.5693724

somme40<-table(z)
somme40
# z
#  1  2
# 23 17
pi40<-somme40/40
pi40
# z
#     1     2
# 0.575 0.425

# 100:
donn100.sep <- separ1(X100, z100)
Xapp100 <- donn100.sep$Xapp
zapp100 <- donn100.sep$zapp
Xtst100 <- donn100.sep$Xtst
ztst100 <- donn100.sep$ztst

mu100
#            [,1]      [,2]
# [1,]  0.1684090  2.059974
# [2,] -0.1872786 -1.063586

sigma100<-cov.wt(method="ML", X100)
sigma100$cov
#           V1        V2
# V1 0.9102199 0.3446472
# V2 0.3446472 3.1470043

somme100<-table(z100)
somme100
z100
#  1  2
# 46 54
pi100<-somme100/100
pi100
z100
#    1    2
# 0.46 0.54

# 500 :
donn500.sep <- separ1(X500, z500)
Xapp500 <- donn500.sep$Xapp
zapp500 <- donn500.sep$zapp
Xtst500 <- donn500.sep$Xtst
ztst500 <- donn500.sep$ztst
mu500<-ceuc.app(X500, z500)

#             [,1]       [,2]
# [1,]  0.02752285  2.0036738
# [2,] -0.03351599 -0.9013625

sigma500<-cov.wt(method="ML", X500)
sigma500$cov
#            V1         V2
# V1 0.94498940 0.03510779
# V2 0.03510779 2.98624360

somme500<-table(z500)
somme500

z500
#   1   2
# 258 242

pi500<-somme500/500
pi500

z500
#     1     2
# 0.516 0.484

> erreurEuclide500<-aleaEuclidien2(donn500)
> erreurEuclide500
#            [,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]       [,8]       [,9]      [,10]      [,11]
# [1,] 0.05405405 0.05705706 0.04804805 0.04504505 0.04804805 0.05105105 0.04504505 0.04504505 0.05405405 0.06306306 0.06906907
# [2,] 0.04790419 0.05988024 0.05988024 0.05988024 0.05988024 0.04790419 0.05988024 0.05988024 0.04191617 0.02994012 0.01796407
#           [,12]      [,13]      [,14]      [,15]      [,16]      [,17]      [,18]      [,19]      [,20]
# [1,] 0.03603604 0.04504505 0.06006006 0.04204204 0.06006006 0.05105105 0.06006006 0.05105105 0.05405405
# [2,] 0.07185629 0.05988024 0.03592814 0.07784431 0.04191617 0.04790419 0.04790419 0.05988024 0.04191617
> estim500<-estimPonct(erreurEuclide500)
> estim500
#       errappmoy  errtstmoy
# [1,] 0.05195195 0.05149701

> erreurVoisin500<-aleaVoisin2(donn500)
> estim500<-estimPonct(erreurVoisin500)
> erreurVoisin500
#            [,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]       [,8]       [,9]      [,10]     [,11]     [,12]
# [1,] 0.07200000 0.04800000 0.06400000 0.04800000 0.06800000 0.05600000 0.06800000 0.04800000 0.06000000 0.06800000 0.0560000 0.0240000
# [2,] 0.04761905 0.07936508 0.04761905 0.03968254 0.04761905 0.04761905 0.05555556 0.05555556 0.05555556 0.08730159 0.0952381 0.0952381
#           [,13]      [,14]      [,15]      [,16]      [,17]      [,18]      [,19]      [,20]
# [1,] 0.06400000 0.05600000 0.04800000 0.05200000 0.05600000 0.03600000 0.04400000 0.05200000
# [2,] 0.02380952 0.04761905 0.07142857 0.04761905 0.07142857 0.07142857 0.08730159 0.06349206
> estim500
#      errappmoy  errtstmoy
# [1,]    0.0544 0.06190476

# 11000 :
donn11000.sep <- separ1(X11000, z11000)
Xapp11000 <- donn11000.sep$Xapp
zapp11000 <- donn11000.sep$zapp
Xtst11000 <- donn11000.sep$Xtst
ztst11000 <- donn11000.sep$ztst
mu11000<-ceuc.app(X11000, z11000)

#             [,1]      [,2]
# [1,] -0.08398649  1.984757
# [2,] -0.03848676 -1.013359

sigma11000<-cov.wt(method="ML", X11000)
sigma11000$cov
#             V1          V2
# V1  1.00089859 -0.01799136
# V2 -0.01799136  3.22599880

somme11000<-table(z11000)
somme11000

z11000
#   1   2
# 481 519

pi11000<-somme11000/1000
pi11000
z11000
#     1     2
# 0.481 0.519

erreurEuclide1000
#            [,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]       [,8]       [,9]      [,10]      [,11]
# [1,] 0.05997001 0.06746627 0.05547226 0.06296852 0.06146927 0.05997001 0.05997001 0.06296852 0.06146927 0.06146927 0.07196402
# [2,] 0.07807808 0.06306306 0.08708709 0.06906907 0.07507508 0.07207207 0.07807808 0.07507508 0.07507508 0.07807808 0.05105105
#           [,12]      [,13]      [,14]      [,15]      [,16]      [,17]      [,18]      [,19]      [,20]
# [1,] 0.06446777 0.07196402 0.06596702 0.06896552 0.05397301 0.07946027 0.07046477 0.05247376 0.05697151
# [2,] 0.07207207 0.05405405 0.07207207 0.05705706 0.09309309 0.03603604 0.05705706 0.09009009 0.07807808

estim1000
#       errappmoy  errtstmoy
# [1,] 0.06349325 0.07057057

erreurVoisin11000<-aleaVoisin2(donn11000)
estim11000<-estimPonct(erreurVoisin11000)
erreurVoisin11000
#       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
# [1,] 0.062 0.090 0.058 0.084 0.064 0.046 0.052 0.058 0.060 0.060 0.060 0.036 0.060 0.036 0.048 0.056 0.042 0.068 0.072 0.048
# [2,] 0.080 0.056 0.080 0.048 0.080 0.064 0.080 0.072 0.088 0.072 0.064 0.068 0.072 0.092 0.088 0.076 0.060 0.056 0.060 0.096
estim11000
     errappmoy errtstmoy
# [1,]     0.058    0.0726

# 21000 :
donn21100.sep <- separ1(X21100, z21100)
Xapp21100 <- donn21100.sep$Xapp
zapp21100 <- donn21100.sep$zapp
Xtst21100 <- donn21100.sep$Xtst
ztst21100 <- donn21100.sep$ztst

# Écrire un script qui effectue N = 20 séparations aléatoires de chaque jeu de données en
# ensembles d’apprentissage et de test, et qui calcule (et stocke) pour chacune le taux d’erreur
# d’apprentissage et le taux d’erreur de test. On pourra utiliser la fonction separ1 pour séparer
# les données


aleaEuclidien<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,1:2]
	z <- donnees[,3]
	for(i in 1:20) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp
		zapp <- donnees.sep$zapp
		Xtst <- donnees.sep$Xtst
		ztst <- donnees.sep$ztst
		mu<-ceuc.app(Xapp, zapp);
		ClassApp<-c();
		ClassTst<-c();
		ClassApp<-ceuc.val(mu, Xapp);
		ClassTst<-ceuc.val(mu, Xtst);
		for(j in 1:length(ClassApp)) {
			if(ClassApp[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(j in 1:length(ClassTst)) {
			if(ClassTst[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
	}
	terrapp<-comptapp/(length(ClassApp)*20);
	terrtst<-compttst/(length(ClassTst)*20);
	res<-cbind(terrapp, terrtst);
	res
}

#permet d'avoir les taux d'erreur pour chaque répétition stocké dans une matrice

aleaEuclidien2<-function(donnees) {
	terrorapp<-c();
	terrortst<-c();
	X <- donnees[,1:2]
	z <- donnees[,3]
	res<-matrix(0,2,20);
	for(i in 1:20) {
		donnees.sep <- separ1(X, z)
		comptapp<-0;
		compttst<-0;
		Xapp <- donnees.sep$Xapp
		zapp <- donnees.sep$zapp
		Xtst <- donnees.sep$Xtst
		ztst <- donnees.sep$ztst
		mu<-ceuc.app(Xapp, zapp);
		ClassApp<-c();
		ClassTst<-c();
		ClassApp<-ceuc.val(mu, Xapp);
		ClassTst<-ceuc.val(mu, Xtst);
		for(j in 1:length(ClassApp)) {
			if(ClassApp[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		terrorapp[i]<-comptapp/(length(ClassApp));
		res[1,i]<-terrorapp[i];
		for(j in 1:length(ClassTst)) {
			if(ClassTst[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
		terrortst[i]<-compttst/(length(ClassTst));
		res[2,i]<-terrortst[i];
	}
	res
}
aleaEuclidien2(donn)

#En considérant ensuite l’ensemble des résultats obtenus lors des N = 20 expériences, donner
#l’estimation ponctuelle du taux d’erreur epsilon ainsi qu’un intervalle de confiance, d’abord à partir
#des estimations faites sur l’ensemble d’apprentissage, puis celles faites sur l’ensemble de test.
#Qu’observez-vous ?

estimPonct<-function(erreurs) {
	errapp<-erreurs[1,];
	errtst<-erreurs[2,];
	errappmoy<-mean(errapp);
	errtstmoy<-mean(errtst);
	estim<-cbind(errappmoy, errtstmoy);
	estim;
}

intconf<-function(erreurs, estim) {
	intconfiance<-matrix(0,2,2);
	intconfiance[1,1]<-estim[1]-((sd(erreurs[1,])/sqrt(length(erreurs[,1])))*qt(0.975, length(erreurs[,1])))
	intconfiance[1,2]<-estim[1]+((sd(erreurs[1,])/sqrt(length(erreurs[,1])))*qt(0.975, length(erreurs[,1])))
	intconfiance[2,1]<-estim[2]-((sd(erreurs[2,])/sqrt(length(erreurs[,1])))*qt(0.975, length(erreurs[,1])))
	intconfiance[2,2]<-estim[2]+((sd(erreurs[2,])/sqrt(length(erreurs[,1])))*qt(0.975, length(erreurs[,1])))
	intconfiance;

}

# question 3
test3<-kppv.tune(Xapp, zapp, Xapp, zapp, nppv)
# nb classe optimale =1 ca me parait logique car on compare les mêmes ensembles.
# Avec 1 on affecte l'étiquette qui correspond au plus proche voisin c'est à dire le point lui meme.

# question 4 #script pour le classifieur des plus proche voisins

aleaVoisin<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	for(i in 1:20) {
		donnees.sep <- separ2(X, z)
		Xapp <- donnees.sep$Xapp
		zapp <- donnees.sep$zapp
		Xval <- donnees.sep$Xval
		zval <- donnees.sep$zval
		Xtst <- donnees.sep$Xtst
		ztst <- donnees.sep$ztst
		ClassApp<-c();
		ClassTst<-c();
		Kopt <- kppv.tune(Xapp, zapp, Xval, zval, 1:10)
		ClassApp <- kppv.val(Xapp, zapp, Kopt, Xapp)
		ClassTst <- kppv.val(Xapp, zapp, Kopt, Xtst)

		for(j in 1:length(ClassApp)) {
			if(ClassApp[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(y in 1:length(ClassTst)) {
			if(ClassTst[y] != ztst[y]) {
				compttst<-compttst+1;
			}
		}

	}
	terrapp<-comptapp/(length(ClassApp)*20);
	terrtst<-compttst/(length(ClassTst)*20);
	res<-cbind(terrapp, terrtst);
	res
}

aleaVoisin2<-function(donnees) {
	terrorapp<-c();
	terrortst<-c();
	X <- donnees[,1:2]
	z <- donnees[,3]
	res<-matrix(0,2,20);
	for(i in 1:20) {
		donnees.sep <- separ2(X, z)
		comptapp<-0;
		compttst<-0;
		Xapp <- donnees.sep$Xapp
		zapp <- donnees.sep$zapp
		Xval <- donnees.sep$Xval
		zval <- donnees.sep$zval
		Xtst <- donnees.sep$Xtst
		ztst <- donnees.sep$ztst
		ClassApp<-c();
		ClassTst<-c();
		Kopt <- kppv.tune(Xapp, zapp, Xval, zval, 1:10)
		ClassApp <- kppv.val(Xapp, zapp, Kopt, Xapp)
		ClassTst <- kppv.val(Xapp, zapp, Kopt, Xtst)

		for(j in 1:length(ClassApp)) {
			if(ClassApp[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		terrorapp[i]<-comptapp/(length(ClassApp));
		res[1,i]<-terrorapp[i];

		for(y in 1:length(ClassTst)) {
			if(ClassTst[y] != ztst[y]) {
				compttst<-compttst+1;
			}
		}
		terrortst[i]<-compttst/(length(ClassTst));
		res[2,i]<-terrortst[i];

	}
	res
}

# question sur le jeu de données synth-2-1000
# question 1)
 donn21000 <- read.table("Synth2-1000.txt", header=F)
 X21000 <- donn21000[,1:2]
 z21000 <- donn21000[,3]
 donn21000.sep <- separ2(X21000, z21000)
 Xapp21000 <- donn21000.sep$Xapp
 zapp21000 <- donn21000.sep$zapp
 Xtst21000 <- donn21000.sep$Xtst
 ztst21000 <- donn21000.sep$ztst
 mu2000<-ceuc.app(Xapp, zapp)
 mu2000
#             [,1]      [,2]
# [1,] -0.08117442  2.972041
# [2,] -0.02142483 -5.093154

 sigma21000<-cov.wt(method="ML", X21000)
 sigma21000
$cov
#            V1          V2
# V1 3.19503880  0.08422374
# V2 0.08422374 19.34216966

 somme21000<-table(z2100)
 somme21000
z
#   1   2
# 486 514

 pi21000<-somme21000/1000
 pi21000
z
#     1     2
# 0.486 0.514

# question 2)
#            [,1]       [,2]       [,3]       [,4]       [,5]       [,6]       [,7]       [,8]       [,9]      [,10]      [,11]
# [1,] 0.02248876 0.01799100 0.02098951 0.01799100 0.02098951 0.01949025 0.01649175 0.01649175 0.01949025 0.02548726 0.02698651
# [2,] 0.01801802 0.03003003 0.02402402 0.03003003 0.02402402 0.02702703 0.03003003 0.03303303 0.02702703 0.01501502 0.01201201
#           [,12]      [,13]      [,14]      [,15]      [,16]      [,17]      [,18]      [,19]      [,20]
# [1,] 0.01649175 0.02398801 0.02398801 0.02098951 0.01949025 0.02398801 0.02248876 0.02398801 0.01949025
# [2,] 0.03003003 0.01801802 0.01801802 0.02402402 0.02702703 0.01801802 0.02102102 0.01801802 0.02702703
 estim
      # errappmoy  errtstmoy
# [1,] 0.02098951 0.02357357

 estimVoisin
#      errappmoy errtstmoy
# [1,]    0.0052    0.0096
 erreurVoisin
#      terrapp terrtst
# [1,]       0    0.12
 erreurVoisin2
#       [,1]  [,2]  [,3]  [,4]  [,5]  [,6] [,7]  [,8]  [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19] [,20]
# [1,] 0.000 0.004 0.006 0.008 0.006 0.006    0 0.002 0.006 0.012 0.012 0.006 0.002 0.002 0.008 0.006 0.002 0.008 0.008 0.000
# [2,] 0.004 0.012 0.008 0.004 0.016 0.012    0 0.012 0.008 0.004 0.004 0.020 0.016 0.004 0.008 0.012 0.008 0.000 0.024 0.016


# Exercice de Bayes

# 1) distribution marginale des variables X1 X2 dans chaque classe:

# fX1(x1)= f(x1)integrale sur X2 de f(x2)dx.
# fonction de densité de la loi normal bivariée cf poly page94
# calcul du déterminant sur R det(matrice)
# calcul de l'inverse d'une matrice solve(matrice)

# Pour la classe 1
# N(0,1) pour X1
# N(2,1) pour X2
# sigma carré = 1
# et mu est le vecteur 0,2

# Pour la classe 2
# N(0,1) pour X1
# N(-1,1) pour X2
# sigma carré = 1
# et mu est le vecteur 0,-1

# 2) calcul loi jointe = k
#  Un point MM de coordonnées (x;y)(x;y) appartient à CC si et seulement si
# (x–a)2+(y–b)2=r2

# 4)
 donn <- read.table("Synth1-40.txt", header=F)
 X <- donn[,1:2]
 z <- donn[,3]
 donn40.sep <- separ1(X, z)
 Xapp40 <- donn40.sep$Xapp
 zapp40 <- donn40.sep$zapp
 Xtst40 <- donn40.sep$Xtst
 ztst40 <- donn40.sep$ztst
 plot(Xapp40, col=c("red","green","blue","magenta","orange")[zapp40], asp=1)
 abline(h=1/2)
 title("Représentation des donnée Synth.1-1000 avec une frontière de décision Y=1/2")

 donn100 <- read.table("Synth1-100.txt", header=F)
 X100 <- donn100[,1:2]
 z100 <- donn100[,3]
 donn100.sep <- separ1(X100, z100)
 Xapp100 <- donn100.sep$Xapp
 zapp100 <- donn100.sep$zapp
 Xtst100 <- donn100.sep$Xtst
 ztst100 <- donn100.sep$ztst
 plot(Xapp100, col=c("red","green","blue","magenta","orange")[zapp100], asp=1)
 abline(h=1/2)
 title("Représentation des donnée Synth.1-100 avec une frontière de décision Y=1/2")

 donn500 <- read.table("Synth1-500.txt", header=F)
 X500 <- donn500[,1:2]
 z500 <- donn500[,3]
 donn500.sep <- separ1(X500, z500)
 Xapp500 <- donn500.sep$Xapp
 zapp500 <- donn500.sep$zapp
 Xtst500 <- donn500.sep$Xtst
 ztst500 <- donn500.sep$ztst
 plot(Xapp500, col=c("red","green","blue","magenta","orange")[zapp500], asp=1)
 abline(h=1/2)
 title("Représentation des donnée Synth.1-500 avec une frontière de décision Y=1/2")


 donn1000 <- read.table("Synth1-1000.txt", header=F)
 X1000 <- donn1000[,1:2]
 z1000 <- donn1000[,3]
 donn1000.sep <- separ1(X1000, z1000)
 Xapp1000 <- donn1000.sep$Xapp
 zapp1000 <- donn1000.sep$zapp
 Xtst1000 <- donn1000.sep$Xtst
 ztst1000 <- donn1000.sep$ztst
 plot(Xapp1000, col=c("red","green","blue","magenta","orange")[zapp1000], asp=1)
 abline(h=1/2)
 title("Représentation des donnée Synth.1-1000 avec une frontière de décision Y=1/2")
