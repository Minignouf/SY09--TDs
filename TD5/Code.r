# Rapport de TP 4. Last but not least!!
setwd("SY09/TD5/fonctions-tp4")

source("mvdnorm.r")
source("prob.ad.r")
source("prob.log.r")
source("prob.log2.r")
#source("separ1.r")

setwd("SY09/TD5/donnees-tp4")
Donn <- read.csv("spam.csv", header=T)
donn1.1000 <- read.table("Synth1-1000.txt", header=T)
X1.1000 <- donn1.1000[,1:2]
z1.1000 <- donn1.1000[,3]
Xapp1.1000 <- X1.1000[c(1:370,495:878),]
zapp1.1000 <- z1.1000[c(1:370,495:878)]
Xtst1.1000 <- X1.1000[c(371:494,879:1000),]
ztst1.1000 <- z1.1000[c(371:494,879:1000)]
donn2.1000 <- read.table("Synth2-1000.txt", header=T)
X2.1000 <- donn2.1000[,1:2]
z2.1000 <- donn2.1000[,3]
Xapp2.1000 <- X2.1000[c(1:375,503:874),]
zapp2.1000 <- z2.1000[c(1:375,503:874)]
Xtst2.1000 <- X2.1000[c(376:502,875:1000),]
ztst2.1000 <- z2.1000[c(376:502,875:1000)]
donn3.1000 <- read.table("Synth3-1000.txt", header=T)
X3.1000 <- donn3.1000[,1:2]
z3.1000 <- donn3.1000[,3]
Xapp3.1000 <- X3.1000[c(1:388,519:879),]
zapp3.1000 <- z3.1000[c(1:388,519:879)]
Xtst3.1000 <- X3.1000[c(389:518,880:1000),]
ztst3.1000 <- z3.1000[c(389:518,880:1000)]
setwd("SY09/TD4/fonctions-tp3")
source("separ1.r")


Xapp1<-regressionLogQuadratique(Xapp1.1000)
Xapp2<-regressionLogQuadratique(Xapp2.1000)
Xapp3<-regressionLogQuadratique(Xapp3.1000)
Xtst3<-regressionLogQuadratique(Xtst3.1000)
Xtst2<-regressionLogQuadratique(Xtst2.1000)
Xtst1<-regressionLogQuadratique(Xtst1.1000)
 param<-log.app(Xapp3.1000, zapp3.1000, 1, 1e-5)
prob.log2(param, Xtst3.1000, ztst3.1000, 0.5)
# Exercice 1
# l’analyse discriminante quadratique, l’analyse discriminante
# linéaire, et le classifieur bayésien naïf.
# 1.1
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

sigmaf<-function(Xapp, zapp, mu){
nbInd <- table(zapp);
sigmaFinal1<-matrix(0, 2, length(Xapp))
sigmaFinal2<-matrix(0, 2, length(Xapp))
 for(i in 1:length(Xapp)){
	sigm1<-0;
	sigm2<-0;
	 for(j in 1:length(Xapp[,1])) {
		if(zapp[j]==1){
			sigm1<-sigm1 + t(as.matrix(Xapp[j,])-mu[1,])%*%(as.matrix(Xapp[j,])-mu[1,]);
		}
		else{
			sigm2<-sigm2 +t(as.matrix(Xapp[j,])-mu[2,])%*%(as.matrix(Xapp[j,])-mu[2,]);
		}
	 }

 }
sigmaFinal1<-sigm1/nbInd[1];
sigmaFinal2<-sigm2/nbInd[2];
sig<-list(sigmaFinal1 = sigmaFinal1, sigmaFinal2 = sigmaFinal2)
sig
}


adq.app<-function(Xapp, zapp){
pi<-table(zapp)/length(zapp);
mu<-matrix(0, 2, length(Xapp));
param<-c();
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
sigmaFinal1<-matrix(0, 2, length(Xapp))
sigmaFinal2<-matrix(0, 2, length(Xapp))
 for(i in 1:length(Xapp)){
	sigm1<-0;
	sigm2<-0;
	 for(j in 1:length(Xapp[,1])) {
		if(zapp[j]==1){
			sigm1<-sigm1 + t(as.matrix(Xapp[j,])-mu[1,])%*%(as.matrix(Xapp[j,])-mu[1,]);
		}
		else{
			sigm2<-sigm2 +t(as.matrix(Xapp[j,])-mu[2,])%*%(as.matrix(Xapp[j,])-mu[2,]);
		}
	 }

 }
sigmaFinal1<-as.array((sigm1/nbInd[1])*(nbInd[1]/(nbInd[1]-1)));
sigmaFinal2<-as.array((sigm2/nbInd[2])*(nbInd[2]/(nbInd[2]-1)));
sig<-list(sigmaFinal1 = sigmaFinal1, sigmaFinal2 = sigmaFinal2);
param$proportion<-pi;
param$mu<-mu;
param$sigmak<-sig
param;
}

adl.app<-function(Xapp, zapp) {
pi<-table(zapp)/length(zapp);
mu<-matrix(0, 2, length(Xapp));
nbInd <- table(zapp);
param<-c();
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
sigmaFinal1<-matrix(0, 2, length(Xapp))
sigmaFinal2<-matrix(0, 2, length(Xapp))
 for(i in 1:length(Xapp)){
	sigm1<-0;
	sigm2<-0;
	 for(j in 1:length(Xapp[,1])) {
		if(zapp[j]==1){
			sigm1<-sigm1 + t(as.matrix(Xapp[j,])-mu[1,])%*%(as.matrix(Xapp[j,])-mu[1,]);
		}
		else{
			sigm2<-sigm2 +t(as.matrix(Xapp[j,])-mu[2,])%*%(as.matrix(Xapp[j,])-mu[2,]);
		}
	 }

 }
sigmaFinal1<-(sigm1/nbInd[1])*(nbInd[1]/(nbInd[1]-1));
sigmaFinal2<-(sigm2/nbInd[2])*(nbInd[2]/(nbInd[2]-1));
sig<-(1/(length(Xapp[,1])-2))*(((nbInd[1]-1)*sigmaFinal1)+((nbInd[2]-1)*sigmaFinal2))
sig<-list(sigmaFinal1 = sig, sigmaFinal2 = sig);
param$proportion<-pi;
param$mu<-mu;
param$sigmak<-sig
param;
}

nba.app<-function(Xapp, zapp) {
	pi<-table(zapp)/length(zapp);
	mu<-matrix(0, 2, length(Xapp));
	nbInd <- table(zapp);
	param<-c();
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

	sigmaFinal1<-matrix(0, 2, length(Xapp))
sigmaFinal2<-matrix(0, 2, length(Xapp))
 for(i in 1:length(Xapp)){
	sigm1<-0;
	sigm2<-0;
	 for(j in 1:length(Xapp[,1])) {
		if(zapp[j]==1){
			sigm1<-sigm1 + t(as.matrix(Xapp[j,])-mu[1,])%*%(as.matrix(Xapp[j,])-mu[1,]);
		}
		else{
			sigm2<-sigm2 +t(as.matrix(Xapp[j,])-mu[2,])%*%(as.matrix(Xapp[j,])-mu[2,]);
		}
	 }

 }
sigmaFinal1<-diag((sigm1/nbInd[1])*(nbInd[1]/(nbInd[1]-1)));
sigmaFinal2<- diag((sigm2/nbInd[2])*(nbInd[2]/(nbInd[2]-1)));
sig<-list(sigmaFinal1 = diag(sigmaFinal1), sigmaFinal2 = diag(sigmaFinal2))
param$proportion<-pi;
param$mu<-mu;
param$sigmak<-sig
param;
}

ad.val<-function(param, Xtst){
	densite1<-mvdnorm(Xtst,param$mu[1,],as.matrix(param$sigmak$sigmaFinal1));
	densite2<-mvdnorm(Xtst,param$mu[2,],as.matrix(param$sigmak$sigmaFinal2));
	etiquette<-c();
	prob1<-(densite1*param$proportion[1])/(densite1*param$proportion[1] + densite2*param$proportion[2] )
	prob2<-(densite2*param$proportion[2])/(densite1*param$proportion[1] + densite2*param$proportion[2] )
	densite<-matrix(0,length(Xtst[,1]),2)
	for(i in 1:length(prob1)){

		if (prob1[i]>prob2[i]){
			etiquette[i]<-1

		}else{
			etiquette[i]<-2
		}
	}
	densite[,1]<-prob1
	densite[,2]<-prob2
	res<-list(etiquette=etiquette, prob=densite);
	res;
}

prob.ad(param, Xtst, etiquette, niveaux);

# question 1.2

log.app<-function(Xapp, zapp, intr, epsi){

	logarithme<-0;
	tab<-c();
	pTot<-c();
	nbIteration<-0;
	diff<-Inf;

	for(j in 1:length(Xapp[,1])){
		if(zapp[j]==1){
			tab[j]<-1;
		}else{
			tab[j]<-0;
		}
	}

	#si on ajoute une ordonnée à l'origine alors beta est de dimension n+1*1 sinon beta est de dimension n*1
	if(intr==1){
		beta<-matrix(0,dim(Xapp)[2]+1,1);
		Xapp<-cbind(rep(1,dim(Xapp)[1]), Xapp);
	}else{
		beta<-matrix(0,dim(Xapp)[2],1);
	}
	while(epsi < diff){
		print("pascoucou")
		MXapp<-as.matrix(Xapp);
		pTot<-post.pr(beta,MXapp);
		gradient<-t(MXapp)%*%(tab-pTot);
		W<-diag(as.vector(pTot*(1-pTot)));
		matHessienne<- -t(MXapp)%*%W%*%MXapp;
		wpred<-beta;
		beta<-beta - ginv(matHessienne)%*%gradient;
		diff<-sqrt(sum((wpred-beta)^2));
		nbIteration<-nbIteration+1;
	}
	logarithme<-sum(tab*(MXapp%*%beta)-log(1+exp(MXapp%*%beta)));
	resultat<-list(beta=beta, nbIteration=nbIteration, logarithme=logarithme);
	resultat
}


log.val<-function(Xtst, beta){
	class<-c();
	#test pour savoir si on doit ajouter une ordonnée à Xtst
	if(length(beta)!=length(Xtst[1,])){
		Xtst<-cbind(rep(1,dim(Xtst)[1]), Xtst);
	}
	 Xtst<-as.matrix(Xtst);
	  beta<-as.matrix(beta);
	  probaPosteriori1<-post.pr(beta, Xtst);
	 probaPosteriori2<-1-probaPosteriori1;

	 for(i in 1:length(probaPosteriori1[,1])){
	 	if(probaPosteriori1[i]>probaPosteriori2[i]){
	 		class[i]<-1;
	 	}else{
	 		class[i]<-2;
	 	}
	 }
	 matriceProb<-cbind(probaPosteriori1,probaPosteriori2)
	 result<-list(prob=matriceProb, etiquette=class)
	 result;
}
prob.log(param, Xtst1.1000, ztst1.1000, 0.5);
regressionLogQuadratique<-function(X){

	X<-cbind(X,X[,1]*X[,2],X[,1]^2,X[,2]^2);
	X;
}

post.pr<-function(beta, X){
	somme<-X%*%beta;
	probaPosteriorie<-exp(somme)/(1+exp(somme));
	probaPosteriorie;
}

# Exercie2

# 2.1

aleaAnalyseDiscriminateQuadratique<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,2:58]
	z <- donnees[,59]
	for(i in 1:20) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		ztst <- donnees.sep$ztst;
		param<-adq.app(Xapp, zapp);
		ClassTst<-ad.val(param, Xtst);
		ClassApp<-ad.val(param, Xapp);
		for(j in 1:length(ClassApp$prob[,1])) {
			if(ClassApp$etiquette[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(j in 1:length(ClassTst$prob[,1])) {
			if(ClassTst$etiquette[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
	}
	terrapp<-comptapp/(length(ClassApp$prob[,1])*20);
	terrtst<-compttst/(length(ClassTst$prob[,1])*20);
	res<-cbind(terrapp, terrtst);
	res
}
aleaAnalyseDiscriminateQuadratique(Donn)
aleaAnalyseDiscriminateLineaire<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,2:58]
	z <- donnees[,59]
	for(i in 1:20) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		ztst <- donnees.sep$ztst;
		param<-adl.app(Xapp, zapp)
		ClassTst<-ad.val(param, Xtst);
		ClassApp<-ad.val(param, Xapp);
		for(j in 1:length(ClassApp$prob[,1])) {
			if(ClassApp$etiquette[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(j in 1:length(ClassTst$prob[,1])) {
			if(ClassTst$etiquette[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
	}
	terrapp<-comptapp/(length(ClassApp$prob[,1])*20);
	terrtst<-compttst/(length(ClassTst$prob[,1])*20);
	res<-cbind(terrapp, terrtst);
	res
}

aleaAnalyseDiscriminateNaif<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,1:7]
	z <- donnees[,8]
	for(i in 1:100) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		ztst <- donnees.sep$ztst;
		param<-nba.app(Xapp, zapp)
		ClassTst<-ad.val(param, Xtst);
		ClassApp<-ad.val(param, Xapp);

		for(j in 1:length(ClassApp$prob[,1])) {
			if(ClassApp$etiquette[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(j in 1:length(ClassTst$prob[,1])) {
			if(ClassTst$etiquette[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
	}
	terrapp<-comptapp/(length(ClassApp$prob[,1])*100);
	terrtst<-compttst/(length(ClassTst$prob[,1])*100);
	res<-cbind(terrapp, terrtst);
	res
}

aleaRegressionLogistique<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,2:58]
	z <- donnees[,59]
	for(i in 1:20) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		ztst <- donnees.sep$ztst;
		param<-log.app(Xapp, zapp, 1, 1e-5);
		ClassTst<-log.val(Xtst, param$beta);
		ClassApp<-log.val(Xapp, param$beta);
		for(j in 1:length(ClassApp$etiquette)) {
			if(ClassApp$etiquette[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(j in 1:length(ClassTst$etiquette)) {
			if(ClassTst$etiquette[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
	}
	terrapp<-comptapp/(length(ClassApp$etiquette)*20);
	terrtst<-compttst/(length(ClassTst$etiquette)*20);
	res<-cbind(terrapp, terrtst);
	res
}
aleaRegressionLogistique(Donn)

aleaRegressionLogQuadratique<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,1:7]
	z <- donnees[,8]
	for(i in 1:20) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		ztst <- donnees.sep$ztst;
		Xapp<-regressionLogQuadratique(Xapp);
		param<-log.app(Xapp, zapp, 1, 1e-5);
		Xtst<-regressionLogQuadratique(Xtst);
		ClassTst<-log.val(Xtst, param$beta);
		ClassApp<-log.val(Xapp, param$beta);
		for(j in 1:length(ClassApp$etiquette)) {
			if(ClassApp$etiquette[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(j in 1:length(ClassTst$etiquette)) {
			if(ClassTst$etiquette[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
	}
	terrapp<-comptapp/(length(ClassApp$etiquette)*20);
	terrtst<-compttst/(length(ClassTst$etiquette)*20);
	res<-cbind(terrapp, terrtst);
	res
}

Dapp<-cbind(Xapp1.1000, class=as.factor(zapp1.1000))
tree(Dapp$class~Dapp$V1+Dapp$V2,control=tree.control(nobs=dim(Dapp)[1],mindev = 0.0001))
plot(tree)
text(tree)
aleaArbre<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,1:7]
	z <- donnees[,8]
	for(i in 1:100) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		#Xtst2 <- data.frame(V1 = donnees.sep$Xtst[,1], V2 = donnnees.sep$Xtst[,2]);
		ztst <- donnees.sep$ztst;
		#Dapp<-cbind(Xapp, classa=as.factor(zapp))
		Dapp<-cbind(Xapp, class = as.factor(zapp))
		#Dtst<-cbind(Xtst, classt=as.factor(ztst))
		tapp<-tree(Dapp$class~., data=Dapp,control=tree.control(nobs=dim(Dapp)[1],mindev = 0.0001));

	 	#cvapp<-cv.tree(tapp, FUN=prune.misclass)
	 	cvapp<-cv.tree(tapp);
	 	 min<-min(cvapp$dev);
	 	 for(i in 1:length(cvapp$size)) {
	 	 	if(cvapp$dev[i]==min && cvapp$size[i]!=1) {
	 	 		node<-cvapp$size[i]
	 	 	}
	 	 }
	 	 papp<- prune.misclass(tapp, best = node);
	 	#ptst<- prune.misclass(ttst, best = node);
	 	#papp<-prune.misclass(tapp, k=cvapp$k)
	  	ClassAppEt<-predict(papp, Dapp, type="class"); #Etiquettes classes app
	   	ClassTstEt<-predict(papp, newdata=Xtst, type="class"); #Etiquettes classes tst

	    	for(j in 1:length(ClassAppEt)) {
	    	#Si classe 1 > class 2 ... Sinon
	    		if(ClassAppEt[j] != zapp[j]) {
	    			comptapp<-comptapp+1;
	    		}
	    	}
	    	for(j in 1:length(ClassTstEt)) {
	    	#Si classe 1 > class 2 ... Sinon
	    		if(ClassTstEt[j] != ztst[j]) {
	    			compttst<-compttst+1;
	    		}
	    	}
	   }
	    terrapp<-comptapp/(length(ClassAppEt)*100);
	    terrtst<-compttst/(length(ClassTstEt)*100);
	    res<-cbind(terrapp, terrtst);
	    res
}
aleaArbre(Donn)
aleaArbre(donn1.1000);

# pima.ltr <- tree(factor(zapp) ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V7+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57, Xapp, control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001))
# 	pima.cv <- cv.tree(pima.ltr, , prune.tree);
# 	pima.prune <- prune.misclass(pima.ltr, best=min(pima.cv$size[which(pima.cv$dev == min(pima.cv$dev))]))
# 	pima.predict <- predict(pima.prune, Xtst)

# etiquette <- c(0,0)
# 	res <- separ1(X, z);
# 	Xapp <- res$Xapp;
# 	zapp <- res$zapp;
# 	Xtst <- res$Xtst;
# 	ztst <- res$ztst;

# 	synth1.ltr <- tree(factor(zapp) ~ V1+V2, Xapp, control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001))
# 	synth1.cv <- cv.tree(synth1.ltr, , prune.tree);
# 	synth1.prune <- prune.misclass(synth1.ltr, best=min(synth1.cv$size[which(synth1.cv$dev == min(synth1.cv$dev))]))
# 	synth1.predict <- predict(synth1.prune, Xtst)


setwd("SY09/TD5/donnees-tp4")
Donn <- read.csv("Pima.csv", header=T)
Donn <- read.csv("spam.csv", header=T)
X<- Donn[,1:7]
z<-Donn[,8]
aleaRegressionLogQuadratiquePima<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,1:7]
	z <- donnees[,8]
	for(i in 1:100) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		ztst <- donnees.sep$ztst;

		Xapp2 <- Xapp
		Xtst2 <- Xtst
		for (p in 1:(dim(Xapp)[2]-1))
		{
		    for (q in (p+1):dim(Xapp)[2])
		    {
		        Xapp2 <- cbind(Xapp2, Xapp[,p]*Xapp[,q])
		        Xtst2 <- cbind(Xtst2, Xtst[,p]*Xtst[,q])
		    }
		}
		for (p in 1:dim(Xapp)[2])
		{
		    Xapp2 <- cbind(Xapp2, Xapp[,p]^2)
		    Xtst2 <- cbind(Xtst2, Xtst[,p]^2)
		}
		param<-log.app(Xapp2, zapp, 1, 1e-5);
		ClassTst<-log.val(Xtst2, param$beta);
		ClassApp<-log.val(Xapp2, param$beta);
		for(j in 1:length(ClassApp$etiquette)) {
			if(ClassApp$etiquette[j] != zapp[j]) {
				comptapp<-comptapp+1;
			}
		}
		for(j in 1:length(ClassTst$etiquette)) {
			if(ClassTst$etiquette[j] != ztst[j]) {
				compttst<-compttst+1;
			}
		}
	}
	terrapp<-comptapp/(length(ClassApp$etiquette)*100);
	terrtst<-compttst/(length(ClassTst$etiquette)*100);
	res<-cbind(terrapp, terrtst);
	res
}



# Exploitation. Calculs des erreurs

aleaAnalyseDiscriminateQuadratique1.1000<-aleaAnalyseDiscriminateQuadratique(donn1.1000)
aleaAnalyseDiscriminateQuadratique1.1000
#        terrapp   terrtst
# [1,] 0.3763118 0.3804805
aleaAnalyseDiscriminateQuadratique2.1000<-aleaAnalyseDiscriminateQuadratique(donn2.1000)
aleaAnalyseDiscriminateQuadratique2.1000
#        terrapp   terrtst
# [1,] 0.1855856 0.1862275
aleaAnalyseDiscriminateQuadratique3.1000<-aleaAnalyseDiscriminateQuadratique(donn3.1000)
aleaAnalyseDiscriminateQuadratique3.1000
#         terrapp    terrtst
# [1,] 0.03470765 0.03378378


aleaAnalyseDiscriminateLineaire1.1000<-aleaAnalyseDiscriminateLineaire(donn1.1000 )
aleaAnalyseDiscriminateLineaire1.1000
#         terrapp    terrtst
# [1,] 0.08005997 0.07642643
aleaAnalyseDiscriminateLineaire2.1000<-aleaAnalyseDiscriminateLineaire(donn2.1000)
aleaAnalyseDiscriminateLineaire2.1000
#         terrapp   terrtst
# [1,] 0.01876877 0.0247006
aleaAnalyseDiscriminateLineaire3.1000<-aleaAnalyseDiscriminateLineaire(donn3.1000)
aleaAnalyseDiscriminateLineaire3.1000
#      terrapp  terrtst
# [1,] 0.10997 0.112012


aleaAnalyseDiscriminateNaif1.1000<-aleaAnalyseDiscriminateNaif(donn1.1000)
aleaAnalyseDiscriminateNaif1.1000
#        terrapp   terrtst
# [1,] 0.3084708 0.3153153
aleaAnalyseDiscriminateNaif2.1000<-aleaAnalyseDiscriminateNaif(donn2.1000)
aleaAnalyseDiscriminateNaif2.1000
#        terrapp   terrtst
# [1,] 0.3011261 0.3017964
aleaAnalyseDiscriminateNaif3.1000<-aleaAnalyseDiscriminateNaif(donn3.1000)
aleaAnalyseDiscriminateNaif3.1000
#       terrapp    terrtst
# [1,] 0.032009 0.03318318


aleaRegressionLogistique1.1000<-aleaRegressionLogistique(donn1.1000)
aleaRegressionLogistique1.1000
#         terrapp    terrtst
# [1,] 0.02466267 0.02612613
aleaRegressionLogistique2.1000<-aleaRegressionLogistique(donn2.1000)
aleaRegressionLogistique2.1000
#         terrapp    terrtst
# [1,] 0.01058559 0.01227545
aleaRegressionLogistique3.1000<-aleaRegressionLogistique(donn3.1000)
aleaRegressionLogistique3.1000
#         terrapp    terrtst
# [1,] 0.01656672 0.01591592


aleaRegressionLogQuadratique1.1000<-aleaRegressionLogQuadratique(donn1.1000)
aleaRegressionLogQuadratique1.1000
#         terrapp    terrtst
# [1,] 0.02233883 0.03138138
aleaRegressionLogQuadratique2.1000<-aleaRegressionLogQuadratique(donn2.1000)
aleaRegressionLogQuadratique2.1000
#         terrapp    terrtst
# [1,] 0.01051051 0.01047904
aleaRegressionLogQuadratique3.1000<-aleaRegressionLogQuadratique(donn3.1000)
aleaRegressionLogQuadratique3.1000
#         terrapp    terrtst
# [1,] 0.01124438 0.01306306

# Données Pima
aleaAnalyseDiscriminateQuadratiquePima<-aleaAnalyseDiscriminateQuadratique(Donn)
aleaAnalyseDiscriminateQuadratique(Donn)
#  terrapp  terrtst
# [1,] 0.212507 0.240678

aleaAnalyseDiscriminateLineairePima<-aleaAnalyseDiscriminateLineaire(Donn)
aleaAnalyseDiscriminateLineaire(Donn)
#       terrapp   terrtst
# [1,] 0.209662 0.2198305

aleaAnalyseDiscriminateNaifPima<-aleaAnalyseDiscriminateNaif(Donn)
 aleaAnalyseDiscriminateNaif(Donn)
#       terrapp   terrtst
# [1,] 0.2297183 0.2359322

 aleaRegressionLogistiquePima<-aleaRegressionLogistique(Donn)
 aleaRegressionLogistique(Donn)
#       terrapp   terrtst
# [1,] 0.2060845 0.2217514

aleaRegressionLogQuadratiquePimarsult<-aleaRegressionLogQuadratiquePima(Donn)
aleaRegressionLogQuadratiquePima(Donn)
#       terrapp   terrtst
# [1,] 0.1954085 0.2339548

# Données "breast cancer Wisconsin"



setwd("SY09/TD4/fonctions-tp4")
Donn <- read.csv("spam.csv", header=T)
XSpam <- Donn[,1:57]
zSpam <- Donn[,58]

aleaForet<-function(donnees) {
	comptapp<-0;
	compttst<-0;
	X <- donnees[,1:3]
	z <- donnees[,4]
	#X <- donnees[,2:58]
	#z <- donnees[,59]
	for(i in 1:20) {
		donnees.sep <- separ1(X, z)
		Xapp <- donnees.sep$Xapp;
		zapp <- donnees.sep$zapp;
		Xtst <- donnees.sep$Xtst;
		ztst <- donnees.sep$ztst;
		Dapp<-cbind(Xapp, class = as.factor(zapp))
 		rf <- randomForest(Dapp$class ~ ., Xapp, ntree = 500, mtry = 2, importance = TRUE)
	 	ClassAppEt<-predict(rf, newdata = Xapp, type="class"); #Etiquettes classes app
	 	ClassTstEt<-predict(rf, newdata = Xtst, type = "class")

	    	for(j in 1:length(ClassAppEt)) {
	    	#Si classe 1 > class 2 ... Sinon
	    		if(ClassAppEt[j] != zapp[j]) {
	    			comptapp<-comptapp+1;
	    		}
	    	}
	    	for(j in 1:length(ClassTstEt)) {
	    	#Si classe 1 > class 2 ... Sinon
	    		if(ClassTstEt[j] != ztst[j]) {
	    			compttst<-compttst+1;
	    		}
	    	}
	   }
	    terrapp<-comptapp/(length(ClassAppEt)*20);
	    terrtst<-compttst/(length(ClassTstEt)*20);
	    res<-cbind(terrapp, terrtst);
	    res
}
aleaForet(cbind(ACP$scores[,1:3], date.frame(Donn$z)))
aleaForet(cbind(ACP$scores,Donn$z))
aleaForet(Donn)

 rf <- randomForest(Dapp$class~., Xapp1.1000)
 rf <- randomForest(Dapp$class ~ ., Xapp1.1000, ntree = 500, mtry = 2, importance = TRUE)
 predict(rf, newdata = Xtst1.1000, type = "prob")

 aleaForet(Donn)
#         terrapp    terrtst
# [1,] 0.02710235 0.05720809


# ACP :
ACP<-princomp(Donn [2:58])

#  terrapp    terrtst
# [1,] 0.0004563233 0.06030659

# 3 Composantes :
#  terrapp   terrtst
# [1,] 0.000309648 0.1965427

# Arbres :
# terrapp   terrtst
# [1,] 0.03578879 0.0816047

# terrapp    terrtst
# [1,] 0.06835072 0.07296151

# 811.279
