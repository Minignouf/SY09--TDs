#Inertie = dispersion/valeur moyenne => taille du crabe par rapport à la valeur moyenne
#Prétraitement des données ou jouer sur les résultats de l'ACP

#Méthode du coude => Observer le décrochement

2.1.4)
#U Vecteurs propres
n<-5;
p<-5;
reconstitution<-c()
for(a in 1:5) {
	reconstitution<-cbind(reconstitution,sum(matrix(res$scores[,a], nrow=n)%*%t(matrix(res$loadings[,a],nrow=p))))
	#sum(matrix(res$scores[,1], nrow=5)%*%t(matrix(res$loadings[,1],nrow=5)))
}

reconstitution<-function(){
print(matrix(res$scores[,1], nrow=200)%*%t(matrix(res$loadings[,1],nrow=5)))}

#Vecteurs + valeurs propres
res<-princomp(crabsquant)
res$sdev #Racine des valeurs propres = écart-type empirique corrigé
res$loadings #Vecteurs propres
res$scores #Composantes
  
plot(res$score[,1], res$score[,2])
  
diag(x = 1/200, nrow=200, ncol=200)
X<-centre(crabsquant) #Matrice centrée
 
inertie<-cbind(inertie, res$sdev[1]*100/sum(res$sdev))
inertie<-cbind(inertie, res$sdev[2]*100/sum(res$sdev))
inertie<-cbind(inertie, res$sdev[3]*100/sum(res$sdev))
inertie<-cbind(inertie, res$sdev[4]*100/sum(res$sdev))
inertie<-cbind(inertie, res$sdev[5]*100/sum(res$sdev))

 
Cor<-matrix(0,5,5)
for(j in 1:5)
	{
	for(a in 1:5)
	{
		Cor[j,a]<- ((1/(sqrt(199/200)*sd(X[,j])))*(1/res$sdev[a])*sum(X[,j]*res$scores[,a])/200)
	}
}

plot(Cor[,1],Cor[,2],xlim=c(-1.5,+1.5),ylim=c(-1.5,+1.5), asp=1)
symbols(0,0,circles=1,inches=F,add=T)
abline(h=0,v=0)



fam<-cbind(crabs, res$scores)
family <- as.factor(fam[,1])
family <- as.factor(fam[,2])
plot(fam[,9], fam[,10], col=family)
plot(fam[,9], fam[,10], pch=as.integer(fam[,1]), col=as.integer(fam[,2])) 

Cor<-matrix(0,3,3)
for(j in 1:3)
	{
	for(a in 1:3)
	{
		Cor[j,a]<- ((1/(sqrt(3/4)*sd(X[,j])))*(1/res$sdev[a])*sum(X[,j]*res$scores[,a])/4)
	}
}
