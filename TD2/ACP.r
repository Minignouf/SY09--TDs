1)
M<-matrix(c(3,4,3,1,4,3,2,3,6,4,1,2),4,byrow=TRUE)
Mcentre <- centre(M)
     [,1] [,2] [,3]
[1,]  0.5    1 -0.5
[2,] -1.5    1 -0.5
[3,] -0.5    0  2.5
[4,]  1.5   -2 -1.5
V<-(1/4) * prodtrans(Mcentre)
      [,1] [,2]  [,3]
[1,]  1.25 -1.0 -0.75
[2,] -1.00  1.5  0.50
[3,] -0.75  0.5  2.25
valeurVecteurPropre <- eigen(V)
$values
[1] 3.1988922 1.4684861 0.3326217


$vectors
           [,1]       [,2]      [,3]
[1,]  0.5240424  0.3386197 0.7814834
[2,] -0.5093555 -0.6107855 0.6062161
[3,] -0.6825955  0.7157358 0.1475997

sommeValeursPropres <- sum(valeurVecteurPropre$values)
5

inertie<-cbind(inertie, valeurVecteurPropre$values[1]*100/sommeValeursPropres)
inertie<-cbind(inertie, valeurVecteurPropre$values[2]*100/sommeValeursPropres)
inertie<-cbind(inertie, valeurVecteurPropre$values[3]*100/sommeValeursPropres)
    [,1]     [,2]     [,3]
[1,] 63.97784 29.36972 6.652434

2)
C<-Mcentre%*%valeurVecteurPropre$vectors
            [,1]       [,2]        [,3]
[1,]  0.09396341 -0.7993436  0.92315798
[2,] -0.95412132 -1.4765829 -0.63980885
[3,] -1.96850984  1.6200297 -0.02174241
[4,]  2.82866775  0.6558968 -0.26160672

 plot(C[,1], C[,2])
 
 3) Voir Représentation des variables p/43
 Col 1, L1:
    (1/(sqrt((3/4))*sd(Mcentre[,1])))*(1/sqrt(valeurVecteurPropre$values[1]))*t(Mcentre[,1])%*%diag(c(1/4,1/4,1/4,1/4))%*%C[,1]
          [,1]
[1,] 0.8383226
 Col 2, L2:
(1/(sqrt((3/4))*sd(Mcentre[,2])))*(1/sqrt(valeurVecteurPropre$values[2]))*t(Mcentre[,2])%*%diag(c(1/4,1/4,1/4,1/4))%*%C[,2]
           [,1]
[1,] -0.6043354
 Col 3, L3:
(1/(sqrt((3/4))*sd(Mcentre[,3])))*(1/sqrt(valeurVecteurPropre$values[3]))*t(Mcentre[,3])%*%diag(c(1/4,1/4,1/4,1/4))%*%C[,3]
           [,1]
[1,] 0.05675048

for(j in 1:3){
for(a in 1:3){
print ((1/(sqrt((3/4))*sd(Mcentre[,j])))*(1/sqrt(valeurVecteurPropre$values[a]))*t(Mcentre[,j])%*%diag(c(1/4,1/4,1/4,1/4))%*%C[,a])
}
}

8

res<-matrix(c(0.8383226,0.367022,0.4031253, -0.7438325, -0.6043354, 0.2854678, -0.8139017,0.5782244, 0.05675048),3,byrow=TRUE) 
 plot(res[,1],res[,2],xlim=c(-1,+1),ylim=c(-1,+1),asp=1)
symbols(0,0,circles=1,inches=F,add=T)
abline(h=0,v=0)
