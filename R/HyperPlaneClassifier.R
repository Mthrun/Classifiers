HyperPlaneClassifier <- function(Data,Hyperplane,PlotIt = 1){
# HyperPlaneClassifier  <- function(Data, Hyperplane, PlotIt)
# Klassifiziert Data in +1  -1 gemaess Hyperplane == [W,C]
# die Seite, in die der Normalenvektor W zeigt, definiert +1
# Hyperplane H(x) mit Gleichung:  W*x - C ==0
# Punkte auf der Hyperebene werden als positiv klassifiziert
#
# INPUT
# Data[1:d,1:n]           the n-dimensional data, cases in rows
# Hyperplane[1:n+1]       Hyperebene H(x) mit Gleichung:    W*x - C ==0 (hess'sche Normalform)  in  als c(W,C)
# 						  Note: W ist normierter Normalenvektor! (D.h. Laenge = 1)
#
# OPTIONAL
# 
# PlotIt                       ==1 (default) plots are made
# 
# OUTPUT
# Cls[1:n] 				  Cls[i] = +-1 je nach Seite auf der der Datensatz liegt
#						  W definiert +1 Richtung

############# TESTdatensatz
  #Dx <- runif(100, 0, 1) ;
  #Dy <- runif(100, 0, 1) ;
  #Data <- cbind(Dx,Dy); Data
  #Hyperplane = c(0,1,0.5) ;
 ############# TESTdatensatz 
  
  
  Size =dim(Data);
  d =  Size[1]; 
  n =  Size[2];
  W =  Hyperplane[1:n];
  C =  Hyperplane[n+1];
  
Cls <- c(1:d)
Abst <- c(1:d)
  
for(i in c(1:d)){
	x <- Data[i,]
	skal <- sum(W*x)
	res <- skal-C
	Abst[i] <- res
	if(res >= 0){ 
		Cls[i] <- 1
	}
	else{ 
		Cls[i] <- -1
	}
}

if( PlotIt == 1){
	pos <- grep('^1$', Cls)
	neg <- grep('^-1$', Cls)
	color <- c(1:d)
	color[pos] <- 'red'
	color[neg] <- 'blue'
	plot(Data[,1],Data[,2], main='HyperPlaneClassifier', xlab='first dimension', ylab='second dimension', pch=16, col=color)
}

 return (Cls)  # Rueckgabe der Klassifikation

 }  # end   HyperPlaneClassifier

