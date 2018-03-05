BuildNaiveBayesClassifier=function(Data,Cls, PlotIt=TRUE) {
# V = BuildNaiveBayesClassifier(Data,Cls)   
# BayesCls=V$BayesCls
# UniqueClasses=V$UniqueClasses
# MeanPerClass=V$MeanPerClass
# StdPerClass=V$StdPerClass
# WeightsPerClass=V$WeightsPerClass
# 
# Bau eines Naiven Bayes Klassifiers:
# Data(1:d,1:n)            the  data n-dimensional data, cases in rows
# Cls(1:n)                 Cls(i) = Class of Data(i,:)
#
# OPTIONAL
# PlotIt                    ==1 (default) plots are made, Currenty unavailible 
# 
# OUTPUT
# BayesCls(1:d)                    die Klassifizierung des naive bayes Klassifikators
#
# DAS BAYES MODELL
# UniqueClasses(1:NrOfClasses)     Klassenbeziechnungen
# MeanPerClass(1:NrOfClasses,1:n)  klassenbezogene Mittelwerte fuer jede Variable
# StdPerClass(1:NrOfClasses,1:n)  klassenbezogene Std fuer jede Variable
# WeightsPerClass(1:NrOfClasses)   relative Klassengroesse
#author: MT 01/17   
requireNamespace('AdaptGauss')
  vec = dim(Data)
  AnzDaten = vec[1]
  AnzVariablen = vec[2]
  
#ind1=which(TrainAndTest$TrainCls==1)
#ind2=which(TrainAndTest$TrainCls==2)

#robust mean
#Means1=apply(X = TrainAndTest$TrainData[ind1,],FUN = mean,MARGIN = 2,0.1)
#Means2=apply(X = TrainAndTest$TrainData[ind2,],FUN = mean,MARGIN = 2,0.1)
#robust std
#stdrobust=function (x, lowInnerPercentile = 25) 
#{
 # if (is.vector(x) || (is.matrix(x) && dim(x)[1] == 1)) 
 #   dim(x) <- c(length(x), 1)
 # lowInnerPercentile <- max(1, min(lowInnerPercentile, 49))
 # hiInnerPercentile <- 100 - lowInnerPercentile
  #faktor <- sum(abs(qnorm(t(c(lowInnerPercentile, hiInnerPercentile)/100), 
 #                         0, 1)))
 # std <- sd(x, na.rm = TRUE)
 # p <- c(lowInnerPercentile, hiInnerPercentile)/100
 # quartile <- prctile(x, p)
 # if (ncol(x) > 1) 
 #   iqr <- quartile[2, ] - quartile[1, ]
#  else iqr <- quartile[2] - quartile[1]
#  shat <- c()
#  for (i in 1:ncol(x)) {
 #   shat[i] <- min(std[i], iqr[i]/faktor, na.rm = TRUE)
#  }
#  dim(shat) <- c(1, ncol(x))
#  colnames(shat) <- colnames(x)
#  return(shat)
#}
#std1=apply(X = TrainAndTest$TrainData[ind1,],FUN = stdrobust,MARGIN = 2,0.2)
#std2=apply(X = TrainAndTest$TrainData[ind2,],FUN = stdrobust,MARGIN = 2,0.2)

#weights=c(length(ind1),length(ind2))/nrow(TrainAndTest$TrainData)    
#bayesclas=Classifiers::ApplyNaiveBayesClassifier(Data = TrainAndTest$TrainData,c(1,2),t(cbind(Means1,Means2)),t(cbind(std1,std2)),WeightsPerClass = weights)
	
  # bestimmen des Modells mit Mean, Sdev und Weight der einzelen Klassen in den jeweiligen variablen
  V = ClassCount(Cls)
  UniqueClasses = V$UniqueClasses
  CountPerClass = V$CountPerClass
  NrOfClasses = V$NumberOfClasses
  WeightsPerClass = CountPerClass / AnzDaten
  
  V = ClassMean(Data, Cls)
  UniqueClasses = V$UniqueClasses
  MeanPerClass = V$MeanPerClass
  
  V = ClassStd(Data, Cls)
  UniqueClasses = V$UniqueClasses
  StdPerClass = V$StdPerClass
  
  BayesPost   = array(0, c(AnzDaten, AnzVariablen, NrOfClasses))  # alle Posteriors fuer alle Variablen und Classen
  Boundaries  = matrix(0, (NrOfClasses - 1), AnzVariablen) # alle Grenzen fuer alle Variablen und Classen
  
  for (i in 1:AnzVariablen) {
    # fuer jede Variable i
    D = Data[, i]
    M = MeanPerClass[, i]
    S = StdPerClass[, i]
    W = WeightsPerClass
    Posteriors = AdaptGauss::Bayes4Mixtures(D, M, S, W)$Posteriors  # Posteriors(1:AnzDaten,1:NrOfClasses] die nach Klassen geordneten Posterioris fuer die i-te variable
    DecisionBoundaries = AdaptGauss::BayesDecisionBoundaries(M, S, W)  # DecisionBoundaries(1:(NrOfClasses-1)) fuer die i-te variable
    # festhalten
    for (c in 1:NrOfClasses) {
      BayesPost[, i, c] = Posteriors[, c]
    } # for c=1:NrOfClasses
    Boundaries[, i] = DecisionBoundaries
    
    if (PlotIt) {
      # zeichnen
      # tileplot(3,3,i)
      # ClassPDEplot(D,Cls, 'br')
      # hold on  AdaptGauss::PlotGaussMixesAndBoundaries(D,M,S,W)  hold off
      # hold on  plot(D,Posteriors,'.' ) hold off
    }  #  if PlotIt==1  # zeichnen
  }  # for i
  
  NaiveBayesPosteriori = matrix(0, AnzDaten, NrOfClasses)
  # Jetzt Naive Bayes posteriori ausrechnen als Produkt der Posteriors
  for (c in 1:NrOfClasses) {
    NaiveBayesPosteriori[, c] = apply(BayesPost[, , c], 1, prod) #matlab: prod(BayesPost(:,:,c],2)
  } # for c=1:NrOfClasses
  
  # jetzt bayes Klassifizieren
  BayesCls = Cls * NaN   # initialisieren
  MaxInd = apply(NaiveBayesPosteriori,1,which.max)#which.max(NaiveBayesPosteriori)
  #matlab: [MaxBayes MaxInd] = nanmax(NaiveBayesPosteriori,[],2)  # das maximum bestimmen, dies ist der Index der zuzuordnenden Klasse
  BayesCls = UniqueClasses[MaxInd]                      # die entsprechende Klasse zuordnen
  
  if (PlotIt) {
    # zeichnen
	requireNamespace('DataVisualisation')
    DataVisualisation::PlotPixMatrix(cbind(NaiveBayesPosteriori, Cls, BayesCls))
  }  #  if PlotIt==1  # zeichnen
  
  return(list(
    BayesCls=BayesCls,
    UniqueClasses=UniqueClasses,
    MeanPerClass=MeanPerClass,
    StdPerClass=StdPerClass,
    WeightsPerClass=WeightsPerClass
  ))
}
