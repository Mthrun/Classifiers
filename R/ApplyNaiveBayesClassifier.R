ApplyNaiveBayesClassifier=function(Data,UniqueClasses,MeanPerClass,StdPerClass,WeightsPerClass, PlotIt=TRUE){
# 
# Anwendung eines Naiven Bayes Klassifiers:
# Data(1:d,1:n)            the  data n-dimensional data, cases in rows
# DAS BAYES MODELL
# UniqueClasses(1:NrOfClasses)     Klassenbeziechnungen
# MeanPerClass(1:NrOfClasses,1:n)  klassenbezogene Mittelwerte fuer jede Variable
# StdPerClass(1:NrOfClasses,1:n)  klassenbezogene Std fuer jede Variable
# WeightsPerClass(1:NrOfClasses)   relative Klassengroesse
#
# OPTIONAL
# PlotIt                    ==1 (default) plots are made
# 
# OUTPUT
# BayesCls(1:d)                    die Klassifizierung des naive bayes Klassifikators
#author: MT 01/17  
requireNamespace('AdaptGauss')
  vec = dim(Data)
  AnzDaten = vec[1]
  AnzVariablen = vec[2]
  NrOfClasses=length(UniqueClasses)
  
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
      # hold on  PlotGaussMixesAndBoundaries(D,M,S,W)  hold off
      # hold on  plot(D,Posteriors,'.' ) hold off
    }  #  if PlotIt==1  # zeichnen
  }  # for i
  
  NaiveBayesPosteriori = matrix(0, AnzDaten, NrOfClasses)
  # Jetzt Naive Bayes posteriori ausrechnen als Produkt der Posteriors
  for (c in 1:NrOfClasses) {
    NaiveBayesPosteriori[, c] = apply(BayesPost[, , c], 1, prod)#prod(BayesPost(:,:,c),2)
  } # for c=1:NrOfClasses
  
  # jetzt bayes Klassifizieren
  #BayesCls = Cls * NaN   # initialisieren
  MaxInd = apply(NaiveBayesPosteriori,1,which.max)
  #matlab: [MaxBayes MaxInd] = nanmax(NaiveBayesPosteriori,[],2)  # das maximum bestimmen, dies ist der Index der zuzuordnenden Klasse
  BayesCls = UniqueClasses[MaxInd]                      # die entsprechende Klasse zuordnen
  
  if (PlotIt) {
    # zeichnen
	requireNamespace('DataVisualisation')
    DataVisualisation::PlotPixMatrix(cbind(NaiveBayesPosteriori, BayesCls))
  }  #  if PlotIt==1  # zeichnen
  
  return(BayesCls)
}