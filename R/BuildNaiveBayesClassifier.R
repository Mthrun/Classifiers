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

  vec = dim(Data)
  AnzDaten = vec[1]
  AnzVariablen = vec[2]
  
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
    Posteriors = Bayes4Mixtures(D, M, S, W)$Posteriors  # Posteriors(1:AnzDaten,1:NrOfClasses] die nach Klassen geordneten Posterioris fuer die i-te variable
    DecisionBoundaries = BayesDecisionBoundaries(M, S, W)  # DecisionBoundaries(1:(NrOfClasses-1)) fuer die i-te variable
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
    NaiveBayesPosteriori[, c] = apply(BayesPost[, , c], 1, prod) #matlab: prod(BayesPost(:,:,c],2)
  } # for c=1:NrOfClasses
  
  # jetzt bayes Klassifizieren
  BayesCls = Cls * NaN   # initialisieren
  MaxInd = apply(NaiveBayesPosteriori,1,which.max)#which.max(NaiveBayesPosteriori)
  #matlab: [MaxBayes MaxInd] = nanmax(NaiveBayesPosteriori,[],2)  # das maximum bestimmen, dies ist der Index der zuzuordnenden Klasse
  BayesCls = UniqueClasses[MaxInd]                      # die entsprechende Klasse zuordnen
  
  if (PlotIt) {
    # zeichnen
    print(DataVisualizations::Pixelmatrix(cbind(NaiveBayesPosteriori, Cls, BayesCls)))
  }  #  if PlotIt==1  # zeichnen
  
  return(list(
    BayesCls=BayesCls,
    UniqueClasses=UniqueClasses,
    MeanPerClass=MeanPerClass,
    StdPerClass=StdPerClass,
    WeightsPerClass=WeightsPerClass
  ))
}