AnalysisOfClassifier <- function(TrueClassification,CLS){
  # V = AnalysisOfClassifier(Cls,BayesCls) 
  # UniqueClasses=V$UniqueClasses
  # NrInClasses=V$NrInClasses
  # Sensitivity=V$Sensitivity
  # Specifity=V$Specifity
  # FalsePositiveRate=V$FalsePositiveRate
  # Accuracy=V$Accuracy
  # ClassAccuracy=V$ClassAccuracy
  # NegativePredVal=V$NegativePredVal
  # PossitivePredVal=V$PossitivePredVal

# measure a classifier's performance.

# INPUT
# TrueClassification(n)   vector of integer numbers representing the TRUE         classification
# CLS[n]                  vector of integer numbers representing the CLASSIFIER'S classification

# OUTPUT
# UniqueClasses           the different classes in TrueClassification
# NrInClasses             number of members in each class
# Sensitivity
# Specifity
# PositivePredVal         Positive Predictive Value
# NegativePredVal         Negative Predictive Value
# Accuracy           Percentage of corretly classified data per class TotalCorrectness * relativeClassSize



 n <- length(TrueClassification)
 nCLS <- length(CLS)
if (n!=nCLS){return('AnalysisOfClassifier(): classification vectors are not the same length');}

 UniqueClasses = sort(unique(TrueClassification))
 NrOfClasses = length(UniqueClasses)

 ## Initialisierung
 INIT = rep(0,NrOfClasses)
 NrInClasses= INIT
 Sensitivity= INIT
 Specifity= INIT
 PossitivePredVal= INIT
 Accuracy= INIT
 TotalAccuracy = INIT
 NrNotInClass= INIT
 TruePos= INIT
  TrueNeg= INIT
  FalseNeg= INIT
  FalsePos= INIT
PosPredVal= INIT
NegPredVal = INIT

for (c in 1:length(UniqueClasses)){
     #Class = UniqueClasses[c];
     NrInClasses[c] = sum(TrueClassification==UniqueClasses[c]); NrNotInClass[c] = n-NrInClasses[c];


     # IsTruePos = ShouldNotBeInClass = which(Cls$Cls[TestInd] != j)
     # ShouldBeInClass = which(Cls$Cls[TestInd] == j)
     # IsInClass = which(TestClassification  == j)
     # IsNotInClass = which(TestClassification != j)

     IsPos  <- (CLS ==  UniqueClasses[c]); IsNeg  <- (FALSE==IsPos); # Positive: Classifier insists on membership
     IsTrue =   ((TrueClassification == UniqueClasses[c]));      # True: Datenpunkt ist wirklich in der Klasse
     IsFalse =  (FALSE == IsTrue);


     IsTruePos = IsTrue & IsPos;       TruePos[c]  = sum(IsTruePos);  # TruePos   regel Positiv,   wirklich in Klasse
     IsTrueNeg   = IsFalse & IsNeg ;    TrueNeg[c]  = sum(IsTrueNeg);  # TrueNeg   regel Neagativ,  wirklich nicht in Klasse
     IsFalseNeg  = IsTrue & IsNeg ;  FalseNeg[c] = sum(IsFalseNeg); # FalseNeg  regel Negativ   wirklich in Klasse
     IsFalsePos  = IsFalse & IsPos ;  FalsePos[c] = sum(IsFalsePos); # FalsePos  regel Positiv,  nicht in Klasse

	 #PossitivePredVal
	 if ((TruePos[c]+FalsePos[c])==0){ PosPredVal[c]=0;}
	 else {PosPredVal[c] = TruePos[c]/(TruePos[c] + FalsePos[c]) *100;}

   if ((TrueNeg[c] +FalseNeg[c]) == 0){NegPredVal[c] = 0;}
   else {NegPredVal[c] = TrueNeg[c]/(TrueNeg[c] + FalseNeg[c]) *100;}
	 #TotalAccuracy (Anteil richtiger Class. an allen Class. Verrechnet mit dem Anteil der Klasse an allen Klassen)
     # dh: Summe sollte Klassifikation entsprechen
     
	 Accuracy[c]=(TruePos[c]+TrueNeg[c])/(TruePos[c]+FalseNeg[c]+FalsePos[c]+TrueNeg[c])*100* NrInClasses[c]/n;
	 TotalAccuracy[c] = (TruePos[c]+TrueNeg[c])/(TruePos[c]+FalseNeg[c]+FalsePos[c]+TrueNeg[c])*100
}



  #spec = TruePos / (TruePos + FalsePos)

 AnzInClass    = TruePos    + FalseNeg
 AnzNotInClass = FalsePos   + TrueNeg
# TotalAnz      = AnzInClass + AnzNotInClass;
# Positive      = TruePos    + FalsePos ;
 
 sens = TruePos/AnzInClass * 100;
 spec = TrueNeg/AnzNotInClass * 100;
 
 FalsePositiveRate = FalsePos / (FalsePos + TruePos)

 return (list(UniqueClasses=UniqueClasses,NrInClasses=NrInClasses,Sensitivity=sens,
              Specifity=spec,PositivePredVal=PosPredVal,NegativePredVal=NegPredVal,ClassAccuracy=Accuracy,
              TotalClassAccuracy = TotalAccuracy,
              Accuracy=sum(Accuracy), FalsePositiveRate = FalsePositiveRate ))
}   #   end  function   AnalysisOfClassifier



