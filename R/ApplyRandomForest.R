ApplyRandomForest = function(Forest, Data){
  # Cls = ApplyRandomForest(Forest, Data)
  # INPUT
  # Forest      Pre Trained Forest. V$Forest output from RandomForestClassifier Function.
  # Data        Data to be predicted
  # OUTPUT
  # Cls
  
  return(as.numeric(predict(Forest, Data)))
}


