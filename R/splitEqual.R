splitEqual = function(Cls,Data,Percentage, ForceEqual=FALSE,Key){
  #   V = splitEqual(Cls,Percentage=50)
  #   TrainInd=V$TrainInd
  #   TestInd=V$TrainInd
  #   V = splitEqual(Cls,Data,Percentage)
  #   TrainInd=V$TrainInd
  #   TestInd=V$TrainInd
  #   TrainData=V$TrainData
  #   TestData=V$TestData
  #   equal split of Data randomly into disjunct sets TrainCls and TestCls,
  #   with |TrainCls| = |TestCls|*Percentage/100.
  #   Splitting is done such that given a percentage the cases over all classes are the same.
  #   Data can be also divided into disjunct set in this function
  #   otherwise use indices to split Data in a later step
  #  
  #   INPUT
  #   Cls               [1:n] numeric vector of classifications
  #   Data              Optional, [1:n,1:d] matrix with n cases and d features
  #   Percentage        Scalar, value between 1 and 99, default is 50
  #   ForceEqual        TRUE: sample with replacement if percentage of full cases is above the number of cases in a class, 
  #                     default: FALSE: less cases are sampled resulting in an still unequal split If a class has a low number of cases
  #   Key               Optional, the key, default either 1:length Cls or rownames(Data) if existing
  #   OUTPUT
  #   TrainInd, TestInd     Indices of the split of the Cls
  #   TrainCls, TestCls     The split of the Cls
  #   TrainKey, TesKey      the split for the key
  #   TrainData, TestData   The split of the data
  #Details: sample without replacement is performend in the default case.  TrainInd and TestInd are permutated.
  #author: MCT, 2023
  
  V=FCPS::ClusterCount(Cls)
  UniqueClasses=V$UniqueClusters
  AnzClasses = V$NumberOfClusters
  CountPerCluster=V$CountPerCluster
  if(missing(Percentage)){
    Percentage=50
  }

  if(Percentage<1){
    Percentage*100
    warning(
      "splitEqual: Percentage smaller than one, assuming that values are given between zero and one instead of 1 and 100%."
    )
  }
  TrainInd <- c()
  TestInd <- c()
  
  Nges=round(length(Cls)*Percentage/100,0)
  NperClassToTake=round(Nges/AnzClasses,0)

  if(isFALSE(ForceEqual)){
    NperClass=pmin(CountPerCluster,NperClassToTake)
  }else{
    NperClass=rep(NperClassToTake,AnzClasses)
  }

  #bestimmte anzahl cases pro class NoCases
  k=1
  for(i in UniqueClasses){
    ClassInd <- which(Cls==i)
    sampleInd <- sample(ClassInd, NperClass[k],replace = ForceEqual)

    TrainInd = c(TrainInd, sampleInd)
    TestInd = c(TestInd, setdiff(ClassInd, sampleInd))
    k=k+1
  }
  
  # Indizes zufaellig durchschuetteln
  TrainInd = sample(TrainInd, length(TrainInd))
  TestInd = sample(TestInd, length(TestInd))
  
  if (!missing(Data)) {
    Data = as.matrix(Data)
    if (length(Cls) == dim(Data)[1]) {
      TrainData = Data[TrainInd, , drop = FALSE]
      TestData = Data[TestInd, , drop = FALSE]
      
      if (missing(Key)) {
        if (!is.null(rownames(Data))) {
          Key = rownames(Data)
        } else{
          Key = 1:(length(Cls))
        }
      }
    } else{
      warning(
        "splitEqual: Number of cases in Data does not equal length of Cls. Ignoring Data Inputargument."
      )
      TrainData = NULL
      TestData = NULL
      if (missing(Key))
        Key = 1:(length(Cls))
    }
  } else{
    TrainData = NULL
    TestData = NULL
    if (missing(Key))
      Key = 1:(length(Cls))
  }
  
  
  TrainCls = Cls[TrainInd]
  TestCls = Cls[TestInd]
  if (length(Cls) == length(Key)) {
    TrainKey = Key[TrainInd]
    TestKey = Key[TestInd]
  }else{
    warning(
      "splitEqual: length of key does not equal length of Cls. Ignoring Key Inputargument."
    )
    Key = 1:(length(Cls))
    TrainKey = Key[TrainInd]
    TestKey = Key[TestInd]
  }
  
  return (
    list(
      TrainInd = TrainInd,
      TestInd = TestInd,
      TrainCls = TrainCls,
      TestCls = TestCls,
      TrainData = TrainData,
      TestData = TestData,
      TrainKey = TrainKey,
      TestKey = TestKey
    )
  )
}

