splitQuoted = function(Cls,Data,Percentage,Key,LowLim){
  #   V = splitquoted(Cls,Percentage=80)
  #   TrainInd=V$TrainInd
  #   TestInd=V$TrainInd
  #   V = splitquoted(Cls,Data,Percentage)
  #   TrainInd=V$TrainInd
  #   TestInd=V$TrainInd
  #   TrainData=V$TrainData
  #   TestData=V$TestData
  #
  #   quota-split of Data randomly into disjunct sets TrainCls and TestCls,
  #   with |TrainCls| = |TestCls|*Percentage/100
  #   Splitting is done such that the percentages are the same over all classes
  #   In Data can be also divided into disjunct set in this function
  #   otherwise use indices to split Data in a later step
  #  
  #   INPUT
  #   Cls               [1:n] numeric vector of classifications
  #   Data              Optional, [1:n,1:d] matrix with n cases and d features
  #   Percentage        Scalar, value between 1 and 99, default is 80
  #   Key               Optional, the key, default either 1:length Cls or rownames(Data) if exists
  #   LowLim            Optional, if not missing the minimum number of cases per class, if minimum higher than number of elements in class, doing sample with replacement
  #
  #   OUTPUT
  #   TrainInd, TestInd     Indices of the split of the Cls
  #   TrainCls, TestCls     The split of the Cls
  #   TrainKey, TesKey      the split for the key
  #   TrainData, TestData   The split of the data
  #
  #
  #Details: sample without replacement is performend in the default case.  TrainInd and TestInd are permutated.
  #author: MCT, 2014, rewritten 2023
  
  V=FCPS::ClusterCount(Cls)
  UniqueClasses=V$UniqueClusters
  AnzClasses = V$NumberOfClusters
  CountPerCluster=V$CountPerCluster
  
  if(missing(Percentage)){
    Percentage=80
  }
  if(Percentage<1){
    Percentage*100
    warning(
    "splitQuoted: Percentage smaller than one, assuming that values are given between zero and one instead of 1 and 100%."
    )
  }
  #how many cases per class should be drawn
  NperClass=round(CountPerCluster*Percentage/100,0)

  Replace=rep(FALSE,AnzClasses)
  if(!missing(LowLim)){
    #set sample with replacement for classes with lower number of cases
    #than LowLim
    Replace=ifelse(NperClass>CountPerCluster,TRUE,FALSE)
    # as we raise each NperClass to lowLim if necessary
    #we have to reduce for the number of cases in NperClass for classes
    # above lowlim to reach pre-defined Percentage
    ntoomany=(sum(pmax(NperClass,LowLim))-sum(NperClass))
    
    if(ntoomany>0){
      n=rep(0,length(NperClass))
      #ntoomany=0
      for(i in 1:length(NperClass)){
        if(NperClass[i]<LowLim){
          # these classes haave cases that were additional added above Percentage
          # by lowlim
          # ntoomany=ntoomany+(LowLim-NperClass[i])
        }else{
          # these classes have more cases than low lim depending on split quoted
          n[i]=NperClass[i]-LowLim
        }
      }
      #now reduce classes until criteria is met
      while(ntoomany>0){#do as long as we have to many cases to match Percentage
        for(i in 1:length(n)){
          if(ntoomany>0){
            if(n[i]-1>=0){
              NperClass[i]=NperClass[i]-1
              ntoomany=ntoomany-1
              n[i]=n[i]-1
            }
          }
        }
        if(sum(n)<1){#break if all classes reached lowLim
          break;
        }
      }
    }
    NperClass=pmax(NperClass,LowLim)
    NewPercentage=round(sum(NperClass)/sum(CountPerCluster)*100,2)
    message(paste("splitQuoted: will take", paste0(NperClass,collapse = " "),"per class which equals a percentage of",NewPercentage))
  }
  
  TrainInd <- c()
  TestInd <- c()
  k=1
  for(i in UniqueClasses){
      ClassInd <- which(Cls==i)
      sampleInd <- sample(ClassInd,NperClass[k],replace = Replace[k])
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
        "splitQuoted: Number of cases in Data does not equal length of Cls. Ignoring Data Inputargument."
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
      "splitQuoted: length of key does not equal length of Cls. Ignoring Key Inputargument."
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

