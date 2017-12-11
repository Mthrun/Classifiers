splitQuoted =function(Data,Cls,Percentage, Key=NULL){

# function [TrainData, TestData, TrainCls,TestCls] = splitquoted(Data,Cls,Percentage);
# % [TrainData, TestData] = splitquoted(Data,Cls,Percentage);
# % quota-split of Data randomly into disjunct sets TrainData and TestData,
# %             with |TrainData| = |Data|*Percentage/100
# % spitting is done such that the percentages are the same over all classes
# % use [TrainDataCls, TestDataCls] = splitquoted([Data,Cls],Cls,Percentage);
# % to get also the Cls vector split
# %
# % INPUT
# % Data(d,n)         d cases,  n variables
# % Cls(d)            vector of classiofications
# % Percentage        between 1 and 99
# % OUTPUT
# % TraingData, TestData    the split Data
#   TrainInd, TestInd     indizes of the Datapoints

  UniqueClasses = unique(Cls);
  AnzClasses = length(UniqueClasses);

  if(is.null(Key)) Key = 1:(nrow(Data))

  # TestData <- matrix(ncol=ncol(Data),nrow=0)
  # TrainData <- matrix(ncol=ncol(Data),nrow=0)
  # TrainCls <- vector()
  # TestCls <- vector()

  TrainInd <- c()
  TestInd <- c()

  for(i in UniqueClasses){
      ClassInd <- which(Cls==i)
      sampleInd <- sample(ClassInd, round(length(ClassInd)* Percentage/100))

      #ClassData <- Data[Cls==i,]
      #sampleInd <- sample(nrow(ClassData),round(nrow(ClassData)* Percentage/100))

      TrainInd = c(TrainInd, sampleInd)
      TestInd = c(TestInd, setdiff(ClassInd, sampleInd))

      # ClassTestData <- matrix(ClassData[-sampleInd,],ncol=ncol(Data))
      # ClassTrainData <- matrix(ClassData[sampleInd,],ncol=ncol(Data))
      # TrainData  <- rbind(TrainData,ClassTrainData)
      # TestData  <- rbind(TestData,ClassTestData)
      #
      # newTrain <-  nrow(ClassTrainData)
      # newTest  <-  nrow(ClassTestData)
      # if (newTrain>0) {TrainCls <- c(TrainCls,rep(i,newTrain))}
      # if (newTest>0)  {TestCls  <- c(TestCls,rep(i,newTest))}
  }

  # Indizes zufaellig durchschuetteln
  TrainInd = sample(TrainInd, length(TrainInd))
  TestInd = sample(TestInd, length(TestInd))

  TrainData = Data[TrainInd,]
  TestData = Data[TestInd,]
  TrainCls = Cls[TrainInd]
  TestCls = Cls[TestInd]
  TrainKey = Key[TrainInd]
  TestKey = Key[TestInd]

 return (list(TrainData=TrainData, TestData=TestData, TrainCls=TrainCls, TestCls=TestCls,
              TrainKey=TrainKey, TestKey = TestKey, TrainInd=TrainInd, TestInd=TestInd))
 }

