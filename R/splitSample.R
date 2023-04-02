splitSample <- function(Data, Percentage) {
  #   V = splitSample(Cls,Percentage=80)
  #   TrainData=V$TrainData
  #   TestData=V$TestData
  #   TrainInd=V$TrainInd
  #   TestInd=V$TrainInd
  #
  # Splits a data vector or matrix into training and test data. The size of the partitions is defined by the given percentage.
  #
  #INPUT
  # Data                    [1:n,1:d] Matrix of n cases and d features, or n d-dimensional datapoints
  # Percentage              Scalar, value between 1 and 99, default is 80      
  #
  # OUTPUT
  # TrainData, TestData     The split of Data
  # TrainInd, TestInd       such that TrainingData = Data[TrainInd,], TestData = Data[TestInd,];
  #
  # author: MCT
  
  if(is.vector(Data) == TRUE){
  	cols <- 1
  	rows <- length(Data)
  }else{
  	cols <- ncol(Data)
  	rows <- nrow(Data)
  }
  
    if(Percentage<=1){
      Percentage*100
      warning(
        "splitSample: Percentage 1 or smaller, assuming that values are given between zero and one instead of 1 and 100%."
      )
    }
   
  nrInTrainData <- round( (rows * Percentage)/ 100)
  
  if (nrInTrainData == rows) {
  nrInTrainData <- nrInTrainData -1
  }
  nrInTestData <- rows - nrInTrainData
  
  perms <- sample(rows)
  trainInd <- perms[1: nrInTrainData]
  testInd <- perms[ (nrInTrainData +1) : rows]
  if(is.vector(Data) == TRUE){
  	trainData <- Data[trainInd]
  	testData <- Data[testInd]
  }else{
  	trainData <- Data[trainInd, ]
  	testData <- Data[testInd, ]
  }
  return(list(trainData = trainData, testData = testData, trainInd = trainInd, testInd = testInd))
}