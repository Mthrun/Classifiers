splitSample <- function(Data, percentage) {

#INPUT
# Data(1:n,1:d)           vector or matrix, n cases,  d variables
# Percentage              between .1 and 99.9        

# OUTPUT
# TrainData, TestData     the split Data
# TrainInd, TestInd       such that TrainingData = Data[TrainInd,], TestData = Data[TestInd,];

if(is.vector(Data) == TRUE){
	cols <- 1
	rows <- length(Data)
}else{
	cols <- ncol(Data)
	rows <- nrow(Data)
}

percentage <- max(percentage, 0.01)
percentage  <- min(percentage, 99.9)
 
nrInTrainData <- round( (rows * percentage)/ 100)

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