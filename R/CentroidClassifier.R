CentroidClassifier <-
function (data, cls, testData = data, centroidMethod = "mean") 
{
requireNamespace('Distances')
    if (centroidMethod == "median") {
        median <- ClassMedian(data, cls)
        uniqueClasses <- median[[1]]
        centroid <- median[[2]]
    }
    else {
        mean <- ClassMean(data, cls)
        uniqueClasses <- mean[[1]]
        centroid <- mean[[2]]
    }
    nrOfData <- nrow(testData)
    nearestInd <- rep(0, nrOfData)
    assignedCls <- nearestInd
    for (i in 1:nrOfData) {
        nearestInd[i] <- Distances::bestBuddy(testData[i, ], centroid)[[1]]
        assignedCls[i] <- uniqueClasses[nearestInd[i]]
    }
    return(list(assignedCls = assignedCls, uniqueClasses = uniqueClasses, 
        centroid = centroid))
}
