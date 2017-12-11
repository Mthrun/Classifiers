ClassCount <- function(Cls) {
# calulates statistics for   points in each group of the data
  # C <-ClassCount(Cls);
  # UniqueClasses <-C$uniqueClasses
  # CountPerClass <-C$countPerClass
  # NrOfClasses   <-C$numberOfClasses
  # ClassPercentages <-C$classPercentages
  #
  # INPUT
  # Cls(d)                          Cls(i) == ClusterNumber of data(i,:)
  #
  # OUTPUT list with:
  # UniqueClasses[1:NrOfClasses]      the  NrOfClasses unique classes in Cls
  # CountPerClass(NrOfClasses,n)    CountPerClass(i) is the Count of the data points in UniqueClasses(i)
  # NumberOfClasses                     the number of classes
  # ClassPercentages                the percentages of the classes
  # All2UniqInd                     UniqueClasses == Cls[All2UniqInd]
  # Uniq2AllInd                     Cls == UniqueClasses[Uniq2AllInd]
  
  uniqueClasses <- sort(na.last = T, unique(Cls))
  numberOfClasses <- length(uniqueClasses)
  countPerClass <-
    rep(0, numberOfClasses) # just initializing the vector, all values are replaced later.
  
  for (i in 1:numberOfClasses) {
    inClassI <-
      sum(Cls == uniqueClasses[i]) # counts all occurances of uniqueClass[i] in Cls
    countPerClass[i] = inClassI # updates countPerClass[i] to the number of occurances of uniqueClasses[i] in Cls.
  }
  
  classPercentages <- rep(0, numberOfClasses)
  
  for (i in 1:numberOfClasses) {
    classPercentages[i] <- (countPerClass[i] / sum(countPerClass)) * 100
  }
  
  return(
    list(
      UniqueClasses = uniqueClasses,
      CountPerClass = countPerClass,
      NumberOfClasses = numberOfClasses,
      ClassPercentages = classPercentages,
      All2UniqInd = vapply(uniqueClasses,function(i){max(which(Cls==i))},0),
      Uniq2AllInd = vapply(uniqueClasses, function(i){which(uniqueClasses == i)},0)
    )
  )
}
