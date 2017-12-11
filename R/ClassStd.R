ClassStd <- function(Data, Cls) {
  # calulate standard deviation in each group of the data
  # INPUT
  # data(d,n)         d cases,  n variables
  # cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)      the  AnzClass unique classes in cls
  # StdPerClass(AnzClass,n)      StdPerClass(i) is the standard deviation of the data points in UniqueClasses(i)
  
  # uniqueClasses <- sort(na.last = T, unique(cls))
  # numberOfClasses <- length(uniqueClasses)
  # stdPerClass <- matrix(0, numberOfClasses, ncol(data))
  # 
  # for (i in 1:numberOfClasses) {
  #   inClassInd <- which(Cls == uniqueClasses[i])
  #     apply(
  #       X = Data[inClassInd, ],
  #       FUN = sd,
  #       MARGIN = 2,
  #       na.rm = T
  #     )
  # }
  res = ClassApply(Data,Cls,sd,na.rm=T)

  return(list(UniqueClasses = res$UniqueClasses, StdPerClass = res$ResultPerClass))
}
