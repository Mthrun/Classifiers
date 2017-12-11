ClassMedian = function(Data, Cls) {
  # calulate Median in each group of the data
  # INPUT
  # data(d,n)         d cases,  n variables
  # cls(d)            cls(i) == ClusterNumber of data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)      the  AnzClass unique classes in cls
  # MedianPerClass(AnzClass,n)   MedianPerClass(i) is the Median of the data points in UniqueClasses(i)
  
  # UniqueClasses = unique(Cls)
  # 
  # All2UniqInd = 1:length(UniqueClasses)
  # medianPerClass = c()
  # 
  # for (i in 1:length(UniqueClasses)) {
  #   inClassInd = which(Cls == UniqueClasses[i])
  #   medianPerClass = rbind(medianPerClass, nanmedian(Data[inClassInd,]))
  # }
  res = ClassApply(Data,Cls,mean,na.rm = T)
  
  return(list(UniqueClasses = res$UniqueClasses, MedianPerClass = res$ResultPerClass))
}