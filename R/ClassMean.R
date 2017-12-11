ClassMean <- function(Data, Cls) {
  # calulate mean in each group of the Data
  # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  # OUTPUT
  # UniqueClasses(AnzClass)       the  AnzClass unique classes in Cls
  # MeanPerClass(AnzClass,n)      MeanPerClass(i) is the mean of the Data points in UniqueClasses(i)
   
  
  # uniqueClasses <- sort(na.last = T, unique(Cls))
  # numberOfClasses <- length(uniqueClasses)
  # meanPerClass <- matrix(0, numberOfClasses, ncol(Data))
  # 
  # for (i in 1:numberOfClasses) {
  #   inClassInd <- which(Cls == uniqueClasses[i])
  #   x = as.matrix(Data[inClassInd, ])
  #   if(length(inClassInd) > 1){
  #     margin = 2
  #   } else { # Bei nur einem Klasseneintrag sind in X von R zeilen und spalten vertauscht. 
  #     margin = 1
  #   }
  #   meanPerClass[i, ]  <-
  #     apply(
  #       X = x,
  #       FUN = mean,
  #       MARGIN = margin,
  #       na.rm = T
  #     )
  # }
  res = ClassApply(Data, Cls, mean, na.rm = T) 
  return(list(UniqueClasses = res$UniqueClasses, MeanPerClass = res$ResultPerClass))
}
