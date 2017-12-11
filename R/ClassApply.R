ClassApply <- function(Data, Cls, Func, ...){
  # Applies a given function to each dimension for each class in Cls over the data.
   # INPUT
  # Data(d,n)         d cases,  n variables
  # Cls(d)            Cls(i) == ClusterNumber of Data(i,:)
  # Func              Function to be applied
  # OUTPUT
  # UniqueClasses[AnzClass]             the  AnzClass unique classes in Cls
  # ResultPerClass[AnzClass,Columns]    ResultPerClass[i] is the result of func for the data points in class UniqueClasses[i]
  
  uniqueClasses <- sort(na.last = T, unique(Cls))
  numberOfClasses <- length(uniqueClasses)
  resultPerClass <- matrix(0, numberOfClasses, ncol(Data))
  
  for (i in 1:numberOfClasses) {
    inClassInd <- which(Cls == uniqueClasses[i])
    x = Data[inClassInd, ]
    if(is.vector(x)) { # Wenns nur ein Datensatz, also ein Vektor, ist, macht R mist. Also konvertieren.
      margin = 1
      x = as.matrix(x)
    } else {
      margin = 2
    }
    resultPerClass[i, ]  <-
      apply(
        X = x,
        FUN = Func,
        MARGIN = margin,
        ...
      )
  }
  
  return(list(UniqueClasses = uniqueClasses, ResultPerClass = resultPerClass))
}