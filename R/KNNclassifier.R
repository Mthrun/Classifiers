KNNclassifier <- function(k,TrainData,TrainCls,TestData,Verbose=1){
# [KNNTestCls,NearestInd ] = KNNclassifier(k,TrainData,TrainCls,TestData,Verbose);
#  k-nearest neighbor clssifier
# INPUT
# k                    the number of neighbor to use
# TrainData            matrix [n,d] containing classified data
# TrainCls             vector [1:n] containing the classes of TrainData
# TestData             matrix [m,d] containing unclassified data
# OPTIONAL
#  Verbose             default ==1, progress report
# OUTPUT
# KNNTestCls           vector [1:m], a KNN classification of TestData
# NearestInd           matrix [1:m,1:k], such that for TestData(i,) theTrainData k nearest
#                      neigbors in increasing distances have NearestInd(i,)
#                      as Index into TrainData or TrainCls
#
#Author: 04/15 RG, imported from matlab

# naive Implementierung
m = nrow(TestData)
AnzVariablen = ncol(TestData)
n = nrow(TrainData)
# AnzVariablen = ncol(TrainData)
ncls = length(TrainCls)
if (n != ncls) {
  stop('KNNclassifier: TrainCls does not match TrainData')
}


KNNTestCls=matrix(0,m,1);
NearestInd = matrix(0,m,k);
#Tacho <- winProgressBar(label = paste("KNNclassifier: i= ",toString(m)), min = 0, max = m , width = 300)
ones=function (n, m = n) 
{
  if (m == 1) {
    return(c(1:n) * 0 + 1)
  }
  else {
    return(matrix(1, n, m))
  }
}


for(i in 1:m){
  if(0 == 1000%%i){
    Sys.sleep(0.1)
#    setWinProgressBar(Tacho, i, title=paste( round(i/m*100, 0),"% done"))
  }
  x = TestData[i,]
  # quadrierte Euclid distanz rechnen
  Diff = TrainData - matrix(ones(n*AnzVariablen,1)*x,ncol=AnzVariablen,byrow=TRUE) # differenz zu allen
  Diff = t(Diff^2)   #  Quadrierte differenzen
  Diff = matrix(colSums(Diff))        # summe der Quadrierten Differenzen
  Sorted <- sort(na.last=NA,Diff,index.return = TRUE)
  SortedDiff = matrix(Sorted$x)
  SortedInd = matrix(Sorted$ix)
  NearestInd[i,] = t(SortedInd[1:k])
  NearestClasses = TrainCls[NearestInd[i,]]
  CC <- ClassCount(NearestClasses)
  UniqueClasses = CC$UniqueClasses
  CountPerClass = CC$CountPerClass
  NrOfClasses = CC$NumberOfClasses # find the most frequent class
  MaxNumber = max(CountPerClass)
  MaxInd = which(CountPerClass==MaxNumber,arr.ind=TRUE)
  MaxInd = min(MaxInd); # falls es 2 gibt;
  MostFrequentCls = UniqueClasses[MaxInd]
  KNNTestCls[i] = MostFrequentCls
}
#close(Tacho)

return(list(KNNTestCls=KNNTestCls, NearestInd=NearestInd))
}
