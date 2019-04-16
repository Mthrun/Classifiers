splitQuoted4CrossValidation =function(Data,Cls,k, Key=NULL){
  
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
  # % k                 number of parts
  # % OUTPUT
  # % CrossData, CrossCls    the split Data
  #   AllClassesInd  indizes of the Datapoints
#if cannot be divided trhough k, the last part gets die nicht teilbaren cases
  #author MT 2018
  warning('Sill in test phase. Use with care')
  UniqueClasses = unique(Cls);
  AnzClasses = length(UniqueClasses);
  
  if(is.null(Key)) Key = 1:(nrow(Data))
  
  # TestData <- matrix(ncol=ncol(Data),nrow=0)
  # TrainData <- matrix(ncol=ncol(Data),nrow=0)
  # TrainCls <- vector()
  # TestCls <- vector()
  
  TrainInd <- c()
  TestInd <- c()
  Percentage=round(100/k,0)
 
 AllClassesInd=list()
  for(i in UniqueClasses){
    ClassIndSamples=list()
    ClassInd <- which(Cls==i)
    samplecurrent=c()
    for(kn in 1:k){
      #print(kn)
  
      # if(kn==1)
      #   sampleInd <- sample(ClassInd, round(length(ClassInd)* Percentage/100))
      # else
      currentind=setdiff(ClassInd, samplecurrent)
      numberdraw=round(length(ClassInd)* Percentage/100)
      if(length(currentind)>=numberdraw)
        sampleInd <- sample(currentind, numberdraw)
      else
        sampleInd <- sample(currentind, length(currentind))
      
        samplecurrent=c(samplecurrent,sampleInd)
        #ClassData <- Data[Cls==i,]
      #sampleInd <- sample(nrow(ClassData),round(nrow(ClassData)* Percentage/100))
        if(kn!=k)
          ClassIndSamples=c(ClassIndSamples,list(sampleInd))
        else
          ClassIndSamples=c(ClassIndSamples,list(c(sampleInd,setdiff(ClassInd,samplecurrent))))
    }
    AllClassesInd=c(AllClassesInd,list(ClassIndSamples))
  }
 CrossData=list(
 )
 CrossCls=list()
for(kn in 1:k){
  CrossDataTmp=matrix(0,nrow=0,ncol=ncol(Data))
  CrossClsTmp=c()
  for(i in UniqueClasses){
    CrossDataTmp=rbind(CrossDataTmp,Data[AllClassesInd[[i]][[kn]],])
    CrossClsTmp=c(CrossClsTmp,Cls[AllClassesInd[[i]][[kn]]])
  }
  CrossData=c(CrossData,list(CrossDataTmp))
  CrossCls=c(CrossCls,list(CrossClsTmp))
}
 return(list(CrossData=CrossData,CrossCls=CrossCls,AllClassesInd=AllClassesInd))
 
}

