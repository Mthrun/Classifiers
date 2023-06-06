bootstrapSample = function(Data,k=1000){
  #   V = bootstrapSample(Cls,Percentage=50)
  #   TrainInd=V$TrainInd
  #   TestInd=V$TrainInd
  #   V = bootstrapSample(Cls,Data,Percentage)
  #   TrainInd=V$TrainInd
  #   TestInd=V$TrainInd
  #   TrainData=V$TrainData
  #   TestData=V$TestData
  # sampling with replacement that equals the 0.632 bootstrap method
  #   the training sample is equal the size of original data
  #   #all samples not beeing in training are in testing, approx 36.8 % od the data
  #   testing indices are filled with 0 to generate a matrix
  #  
  #   INPUT
  #   Cls                 [1:n] numeric vector of classifications
  #   Data                Optional, [1:n,1:d] matrix with n cases and d features
  #   k                    number of bootstrap taining and test samples              
  #   OUTPUT
  #   TrainInd, TestInd     Indices of the split of the Cls

  #Details: sample without replacement is performend in the default case.
  #author: MCT, 2023
  
  if(is.matrix(Data)){
    N=nrow(Data)
  }else if(is.vector(Data)){
    N=length(Data)
  }else{
    Data=as.matrix(Data)
    N=nrow(Data)
  }

  #permute
  ind=sample(1:N,size = N*k,replace = T)
  
  indBootstrap=matrix(ind,nrow = N,byrow = T,ncol = k)
  
  TestIndV=apply(indBootstrap, 2, function(a,b) {
    #all not drawn are in test set, approximately 36.8%
    test_ind=setdiff(y = unique(a),b)
    return(test_ind)
  },1:N)
  TestInd=do.call(DataVisualizations::CombineCols,TestIndV)
  colnames(TestInd)=NULL
  TestInd=as.matrix(TestInd)
  TestInd[!is.finite(TestInd)]=0
  
  return(list(TrainingInd=indBootstrap,TestInd=TestInd))
}

