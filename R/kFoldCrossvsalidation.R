kFoldCrossvsalidation=function(Data,k,ForceEqualSize=TRUE){
  N=nrow(Data)
  #permute
  ind=sample(1:N,N)
  folds=cut(ind,breaks=k,labels=FALSE)
  ind_test=list()
  ind_train=list()
  for(i in 1:k){
    indcur=which(folds==i)
    ind_test[[i]]=indcur
    ind_train[[i]]=setdiff(ind,indcur)
  }
  if(isTRUE(ForceEqualSize)){
    
  TestMat=suppressWarnings(do.call(cbind,ind_test))
  TrainingMat=suppressWarnings(do.call(cbind,ind_train))
  }
  if(isFALSE(ForceEqualSize)){
    TestMat=do.call(cbind,ind_test)
    TrainingMat=do.call(cbind,ind_train)
    for(i in 1:k){
      TestMat[duplicated(TestMat[,i]),i]=0
      TrainingMat[duplicated(TrainingMat[,i]),i]=0
    }
  }
  return(list(TrainingInd=TrainingMat,TestInd=TestMat))
}
  
