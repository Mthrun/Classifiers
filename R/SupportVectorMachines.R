SupportVectorMachines=function(TrainData,TrainCls,TestData,method="C-classification",scale=rep(FALSE,ncol(TrainData)),kernel="linear",degree=3,gamma=0.25,coef0=0){
  requireNamespace('e1071')
  model=e1071::svm(x=TrainData,y=TrainCls,scale=scale,type=method,kernel=kernel,degree=degree,gamma=gamma,coef0=coef0)
  
  pred_train <-predict(model,TestData)
  
  pred_train=round(as.numeric(pred_train),0)
  

  return(list(Cls=round(pred_train,0),Probabilities=as.numeric(pred_train),Model=model))
}