SupportVectorMachines=function(TrainData,TrainCls,TestData,method="C-classification",kernel="linear",degree,gamma,coef0){
  requireNamespace('e1071')
  model=e1071::svm(x=TrainData,y=TrainCls,type=type,kernel=kernel)#,degree=degree,gamma=gamma,coef0=coef0)
  
  pred_train <-predict(svm_model,TestData)
  
  pred_train=round(pred_train,0)
  

  return(list(Cls=round(pred_train,0),Probabilities=pred_train,Model=model))
}