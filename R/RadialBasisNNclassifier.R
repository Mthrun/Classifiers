RadialBasisNNclassifier=function(TrainData,TrainCls,TestData,PlotIt=FALSE,...){
  requireNamespace("RSNNS")
  rnd <- function(x)
    trunc(x + sign(x) * 0.5)
  
  model = RSNNS::rbf(x = TrainData, y = TrainCls, ...)
  
  
  ModelCls = NULL
  try({
    ModelCls = rnd(model$fitted.values)# TestCls=apply(predicted, 1, which.max)
  })
  
  TestCls = NULL
  
  if (!missing(TestData)) {
    predicted = predict(object = model, newdata = TestData)
    try({
      TestCls = rnd(predicted)
    })
  }

  if (isTRUE(PlotIt)) {
    RSNNS::plotIterativeError(model)
    RSNNS::plotROC(T = predicted, D = TestCls, main = "Model vs Training Data")
  }
  
  return(list(
    TestCls = TestCls,
    Classification = ModelCls,
    RBFmodel = model
  ))
  
}