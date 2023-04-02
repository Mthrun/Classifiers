RandomForestClassifier=function(TrainData,TrainCls,TestData,Names,
                                NumberOfTrees=500,VariableImportance=TRUE,
                                Seed,PlotIt=TRUE,Verbose=FALSE,ABCanalysis=FALSE,
                                Fast=FALSE){
# V = RandomForestClassifier(TrainData,TrainCls,NumberOfTrees=500)
# randomForest classifier
#
# INPUT
# TrainData(1:n,1:d)        matrix of n cases with d variables, either full data or training data
# TrainCls(1:n)             vector of classes, n integer numbers of k clusters, number j indicates cluster j<=k
#
# OPTIONAL
# TestData (1:m,1:d)        If Test TrainCls should be generated for evaluation of generalization efficiency
# Names(1:d,:)         Default: colnames of TrainData, if not missing then vector, Array of variable names
# NumberOfTrees       Number of trees to grow. This should not be set to a too small number, to enshure that every input row gets predicted at least a few times 
# VariableImportance  Should importance of predictors be assessed?
# Seed								set a seed for randomization
# PlotIt							wether to ploz error versus trees
# Verbose             whether to show results of forest, default FALSE
# ABCanalysis         Defualt FALSE, If TRUE select indizes of group A of feautures with highest Accuracy values by computed ABCanalysis
#
# OUTPUT list V with
# Classification        = V$Classification		  #  [1:n], classification by randomForest of k clusters
# ImportancePerVariable = V$ImportancePerVariable  #  [1:d,1:2], Importance of Variables by Gini and Accuracy
# ContingencyTable      = V$ContingencyTable    #  Vergleich TrainCls zu Classification
# ImportancePerClass    = V$ImportancePerClass  #  [1:k,1:d], Importance of Variables per Cluster j of k clusters of TrainCls: the prediction error
#                                               #  on the out-of-bag portion of the TrainData is recorded (error rate for each tree).
#                                               #  Then the same is done after permuting each predictor variable. The difference between 
#                                               #  the two are then averaged over all trees, and normalized by the standard deviation of the differences. 
#                                               #  If the standard deviation of the differences is equal to 0 for a variable, 
#                                               #  the division is not done (but the average is almost always equal to 0 in that case).
#
# Forest                = V$Forest  					  #  Object of randomForest
# MostImportantFeatures = V$MostImportantFeatures # [1:m] Feautures Index of the first m most-important features defined by group A ob ABCanalysis
# TestCls               = V$TestCls               #Null if no TestData Given, [1:m] vector of k classes otherwise
#
# Details:  generation of a randomForest classification	
  
# author MT 2017
# 1.Editor: MT 2019
  #requireNamespace("pmml")
  
  if(!is.matrix(TrainData)){
    warning('"TrainData" is not a matrix. calling as.matrix')
    TrainData=as.matrix(TrainData)
  }
  if(!is.vector(TrainCls)){
    warning('"TrainCls" is not a vector calling as.vector')
    TrainCls=as.vector(TrainCls)
  } 
  if(mode(TrainData)!="numeric"){
    warning('"TrainData" is not a numeric. stetting mode to numeric')
    mode(TrainData)="numeric"
  }
  if(!is.numeric(TrainCls)){
    warning('"TrainCls" is not a numeric calling as.numeric')
    TrainCls=as.numeric(TrainCls)
  } 
  NC=ncol(TrainData)
  if(missing(Names))
    Names=colnames(TrainData)
  else
    colnames(TrainData)=Names
  
	if(!missing(Seed))
			set.seed(Seed)
	
  if(isTRUE(Fast)){
    DF=as.data.frame(TrainData)
    DF$TrainCls=as.factor(TrainCls) 
    requireNamespace("randomForestSRC")
    model=randomForestSRC::rfsrc.fast(TrainCls ~ .,data =  DF, ntree = NumberOfTrees, 
                          importance=VariableImportance, proximity=TRUE,
                          forest=TRUE
    )
  }else{
    requireNamespace("randomForest")
    model = randomForest::randomForest(as.factor(TrainCls) ~ .,data =  TrainData, ntree = NumberOfTrees, na.action = randomForest::na.roughfix, 
                                       importance=VariableImportance, proximity=TRUE, replace = T
    )
  }

  if(isTRUE(VariableImportance)&isFALSE(Fast)){
  	Importance=randomForest::importance(model)
  	ind1=which(colnames(Importance)=='MeanDecreaseAccuracy')
  	ind2=which(colnames(Importance)=='MeanDecreaseGini')
  
  	ImportancePerVariable=Importance[,c(ind1,ind2)]
  
  	ImportancePerClass=Importance[,-c(ind1,ind2)]
  	#ordered=order(ImportancePerVariable[,2],decreasing = T)
  	#ImportancePerVariable=ImportancePerVariable[ordered,]
  }else{
    ImportancePerClass=NULL
    ImportancePerVariable=NULL
  }
	if(isTRUE(ABCanalysis)&isTRUE(VariableImportance)&isFALSE(Fast)){
	  requireNamespace("ABCanalysis")
	  ind=ABCanalysis::ABCanalysis(ImportancePerVariable[,1])$Aind
	}else{
	  ind=NULL
	}
	if(!missing(TestData)){
	  
	  if(!is.matrix(TestData)){
	    warning('"TestData" is not a matrix. calling as.matrix')
	    TestData=as.matrix(TestData)
	  }
	  if(mode(TestData)!="numeric"){
	    warning('"TestData" is not a numeric. stetting mode to numeric')
	    mode(TestData)="numeric"
	  }
	  if(NC!=ncol(TestData)){
	    warning('"TestData" has not the same numbr of columns as "TrainData"')
	  }
	  if(isTRUE(Fast)){
	    #TestCls = as.numeric(predict(model,newdata = as.data.frame(TestData)))
	    TestClsV = predict(model,newdata = as.data.frame(TestData))
	    predicted=TestClsV$predicted
	    TestCls=apply(predicted, 1, which.max)
	  }else{
	    TestCls = as.numeric(predict(model,newdata = as.matrix(TestData)))
	  }
	 
	}else{
	  TestCls=NULL
	}
	if(Verbose)
		print(model)
	if(PlotIt)
		plot(model)
		
return(list(Classification=as.numeric(model$predicted),
            ImportancePerVariable=ImportancePerVariable,
            ContingencyTable=model$confusion,
            ImportancePerClass=ImportancePerClass,
            Forest=model,
            MostImportantFeatures=ind,
            TestCls=TestCls))
}