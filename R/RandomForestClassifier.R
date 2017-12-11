RandomForestClassifier=function(Data,Names,Cls,NumberOfTrees=500,VariableImportance=TRUE,Seed,PlotIt=TRUE,Verbose=F){
# Forest = RandomForestClassifier(Data,Cls,NumberOfTrees=500)
# randomForest classifier
#
# INPUT
# Data(1:d,1:n)        matrix, data Array of d cases with n variables
# Names(1:n,:)         vector, Array of variable names
# Cls(1:d)             vector of classes, d integer numbers, number k indicates cluster k
#
# OPTIONAL
# NumberOfTrees       Number of trees to grow. This should not be set to a too small number, to enshure that every input row gets predicted at least a few times 
# VariableImportance  Should importance of predictors be assessed?
# Seed								set a seed for randomization
# PlotIt							wether to ploz error versus trees
# Verbose             whether to show results of forest, default FALSE
#
# OUTPUT list V with
# Classification			[1:d], classification by randomForest
# ContingencyTable    Vergleich Cls zu Classification
# Forest  						Object of randomForest
#
# Details:  generation of a randomForest classification	
# author MT 2017
  #requireNamespace("pmml")
  requireNamespace("randomForest")
  colnames(Data)=Names
	if(!missing(Seed))
			set.seed(Seed)
		
	results = randomForest::randomForest(as.factor(Cls) ~ .,data =  Data, ntree = NumberOfTrees, na.action = randomForest::na.roughfix, 
                                              importance=VariableImportance, proximity=TRUE, replace = T
	)
	if(Verbose)
		print(results)
	if(PlotIt)
		plot(results)
		
return(list(Classification=as.numeric(results$predicted),ContingencyTable=results$confusion,Forest=results))
}