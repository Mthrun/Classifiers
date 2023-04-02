\name{RandomForestClassifier}
\alias{RandomForestClassifier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Forest Classifier
}
\description{
Training of a random forest classification, if required also predicts classification on test data
}
\usage{
RandomForestClassifier(TrainData, TrainCls, TestData, Names,
NumberOfTrees=500, VariableImportance=TRUE, Seed,
PlotIt=TRUE, Verbose=FALSE, ABCanalysis=FALSE, Fast=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{TrainData}{(1:n,1:d)  matrix, data Array of n cases withd variables of TrainData or Full data}
\item{TrainCls}{vector, Array of variable names}
\item{TestData}{Optional, (1:m,1:d)  matrix, data Array of d cases with n variables of TestData}
\item{Names}{Optional, (1:d)         vector, Array of variable names, if not given, colnames are used}
\item{NumberOfTrees}{Number of trees to grow. This should not be set to a too small number, to enshure that every input row gets predicted at least a few times}
\item{VariableImportance}{Should importance of predictors be assessed?}
\item{Seed}{set a seed for randomization}
\item{PlotIt}{wether to plot error versus trees}
\item{Verbose}{whether to show results of forest, default FALSE}
\item{ABCanalysis}{Defualt FALSE, If TRUE select indizes of group A of feautures with highest accuracy values by computed ABCanalysis, only if VariableImportance==TRUE}
\item{Fast}{default FALSE ( \pkg{randomForest} is used. TRUE:  \pkg{randomForestSRC} is used but VariableImportance and 
ABCanalysis is set FALSE, because its not implemented for this case.}
}
\details{
reiman's random forest algorithm (based on Breiman and Cutler's original Fortran code) for classification. The “local” (or casewise) variable importance is computed as follows: For classification, it is the increase in percent of times a case is OOB and misclassified when the variable is permuted, in more detail:

the prediction error on the out-of-bag portion of the TrainData is recorded (error rate for each tree). Then the same is done after permuting each predictor variable. The difference between the two are then averaged over all trees, and normalized by the standard deviation of the differences. If the standard deviation of the differences is equal to 0 for a variable, the division is not done (but the average is almost always equal to 0 in that case).
}
\value{
list V with
\item{Classification}{[1:n], classification by randomForest of k clusters}
\item{ImportancePerVariable}{[1:d,1:2], Importance of Variables by Gini and Accuracy}
\item{ContingencyTable}{Vergleich TrainCls zu Classification}
\item{ImportancePerClass}{Importance of Variables per Cluster j of k clusters of TrainCls}
\item{Forest}{Object of randomForest}
\item{MostImportantFeatures}{Feautures Index of the first m most-important features defined by group A ob ABCanalysis}
\item{TestCls}{Null if no TestData Given, [1:m] vector of k classes otherwise}
}
\references{
Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32.

Breiman, L (2002), “Manual On Setting Up, Using, And Understanding Random Forests V3.1”, https://www.stat.berkeley.edu/~breiman/Using_random_forests_V3.1.pdf.
}
\author{
Michael Thrun
}
\examples{
library(FCPS)
data("Chainlink")
Data=Chainlink$Data
Cls=Chainlink$Cls
split=Classifiers::splitquoted(Data,Cls,Percentage = 80)
out=RandomForestClassifier(split$TrainData,TrainCls = split$TrainCls,TestData = split$TestData)
table(out$TestCls,split$TestCls)
}

\keyword{Forest}% use one of  RShowDoc("KEYWORDS")
\keyword{RandomForest}% __ONLY ONE__ keyword per line
