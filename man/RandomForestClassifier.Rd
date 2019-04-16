\name{RandomForestClassifier}
\alias{RandomForestClassifier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
randomForest classifier
}
\description{
generation of a randomForest classification	
}
\usage{
RandomForestClassifier(Data,Names, Cls, NumberOfTrees = 500, VariableImportance = TRUE, Seed, PlotIt = TRUE, Verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
(1:d,1:n)  matrix, data Array of d cases with n variables
}
  \item{Names}{
(1:n,:)         vector, Array of variable names
}
  \item{Cls}{
vector, Array of variable names
}
  \item{NumberOfTrees}{
Number of trees to grow. This should not be set to a too small number, to enshure that every input row gets predicted at least a few times 
}
  \item{VariableImportance}{
Should importance of predictors be assessed?
}
  \item{Seed}{
set a seed for randomization
}
  \item{PlotIt}{
wether to ploz error versus trees
}
  \item{Verbose}{
whether to show results of forest, default FALSE
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
list V with

Classification			[1:d], classification by randomForest

ContingencyTable    Vergleich Cls zu Classification

Forest  						Object of randomForest
}
\references{
Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32.

Breiman, L (2002), “Manual On Setting Up, Using, And Understanding Random Forests V3.1”, https://www.stat.berkeley.edu/~breiman/Using_random_forests_V3.1.pdf.
}
\author{
Michael Thrun
}

\keyword{Forest}% use one of  RShowDoc("KEYWORDS")
\keyword{RandomForest}% __ONLY ONE__ keyword per line
