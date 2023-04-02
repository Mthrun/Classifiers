\name{RadialBasisNNclassifier}
\alias{RadialBasisNNclassifier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RadialBasisNNclassifier
}
\description{
The use of an RBF network is similar to that of an mlp. The idea of radial basis function networks comes from function interpolation theory. The RBF performs a linear combination of n basis functions that are radially symmetric around a center/prototype.
}
\usage{
RadialBasisNNclassifier(TrainData, TrainCls, TestData, PlotIt = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TrainData}{
(1:n,1:d)  matrix, data Array of n cases withd variables of TrainData or Full data
}

  \item{TrainCls}{
vector, Array of variable names
}
  \item{TestData}{
Optional, (1:m,1:d)  matrix, data Array of d cases with n variables of TestData
}
  \item{PlotIt}{
TRUE: plots two plots evaluating the model
}
  \item{\dots}{
see \code{\link[RSNNS]{rbf}}
}
}
\details{
RBF networks are feed-forward networks with one hidden layer. Their activation is not sigmoid (as in MLP), but radially symmetric (often gaussian). Thereby, information is represented locally in the network (in contrast to MLP, where it is globally represented). Advantages of RBF networks in comparison to MLPs are mainly, that the networks are more interpretable, training ought to be easier and faster, and the network only activates in areas of the feature space where it was actually trained, and has therewith the possibility to indicate that it "just doesn't know".
}
\value{
list V with
\item{TestCls}{Null if no TestData Given, [1:m] vector of k classes otherwise}
\item{Classification}{[1:n], classification by RadialBasisNNclassifier of k classes}
\item{RBFmodel}{Object of rsnns}
}

\references{
Poggio, T. & Girosi, F. (1989), 'A Theory of Networks for Approximation and Learning'(A.I. Memo No.1140, C.B.I.P. Paper No. 31), Technical report, MIT ARTIFICIAL INTELLIGENCE LABORATORY.

Vogt, M. (1992), 'Implementierung und Anwendung von Generalized Radial Basis Functions in einem Simulator neuronaler Netze', Master's thesis, IPVR, University of Stuttgart. (in German)

Zell, A. et al. (1998), 'SNNS Stuttgart Neural Network Simulator User Manual, Version 4.2', IPVR, University of Stuttgart and WSI, University of Tübingen. http://www.ra.cs.uni-tuebingen.de/SNNS/welcome.html

Zell, A. (1994), Simulation Neuronaler Netze, Addison-Wesley. (in German)
}
\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[RSNNS]{rbf}}
}
\examples{
library(FCPS)
data("Chainlink")
Data=Chainlink$Data
Cls=Chainlink$Cls
split=Classifiers::splitquoted(Data,Cls,Percentage = 80)
out=RadialBasisNNclassifier(split$TrainData,TrainCls = split$TrainCls,TestData = split$TestData)
table(out$TestCls,split$TestCls)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
