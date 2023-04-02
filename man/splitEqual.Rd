\name{splitEqual}
\alias{splitEqual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
splitEqual
}
\description{
Equal split of Data randomly into disjunct sets TrainCls and TestCls, with |TrainCls| = |TestCls|*Percentage/100. Splitting is done such that given a percentage the cases over all classes are the same. Data can be also divided into disjunct set in this function otherwise use indices to split Data in a later step.
}
\usage{
splitEqual(Cls, Data, Percentage, ForceEqual = FALSE, Key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Cls}{
[1:n] Numeric vector of classifications, with k unique classes
}
  \item{Data}{
Optional, [1:n,1:d] Matrix of n cases and d features, or n d-dimensional datapoints
}

  \item{Percentage}{
Scalar, gives the percentage for the split as value between 1 and 99. If missing default is 50
}
  \item{ForceEqual}{
TRUE: sample with replacement if percentage of full cases is above the number of cases in a class, default: FALSE: less cases are sampled resulting in an still unequal split If a class has a low number of cases
  }
  \item{Key}{
Optional, [1:n] vector, containing the keys. Default is either 1:length(Cls) or rownames(Data) if existing
}
}

\value{
List with,
\item{TrainInd }{Numeric vector, containing the indices of the the Cls/Data entries in the Train split}
\item{TestInd }{Numeric vector, containing the indices of the the Cls entries in the Test split}
\item{TrainCls }{Numeric vector, containing the cls entries in the Train split}
\item{TestCls }{Numeric vector, containing the cls entries in the Test split}
\item{TrainData }{Numeric vector, containing the Data entries in the Train split. If Data is missing, then TrainData is NULL}
\item{TestData }{Numeric vector, containing the Data entries in the Test split. If Data is missing, then TestData is NULL}
\item{TrainKey }{Numeric vector, containing the key entries in the Train split}
\item{TestKey }{Numeric vector, containing the key entries in the Test split}
}

\details{
Sample without replacement is performend in the default case. 

TrainInd and TestInd are permutated.
}

\author{
Michael Thrun
}

\examples{
data("USelectionPoll")
V=splitEqual(USelectionPoll$Cls,Percentage=50)
}

\keyword{Split}
\keyword{Splitting}
\concept{Data preprocessing}
