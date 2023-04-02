\name{splitQuoted}
\alias{splitQuoted}
\title{splitQuoted}

\description{Quota-split of Data randomly into disjunct sets TrainCls and TestCls, with |TrainCls| = |TestCls|*Percentage/100 splitting is done such that the percentages are the same over all classes. Data can be also divided into disjunct set in this function otherwise use indices to split Data in a later step
}
\usage{
splitQuoted(Cls,Data,Percentage, Key,LowLim)
}
\arguments{
  \item{Cls}{[1:n] numeric vector of classifications}
  \item{Data}{Optional, [1:n,1:d] Matrix of n cases and d features, or n d-dimensional datapoints}
  \item{Percentage}{Scalar, value between 1 and 99, default is 80}
  \item{Key}{Optional, [1:n] vector, containing the keys. Default is either 1:length(Cls) or rownames(Data) if existing}
  \item{LowLim}{Optional, Scalar giving the minimum number if cases per class, if the minimum is higher than the number of elements in class, sampling is done with replacement}
}
\value{
List with
\item{TrainInd }{Numeric vector, containing the indices of the the Cls/Data entries in the Train split}
\item{TestInd }{Numeric vector, containing the indices of the the Cls entries in the Test split'}
\item{TrainCls }{Numeric vector, containing the cls entries in the Train split}
\item{TestCls }{Numeric vector, containing the cls entries in the Test split}
\item{TrainData }{Numeric vector, containing the Data entries in the Train split. If Data is missing, then TrainData is NULL}
\item{TestData }{Numeric vector, containing the Data entries in the Test split. If Data is missing, then TestData is NULL}
\item{TrainKey }{Numeric vector, containing the key entries in the Train split}
\item{TestKey }{Numeric vector, containing the key entries in the Test split}
}
\details{
Quota-split of Data randomly into disjunct sets TrainData and TestData, with |TrainData| = |Data|*Percentage/100.
Splitting is done such that the percentages are the same over all classes.
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
