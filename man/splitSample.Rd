\name{splitSample}
\alias{splitSample}
\title{splitSample}

\description{
Splits a data vector or matrix into training and test data. The size of the partitions is defined by the given percentage.
}

\usage{
splitSample(Data, Percentage)
}

\arguments{
\item{Data}{[1:n,1:d] Matrix of n cases and d features, or n d-dimensional datapoints}
\item{Percentage}{Scalar, value between 1 and 99}
}

\value{
  \item{trainData}{Numeric vector, containing the Data entries in the Train split}
  \item{testData}{Numeric vector, containing the Data entries in the Test split}
  \item{trainInd}{Numeric vector, containing the indices of the the Data entries in the Train split}
  \item{testInd}{Numeric vector, containing the indices of the the Cls entries in the Test split}
}

\author{
Michael Thrun
}

\examples{
data("USelectionPoll")
V=splitSample(USelectionPoll$Data,Percentage=50)
}

\keyword{Split}
\keyword{Splitting}
\concept{Data preprocessing}
