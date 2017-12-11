\name{splitSample}
\alias{splitSample}
\title{splitSample}
\arguments{
\item{Data}{vector or matrix [n,d], n cases,  d variables}
\item{Percentage}{between .1 and 99.9}
}
\value{
  \item{TrainData, TestData}{the split Data}
  \item{TrainInd, TestInd}{such that TrainingData = Data[TrainInd,], TestData = Data[TestInd,]}
}
\description{
  Splits a data vector or matrix into training and test data. The size of the partitions is defined by the given percentage.
}
