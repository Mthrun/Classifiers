\name{KNNclassifier}
\alias{KNNclassifier}
\title{  k-nearest neighbor clssifier}
\usage{
KNNclassifier(k, TrainData, TrainCls, TestData,  Verbose)
}
\arguments{
\item{k}{the number of neighbor to use}
\item{TrainData}{matrix [n,d] containing classified data}
\item{TrainCls}{vector [1:n] containing the classes of TrainData}
\item{TestData}{matrix [m,d] containing unclassified data Verbose             default ==1, progress report}
\item{Verbose}{Optional: progress report. Default: 1}
}
\value{
\item{KNNTestCls}{Vector [1:m], a KNN classification of TestData}
}
\description{
  k-nearest neighbor clssifier}
\author{
04/15 RG, imported from matlab}
