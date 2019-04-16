\name{splitquoted}
\alias{splitquoted}
\title{splitquoted}

\description{quota-split of Data randomly into disjunct sets TrainData and TestData}
\usage{
splitquoted(Data, Cls, Percentage)
}
\arguments{
  \item{Data}{vector or matrix, n cases,  d variables}
  \item{Cls}{vector of classifications}
  \item{Percentage}{ between .1 and 99.9}
}
\value{List with
  \item{TrainData}{}
  \item{TestData}{}
  \item{TrainCls}{}
  \item{TestCls}{}
  The splitted data and classifications
}
\details{
quota-split of Data randomly into disjunct sets TrainData and TestData, with |TrainData| = |Data|*Percentage/100.
Splitting is done such that the percentages are the same over all classes.
}