\name{CentroidClassifier}
\alias{CentroidClassifier}
\title{ Centroid Classifier}
\description{
Assigns each point of data the class of its centroid. }
\usage{
CentroidClassifier(data, cls, testData, centroidMethod)
}
\arguments{
\item{data}{ A matrix containing the learn data for the classifier.} 
\item{cls}{ The clusternumber of each point of data. A vector.}
\item{testData}{ The data to be classified. Use Data, if not given.}
\item{centroidMethod}{ The centroid method to be used. Could be the standard centroid or mean.}
}
\value{
\item{assignedCls}{The classes assigned to the data. }
\item{uniqueCls}{(NumberOfClass)The unique classes in Cls. }
\item{centroid(NumberOfClass)}{Centroid(i) is the centroid of the data points in the class with the number i. }
} 
\author{ Raphael Paebst}
