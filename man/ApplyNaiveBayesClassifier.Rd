\name{ApplyNaiveBayesClassifier}
\alias{ApplyNaiveBayesClassifier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ApplyNaiveBayesClassifier
}
\description{
 Anwendung eines Naiven Bayes Klassifiers:
}
\usage{
ApplyNaiveBayesClassifier(Data,UniqueClasses,MeanPerClass,StdPerClass,
WeightsPerClass, PlotIt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{(1:d,1:n) the data n-dimensional data, cases in rows
}
\item{UniqueClasses}{ (1:NrOfClasses) Klassenbeziechnungen}
\item{MeanPerClass}{(1:NrOfClasses,1:n)  klassenbezogene Mittelwerte fuer jede
Variable}
\item{StdPerClass}{(1:NrOfClasses,1:n) klassenbezogene Std fuer jede Variable}
\item{WeightsPerClass}{(1:NrOfClasses) relative Klassengroesse}
\item{PlotIt}{==T (default) plots are made}
}
\value{
\item{BayesCls}{(1:d) die Klassifizierung des naive bayes Klassifikators}
}
\author{
Michael Thrun
}
