\name{BuildNaiveBayesClassifier}
\alias{BuildNaiveBayesClassifier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BuildNaiveBayesClassifier
}
\description{
Bau eines Naiven Bayes Klassifiers:
}
\usage{
BuildNaiveBayesClassifier(Data,Cls, PlotIt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
 (1:d,1:n)            the  data n-dimensional data, cases in rows
}
  \item{Cls}{
(1:n)                 Cls(i) = Class of Data(i,:)
}
  \item{PlotIt}{
==TRUE (default) plots are made, Currenty unavailible 
}

}

\value{
 BayesCls(1:d)                    die Klassifizierung des naive bayes Klassifikators

 DAS BAYES MODELL
 
 UniqueClasses(1:NrOfClasses)     Klassenbeziechnungen
 
 MeanPerClass(1:NrOfClasses,1:n)  klassenbezogene Mittelwerte fuer jede Variable
 
 MeanPerClass(1:NrOfClasses,1:n)  klassenbezogene Std fuer jede Variable
 
 WeightsPerClass(1:NrOfClasses)   relative Klassengroesse
  
}

\author{
Michael Thrun
}
