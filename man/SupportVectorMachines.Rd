\name{SupportVectorMachines}
\alias{SupportVectorMachines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Support Vector Machines
}
\description{
...
}
\usage{
SupportVectorMachinesn(TrainData,TrainCls,TestData,

method="C-classification",scale=rep(FALSE,ncol(TrainData)),

kernel="linear",degree=3,gamma=0.25,coef0=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TrainData}{
  [1:m,1:d] dataset with m cases and d columns the classifier is build with.
}
  \item{TrainCls}{
  [1:m,1] classification as a numeric vector for the m cases the classifier is trained with.
}
  \item{TestData}{
  [1:n,1:d] dataset with n cases and d columns the classifier uses to generate new classification \code{Cls} for testing purposes.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{scale}{
%%     ~~Describe \code{method} here~~
}
  \item{kernel}{
%%     ~~Describe \code{kernel} here~~
}
  \item{degree}{
%%     ~~Describe \code{degree} here~~
}
  \item{gamma}{
%%     ~~Describe \code{gamma} here~~
}
  \item{coef0}{
%%     ~~Describe \code{coef0} here~~
}
}
\details{
many details to write about
}
\value{
List of
  \item{Cls}{
  [1:n,1] classification for test data with n cases the classifier can be evaluated with.
  }


  \item{Probabilities}{
%%     ~~Describe \code{coef0} here~~
}
  \item{Model}{
An object of class "svm" containing the fitted model, including:

SV: The resulting support vectors (possibly scaled).

index:  The index of the resulting support vectors in the data matrix. Note that this index refers to the preprocessed data (after the possible effect of na.omit and subset)

coefs:  The corresponding coefficients times the training labels.

rho:  The negative intercept.

sigma:  In case of a probabilistic regression model, the scale parameter of the hypothesized (zero-mean) laplace distribution estimated by maximum likelihood.

probA, probB: numeric vectors of length k(k-1)/2, k number of classes, containing the parameters of the logistic distributions fitted to the decision values of the binary classifiers (1 / (1 + exp(a x + b))).
}
}
\references{
...
}
\author{
For this Wrapper: Michael Thrun, for the internal algorithm please see \code{e1071} package
}
\note{
Michael Thrun just extended the Documentation and build an easier to use wrapper for the \code{e1071::svm} function
}

\seealso{
\code{\link{e1071::svm}}
}
\examples{
#will be written
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SupportVectorMachines}% use one of  RShowDoc("KEYWORDS")
\keyword{svm}% __ONLY ONE__ keyword per line
\keyword{Support Vector Machines}% use one of  RShowDoc("KEYWORDS")