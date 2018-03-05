\name{SupportVectorMachines}
\alias{SupportVectorMachines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Support Vector Machines (SVM)
}
\description{
This classifiers divides data into two classes. SVM searches for the right hyperplane in order to seperate data by a prior classification in an multidimensional space. SVM uses the the scalar product and optimizes in general as follows: It maximizes the distance of the hyper plane to the nearest points. These nearest points of the hyperplane are called support vectors.

Ofthe the data is porjected to a space of higher dimensions to make it linearly seprable by using the kernel trick, which is in short a redefinition of the scalar product.
}
\usage{
SupportVectorMachines(TrainData,TrainCls,TestData,

Method="C-classification",Scale=rep(FALSE,ncol(TrainData)),

Kernel="linear",Gamma=round(1/ncols(TrainData),2),CoefR=0,PolynomialDegree=3,CostC=1,Nu)
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
  \item{Method}{
either 'C-classification','nu-classification','one-classification' (for novelty detection),'eps-regression', or 'nu-regression'
}
  \item{Scale}{
A logical vector indicating the variables to be scaled. If scale is of length 1, the value is recycled as many times as needed. Per default, data are scaled internally (both x and y variables) to zero mean and unit variance. The center and scale values are returned and used for later predictions.
}
  \item{Kernel}{
      either 'radial', 'polynomial' or 'sigmoid' or 'linear'.
      Linear means that the normal scalar product is used and a linear seperation of data is possible.
      If the data has high dimensionality a linear seperation cannot be assumed. The other kernels have to be       tried out (e.g. with cross-validation for small data sets or repeatadly with split quote approach)
}
  \item{Gamma}{
    parameter needed for all kernels except linear (default: 1/(data dimension))
  }
  \item{CoefR}{
    The addition coefficient for the kernels, it parameter needed for kernels of type polynomial (degree zero) and sigmoid with the default 0
  }
  \item{CostC}{
penalty parameter of the error term, cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation. Good values could be 0.01, 0.1, 1, 10, 100, 1000. You have to test several combinations to find the right one.
}
  \item{PolynomialDegree}{
    parameter needed for kernel of type polynomial (default: 3)
}
  \item{Nu}{
   integer value (0,1] (higher than zero and up to the number 1) setting the additional parameter described in details.
}
}
\details{
C-classification: Two class problem with conventional optimization for SVM .

nu-classification: Two class problem with \code{Nu} optimization for SVM. It is proved thatnu an upper bound on the fraction of training errors and a lower bound of the fraction of support vectors.

one-classification: One class SVM for estimating the support of a high-dimensional distribution

eps-regression: SVM regression approach; in this case the Cls vector does not have to consist of integer values defining classes but can have decimal values

nu-regression: SVM regression approach by using additional parameter \code{Nu} to control the number of support vectors. In this case the Cls vector does not have to consist of integer values defining classes but can have decimal values.
}
\value{
List of
  \item{Cls}{
  [1:n,1] classification for test data with n cases the classifier can be evaluated with.
  }


  \item{Probabilities}{
  the probabilites of a data point belonging to a class. They are the not rounded values of \code{Cls}
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
Chang, Chih-Chung, and Chih-Jen Lin: LIBSVM: a library for support vector machines, ACM transactions on intelligent systems and technology (TIST), 2. Jg., Nr. 3, S. 27, 2011.
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
data(USelectionPoll)
#It is preferable to scale the data between zero and 1
\donttest{
data(USelectionPoll)

#Data Has to be divided betwe ino train and test set
# 80 percent in train set because of pareto rule
TrainAndTest=Classifiers::splitQuoted(USelectionPoll$Data,USelectionPoll$Cls,Percentage = 80)
trainset <- TrainAndTest$TrainData
testset <- TrainAndTest$TestData
#build model – radial kernel and C-classification (soft margin) with default cost (C=1)
##It is preferable to scale the data between zero and 1
# However this should be done manually and not with this function
Results=SupportVectorMachines(TrainData = trainset,TrainCls = TrainAndTest$TrainCls,

TestData = testset,Scale=rep(TRUE,ncol(trainset)),Kernel = "radial",CostC = 100)


#TrainSet - here to model works perfectly
table(as.numeric(Results$Model$fitted),TrainAndTest$TrainCls)
Classifiers::AnalysisOfClassifier(as.numeric(Results$Model$fitted),TrainAndTest$TrainCls)
#TestSet - here to model does not work
Classifiers::AnalysisOfClassifier(Results$TestCls,TrainAndTest$TestCls)
table(Results$TestCls,TrainAndTest$TestCls)
#that is the reason, that a classifier should always be evaluated on the test set,
#a data set which is not used to build the model
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SupportVectorMachines}% use one of  RShowDoc("KEYWORDS")
\keyword{svm}% __ONLY ONE__ keyword per line
\keyword{Support Vector Machines}% use one of  RShowDoc("KEYWORDS")