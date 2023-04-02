\name{KNNclassifierDistance}
\alias{KNNclassifierDistance}
\title{
k-Nearest Neighbour Classification of Versatile Distance Version
}
\description{
k-nearest neighbour classification of versatile Distance version for test set
from training set. For each row of the test set, the k nearest (in multiple
distances) training set vectors are found, and the classification is decided by
majority vote. This function allows you measure the distance bewteen vectors by
six different means. K Threshold Value Check and Same K_i Problem Dealing are
also been considered.
}
\usage{
KNNclassifierDistance(K = 1, TrainData, TrainCls, TestData=NULL,ShowObs=F,
method = "euclidean", p =2, DTW_windowsize=5)
}
\arguments{
\item{K}{
number of top K nearest neighbours considered.
}
\item{TrainData}{
matrix or data frame of training set cases.
}
\item{TrainCls}{
matrix or data frame of true classifications of training set.
}
\item{TestData}{
matrix or data frame of test set cases. A vector will be interpreted as a row
vector for a single case.
}

\item{ShowObs}{
logical, when it's ture, the funtion will output the imformation of training set
cases.
}
\item{method}{
the distance measure to be used in the \code{DistanceMatrix} function. This must
be one of 'euclidean','sqEuclidean','binary','cityblock', 'maximum','canberra',
'cosine','chebychev','jaccard', 'mahalanobis','minkowski','manhattan','braycur',
'cosine'. Any unambiguous substring can be given.
}
\item{p}{
The power of the Minkowski distance.
}
}
\details{
K Threshold Value is stipulated to be less than the minimum size of the class in
training set, or a warning will be shown.

Sometimes a case may get same "ballot" from class A and class B
(even C, D, ...), this time a weighted voting process will be activated. The
weight is based on the actual distance calculated between the test case and  K
cases in neighbor A and B. The test case belongs to the class with less total
distance.

The multiple distances are implemented by transfering the function dist(). For
the convenience of users, we quote the details of function "dist()" and show
them here.

Available distance measures are :

euclidean:
Usual square distance between the two vectors (2 norm).

maximum:
Maximum distance between two components of x and y (supremum norm)

manhattan:
Absolute distance between the two vectors (1 norm).

canberra:
sum(abs(Xi-Yi)/abs(Xi+Yi))
Terms with zero numerator and denominator are omitted from the sum and treated
as if the values were missing.

This is intended for non-negative values (e.g. counts): taking the absolute
value of the denominator is a 1998 R modification to avoid negative distances.

binary:
(aka asymmetric binary): The vectors are regarded as binary bits, so non-zero
elements are "on" and zero elements are "off". The distance is the proportion of
bits in which only one is on amongst those in which at least one is on.

minkowski:
The p norm, the pth root of the sum of the pth powers of the differences of the
components.

Missing values are allowed, and are excluded from all computations involving the
rows within which they occur. Further, when Inf values are involved, all pairs
of values are excluded when their contribution to the distance gave NaN or NA.
If some columns are excluded in calculating a Euclidean, Manhattan, Canberra or
Minkowski distance, the sum is scaled up proportionally to the number of columns
used. If all pairs are excluded when calculating a particular distance, the
value is NA.

}
\value{
\item{result}{result of classifications of test set will be returned. 
(When TstX is NULL, the function will automatically consider the user is trying
to test the knn algorithm. Hence, a test result table and accuracy report will
be shown on the R-console.)
}
\item{KNNTestCls}{vector [1:m], a KNN classification of TestData}
}
\references{

Ripley, B. D. (1996) Pattern Recognition and Neural Networks. Cambridge.

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S.
Fourth edition. Springer.

}
\author{
Michael Thrun, Xinmiao Wang [cph]
}
\note{
taken from the knnGarden package and rewritten for dbt purposes

If you want to use the distance measure "binary", the vectors must be binary
bits, non-zero elements are "on" and zero elements are "off".
}

\examples{
#dbt approach
\donttest{
datas=splitquoted(FCPS$Hepta$Data,FCPS$Hepta$Cls,80)
KNNTestCls=as.numeric(KNNclassifierDistance(K=3,datas$TrainData,datas$TrainCls,datas$TestData,ShowObs=FALSE,method="euclidean",p = 2)$KNNTestCls)
ClsToTrueCls(KNNTestCls,datas$TestCls)$accuracy
}
library(Distances)
#general approach
data(iris)
## Define data
TrnX<-iris[,1:4]
OrigTrnG<-iris[,5]
#
TstX<-iris[c(1:20,50:70,120:140),1:4]
#or
TstX<-NULL
## Call function
KNNclassifierDistance(K=5,TrnX,OrigTrnG,TstX,ShowObs=FALSE,method="euclidean",p = 2)
}
\keyword{ knn }
\keyword{ dist }












