SupportVectorMachines=function(TrainData,TrainCls,TestData,
                                                    
                                                    Method="C-classification",Scale=rep(FALSE,ncol(TrainData)),
                                                    
                                                    Kernel="linear",Gamma=round(1/ncol(TrainData),2),CoefR=0,PolynomialDegree=3,CostC=1,Nu)
{
#Results=SupportVectorMachines(TrainData,TrainCls,TestData, Method="C-classification",Scale=rep(FALSE,ncol(TrainData)),Kernel="linear",Gamma=round(1/ncols(TrainData),2),CoefR=0,PolynomialDegree=3,CostC=1,Nu) 
#
# Support Vector Machines (SVM)
# This classifiers divides data into two classes. SVM searches for the right hyperplane in order to seperate data by a prior classification in an multidimensional space. SVM uses the the scalar product and optimizes in general as follows: It maximizes the distance of the hyper plane to the nearest points. These nearest points of the hyperplane are called support vectors.
#   
# Ofthe the data is porjected to a space of higher dimensions to make it linearly seprable by using the kernel trick, which is in short a redefinition of the scalar product.
# INPUT
#   TrainData[1:m,1:d]      dataset with m cases and d columns the classifier is build with.
#   
#   TrainCls}[1:m,1]        classification as a numeric vector for the m cases the classifier is trained with.
#
#   TestData[1:n,1:d]       dataset with n cases and d columns the classifier uses to generate new classification \code{Cls} for testing purposes.
#
#   Method                  either 'C-classification','nu-classification','one-classification' (for novelty detection),'eps-regression', or 'nu-regression'
#   
#   Scale                   A logical vector indicating the variables to be scaled. If scale is of length 1, the value is recycled as many times as needed. Per default, data are scaled internally (both x and y variables) to zero mean and unit variance. The center and scale values are returned and used for later predictions.
#
#   Kernel                  either 'radial', 'polynomial' or 'sigmoid' or 'linear'.
#                           Linear means that the normal scalar product is used and a linear seperation of data is possible.
#                           If the data has high dimensionality a linear seperation cannot be assumed. The other kernels have to be       tried out (e.g. with cross-validation for small data sets or repeatadly with split quote approach)
#
#   Gamma                   parameter needed for all kernels except linear (default: 1/(data dimension))
#   CoefR                   The addition coefficient for the kernels, it parameter needed for kernels of type polynomial (degree zero) and sigmoid with the default 0
#
#   CostC                   penalty parameter of the error term, cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation. Good values could be 0.01, 0.1, 1, 10, 100, 1000. You have to test several combinations to find the right one.
#   
#   PolynomialDegree        parameter needed for kernel of type polynomial (default: 3)
#
#   Nu                      integer value (0,1] (higher than zero and up to the number 1) setting the additional parameter described in details.
#
# OUTPUT
# List of
#     Cls[1:n]               classification for test data with n cases the classifier can be evaluated with.
#     Probabilities[1:n]     the probabilites of a data point belonging to a class. They are the not rounded values of \code{Cls}
#     Model                  An object of class "svm" containing the fitted model, including:
#                             SV: The resulting support vectors (possibly scaled).
#     
#                             index:  The index of the resulting support vectors in the data matrix. Note that this index refers to the preprocessed data (after the possible effect of na.omit and subset)
#     
#                             coefs:  The corresponding coefficients times the training labels.
#     
#                             rho:  The negative intercept.
#     
#                             sigma:  In case of a probabilistic regression model, the scale parameter of the hypothesized (zero-mean) laplace distribution estimated by maximum likelihood.
#     
#                             probA, probB: numeric vectors of length k(k-1)/2, k number of classes, containing the parameters of the logistic distributions fitted to the decision values of the binary classifiers (1 / (1 + exp(a x + b))).
#author: MT 2018
  requireNamespace('e1071')

  if(missing(Nu))
    model=e1071::svm(x=TrainData,y=TrainCls,scale=Scale,type=Method,kernel=Kernel,degree=PolynomialDegree,gamma=Gamma,coef0=CoefR,cost=CostC)
  else
    model=e1071::svm(x=TrainData,y=TrainCls,scale=Scale,type=Method,kernel=Kernel,degree=PolynomialDegree,gamma=Gamma,coef0=CoefR,cost=CostC,nu=Nu)
  
  pred_train <-predict(model,TestData)
  
  pred_train=round(as.numeric(pred_train),0)
  

  return(list(TestCls=round(pred_train,0),Probabilities=as.numeric(pred_train),Model=model))
}