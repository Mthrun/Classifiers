KNNclassifierDistance = function(K=1,TrainData,TrainCls,TestData=NULL,ShowObs=F, method = "euclidean",p = 2){
  # [KNNTestCls,NearestInd ] = KNNclassifier(k,TrainData,TrainCls,TestData,Verbose);
  #  k-nearest neighbor clssifier
  # INPUT
  # K                    the number of neighbor to use
  # TrainData            matrix [n,d] containing classified data
  # TrainCls             vector [1:n] containing the classes of TrainData
  # TestData             matrix [m,d] containing unclassified data
  # OPTIONAL
  # ShowObs             logical, when it's ture, the funtion will output the imformation of training set
  #                      cases.
  # method              'euclidean','sqEuclidean','binary','cityblock', 'maximum','canberra','cosine','chebychev','jaccard', 'mahalanobis','minkowski','manhattan','braycur','cosine'
  # p                   The power of the Minkowski distance.
  # OUTPUT
  # value:               result of classifications of test set will be returned. (When TstX is NULL, the function will automatically
  #                      consider the user is trying to test the knn algorithm. Hence, a test result table and accuracy
  #                     report will be shown on the R-console.)
  #KNNTestCls			vector [1:m], a KNN classification of TestData
  #Author: MT 17/08 umgeschrieben aus knngarden paket, copyright also Xinmiao Wang
#Description
  # k-nearest neighbour classification of versatile Distance version for test set from training set. For
  # each row of the test set, the k nearest (in multiple distances) training set vectors are found, and the
  # classification is decided by majority vote. This function allows you measure the distance bewteen
  # vectors by six different means. K Threshold Value Check and Same K_i Problem Dealing are also
  # been considered.
#Details:
  # K Threshold Value is stipulated to be less than the minimum size of the class in training set, or a
  # warning will be shown.
  # Sometimes a case may get same "ballot" from class A and class B (even C, D, ...), this time a
  # weighted voting process will be activated. The weight is based on the actual distance calculated
  # between the test case and K cases in neighbor A and B. The test case belongs to the class with less
  # total distance.
  # The multiple distances are implemented by transfering the function dist(). For the convenience of
  # users, we quote the details of function "dist()" and show them here.
  # Available distance measures are :
  #   euclidean: Usual square distance between the two vectors (2 norm).
  # maximum: Maximum distance between two components of x and y (supremum norm)
  # manhattan: Absolute distance between the two vectors (1 norm).
  # canberra: sum(abs(Xi-Yi)/abs(Xi+Yi)) Terms with zero numerator and denominator are omitted
  # from the sum and treated as if the values were missing.
  # This is intended for non-negative values (e.g. counts): taking the absolute value of the denominator
  # is a 1998 R modification to avoid negative distances.
  # binary: (aka asymmetric binary): The vectors are regarded as binary bits, so non-zero elements are
  # "on" and zero elements are "off". The distance is the proportion of bits in which only one is on
  # amongst those in which at least one is on.
  # minkowski: The p norm, the pth root of the sum of the pth powers of the differences of the components.
  # Missing values are allowed, and are excluded from all computations involving the rows within
  # which they occur. Further, when Inf values are involved, all pairs of values are excluded when
  # their contribution to the distance gave NaN or NA. If some columns are excluded in calculating a
  # Euclidean, Manhattan, Canberra or Minkowski distance, the sum is scaled up proportionally to the
  # number of columns used. If all pairs are excluded when calculating a particular distance, the value
  # is NA.
  TrainCls = as.factor(TrainCls)
  TrnG = as.numeric(TrainCls)
  CodeMeaning = data.frame(TrnG, TrainCls)
  
  TK = sort(as.matrix(table(TrnG)), decreasing = F)
  if (K > TK[1])
  {
    stop(
      c(
        "
        NOTES:
        sorry, the value of K ",
        "(K=",
        K,
        ") ",
        "you have selected is bigger than the capacity of one class in your training data set",
        "(",
        "the capacity is ",
        TK[1],
        ")",
        ",",
        "please choose a less value for K"
      )
      )
  }
  
  if (is.null(TestData) == T)
  {
    IsTst = 1
    TestData <- as.matrix(TrainData)
  } else
  {
    IsTst = 0
  }
  
  if (is.matrix(TestData) == F)
  {
    TestData <- as.matrix(TestData)
  }
  
  TrainData <- as.matrix(TrainData)
  ElmTrnG = union(TrnG, TrnG)
  LevTrnG = length(ElmTrnG)
  TrnTotal = cbind(TrnG, TrainData)
  
  NTestData = nrow(TestData)
  NTrnTotal = nrow(TrnTotal)
  
  VoteResult = NULL
  VoteResultList = NULL
  
  #Anpassung MT 03/2019
  Total = rbind(TestData, TrainData)
  nt=nrow(TestData) #cases
  TotalMatrix=DistanceMatrix(
      Total, method = method , dim = p
    )
    n=nrow(TotalMatrix)

  #for (i in 1:nrow(TestData))
  for (i in 1:nt)
  {
    RankBoardI <- NULL
    RankBoardIJ <- NULL
    

    #Total = rbind(TestData[i, ], TrainData)
    # if(is.null(DistanceMatrix))
    #RankBoardI = as.matrix(as.dist(DistanceMatrix(
    #  Total, method = method , dim = p
    #))[1:nrow(TrainData)])
    #Anpassung MT 03/2019
    RankBoardI = as.matrix(as.dist(
      TotalMatrix[c(i,(nt+1):n),c(i,(nt+1):n)]
    
    )[1:nrow(TrainData)])
    
    # else
    #   RankBoardI=as.matrix(as.dist(DistanceMatrix)[1:nrow(TrainData)])
    
    RankBoardIJ = cbind(TrnG, RankBoardI)
    
    VoteAndWeight = RankBoardIJ[sort(RankBoardIJ[, 2], index.return = T)$ix[1:K], 1:2]
    TempVote4TestDataI = RankBoardIJ[sort(RankBoardIJ[, 2], index.return =
                                            T)$ix[1:K], 1]
    ElmVote = union(TempVote4TestDataI, TempVote4TestDataI)
    
    CountVote = as.matrix(sort(table(TempVote4TestDataI), decreasing = T))
    TempWinner = as.numeric(rownames(CountVote))
    
    if (length(CountVote) == 1 | K == 1)
    {
      Winner = TempWinner[1]
      TestDataIBelong = union(CodeMeaning$TrainCls[which(CodeMeaning$TrnG ==
                                                           Winner)],
                              CodeMeaning$TrainCls[which(CodeMeaning$TrnG ==
                                                           Winner)])
      VoteResultNode = data.frame(TestDataIBelong)
      VoteResultList = rbind(VoteResultList, VoteResultNode)
      
    } else
    {
      NumOfTie = CountVote[1]
      FinalList = NULL
      
      j = 1
      TempWeight = sum(VoteAndWeight[which(VoteAndWeight[, 1] == TempWinner[j]), 2])
      FinalList = data.frame(TempWinner[j], TempWeight)
      while (CountVote[j] == CountVote[j + 1] & j < length(CountVote))
      {
        TempWeight = sum(VoteAndWeight[which(VoteAndWeight[, 1] == TempWinner[j +
                                                                                1]), 2])
        FinalListNode = c(TempWinner[j + 1], TempWeight)
        FinalList = rbind(FinalList, FinalListNode)
        j = j + 1
      }
      
      FinalList = FinalList[sort(FinalList$TempWeight, index.return = T)$ix[1], ]
      TestDataIBelong = union(CodeMeaning$TrainCls[which(CodeMeaning$TrnG ==
                                                           FinalList[1, 1])],
                              CodeMeaning$TrainCls[which(CodeMeaning$TrnG ==
                                                           FinalList[1, 1])])
      VoteResultNode = data.frame(TestDataIBelong)
      VoteResultList = rbind(VoteResultList, VoteResultNode)
      
    }
    
  }
  
  if (IsTst == 1)
  {
    CheckT = as.matrix(table(data.frame(VoteResultList, TrainCls)))
    AccuStat = 1 - sum(CheckT - diag(diag(CheckT))) / length(TrnG)
    print(CheckT)
    cat(
      "the classification accuracy of this algorithm on this training dataset is: ",
      AccuStat * 100,
      "%",
      "\n\n\n"
    )
    
  }
  
  if (IsTst == 1 & ShowObs == F) {
    result = data.frame(VoteResultList, TrainCls)
  } else
  {
    if (IsTst == 1 & ShowObs == T) {
      result = data.frame(TestData, VoteResultList, TrainCls)
    } else
    {
      if (ShowObs == F) {
        result = data.frame(VoteResultList)
      } else{
        result = data.frame(TestData, VoteResultList)
      }
    }
  }
  return(list(
    result = result,
    KNNTestCls = as.vector(VoteResultList$TestDataIBelong)
  ))
}
