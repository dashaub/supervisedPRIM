#'Fit PRIM model to a labeled dataset
#'@export
#'@import prim
#'@import stats
#'@description perform supervised classification using Patient Rules Induction Method (PRIM)
#'@param x matrix of data values
#'@param y binary vector of 0/1 response values
#'@param peel.alpha peeling quantile tuning parameter
#'@param paste.alpha pasting quantile tuning parameter
#'@param mass.min minimum mass tuning parameter
#'@param threshold.type threshold direction indicator: 1 = ">= threshold",
#'-1 = "<= threshold"
#'@param ... additional arguments to pass to \code{\link[prim]{prim.box}}
#'@details Fit 
#'@return an object of class \code{supervisedPRIM}. See additional details
#'in \link[prim]{prim.box}
#'@author David Shaub
#'@examples
#'# Train a model to determine if a flower is setosa
#'data(iris)
#'yData <- factor(ifelse(iris$Species == "setosa", "setosa", "other"), levels = c("setosa", "other"))
#'xData <- iris
#'xData$Species <- NULL
#'primModel <- supervisedPRIM(x = xData, y = yData)
supervisedPRIM <- function(x, y, peel.alpha = 0.05, paste.alpha = 0.01,
                           mass.min = 0.05, threshold.type = 1, ...){
  # Ensure the input is all numeric data
  if(!all(sapply(x, class) %in% c("numeric", "integer"))){ 
    stop("All columns in x must be numeric")
  }
  
  # Convert factor to binary
  lev <- levels(y)
  if(length(lev) != 2){
    stop("Only two-class classification is supported.")
  } else{
   y <- ifelse(y == lev[1], 1L, 0L) 
  }
  
  # Ensure the data are binary
  uniquey <- unique(y)
  if(length(uniquey) != 2){
    stop("The y data must be labeled binary 0/1 data.")
  }
  if(!all(unique(y) %in% c(0, 1))){
    
  }
  
  result <- prim.box(x, y, peel.alpha = peel.alpha, paste.alpha = paste.alpha,
                     mass.min = mass.min, threshold.type = threshold.type, ...)
  # Save levels
  result$levels <- lev
  class(result) <- c("supervisedPRIM", "prim")
  
  
  return(result)
}

#'Model Predictions
#'@description Perform prediction on a trained \code{supervisedPRIM} model. Output
#'to either predicted class or positive class probability is supported.
#'@param object A trained model of class \code{supervisedPRIM} returned by \link[supervisedPRIM]{supervisedPRIM}
#'@param newdata The new data on which to create predictions
#'@param classProb Should the function return the estimated class
#'@param ... additional arguments (ignored)
#'probabilities instead of the predicted class?
#'@export
#'@author David Shaub
#'@examples
#'# Train a model to determine if a flower is setosa
#'data(iris)
#'yData <- factor(ifelse(iris$Species == "setosa", "setosa", "other"), levels = c("setosa", "other"))
#'xData <- iris
#'xData$Species <- NULL
#'primModel <- supervisedPRIM(x = xData, y = yData)
#'# Predict on the original dataset
#'predictions <- predict(primModel, newdata = xData)
predict.supervisedPRIM <- function(object, newdata, classProb = FALSE, ...){
  # Determine if the threshold is upper or lower
  positive <- object$ind[1]
  
  # Determine label for observations outside of the boxes
  other <- object$num.class
  
  # Fit the prim box on the new data
  class(object) <- "prim"
  primPred <- predict(object = object, newdata = newdata)
  
  # Determine the boxes to use
  if(0 %in% primPred){
    # The newdata matrix contains observations outside the range of the train set
    other <- c(0, other)
  }
  inBox <- !(primPred %in% other)
  
  # Calculuate class probabilities using the same methodology of CART
  if(classProb){
    # Obtain the probabilities inside of each box
    boxProbs <- sapply(object$y, FUN = mean)
    
    # Return class 0 in primPred indicates outside of training region
    # Use mean imputation for these
    boxProbs <- c(boxProbs, mean(unlist(object$y)))
    return(boxProbs[primPred + 1])
  }
  
  
  #classPred <- ifelse(primPred != other, positive, 1 - positive)
  
  shift <- ifelse(other[1] == "0", 1L, 0L)
  if(!shift){
    #firstSlot <- ifelse(positive == 1, 1, 0)
    #secondSlot <- ifelse(positive == -1, 0, 1)
    #classPred <- ifelse(inBox, 1L, 0L)
    classPred <- ifelse(positive, inBox, as.numeric(!inBox))
    classPred <- factor(ifelse(classPred == 1L, object$levels[1], object$levels[2]), levels = object$levels)
    return(classPred)
  }
  classPred <- ifelse(inBox, 0L, 1L)
  #classPred <- factor(ifelse(predict(modelFit, newdata, classProb = FALSE) == 1, modelFit$levels[1], modelFit$levels[2]), levels = modelFit$levels)
  classPred <- factor(ifelse(classPred == 1L, object$levels[1], object$levels[2]), levels = object$levels)
  return(classPred)
  
  
  # To ensure the estimate for the negative class is unbiased,
  # use the training sample proportion to randomly assign
  #    if(biasAdj){
  #       
  #    }
  return(classPred)
}
