#'
#'@import prim
#'
#'@export
#'
#'@title Fit PRIM model to a labeled dataset
#'@description perform supervised classification using Patient Rules Induction Method (PRIM)
#'@param x matrix of data values
#'@param y vector of response values
#'@param peel.alpha peeling quantile tuning parameter
#'@param paste.alpha pasting quantile tuning parameter
#'@param mass.min minimum mass tuning parameter
#'@param threshold.type threshold direction indicator: 1 = ">= threshold",
#'-1 = "<= threshold"
#'@param ... additional arguments to pass to \code{\link{prim.box}}
#'@details Fit 
#'@return an object of class \code{supervisedPRIM}. See additional details
#'in \link{prim.box}
#'
supervisedPRIM <- function(x, y, peel.alpha = 0.05, paste.alpha = 0.01,
                           mass.min = 0.05, threshold.type = 1, ...){
   # Ensure the input is all numeric data
   if(!all(sapply(x, class) %in% c("numeric", "integer"))){
      stop("All columns in x must be numeric")
   }
   
   # Ensure the data are binomial
   uniquey <- unique(y)
   if(length(uniquey) != 2){
      stop("The y data must be labeled binomial 0/1 data.")
   }
   if(!all(unique(y) %in% c(0, 1))){
      
   }
   
   result <- prim::prim.box(x, y, peel.alpha = peel.alpha, paste.alpha = paste.alpha,
                  mass.min = mass.min, threshold.type = threshold.type, ...)
   class(result) <- c("supervisedPRIM", "prim")
   
   
   return(result)
}

#'
#'@export
#'@param x A trained model of class \code{supervisedPRIM} returned by \link{sueprvisedPRIM}
#'@param newdata The new data on which to create predictions
#'@param classProb Should the function return the estimated class
#'probabilities instead of the predicted class?
#'
#'
predict.supervisedPRIM <- function(x, newdata, classProb = FALSE){
   # Determine if the threshold is upper or lower
   positive <- x$ind[1]
   
   # Determine label for observations outside of the boxes
   other <- x$num.class
   
   # Obtain the probabilities inside of each box
   boxProbs <- sapply(x$y, FUN = mean)
   
   # Fit the prim box on the new data
   class(x) <- "prim"
   primPred <- predict(x, newdata = newdata)
   
   # Calculuate class probabilities using the same methodology of CART
   if(classProb){
      return(boxProbs[primPred])
   }
   
   # Determine the boxes to use
   inBox <- primPred != other
   #classPred <- ifelse(primPred != other, positive, 1 - positive)
   if(x$ind[1] == 1){
      classPred <- ifelse(inBox, 1L, 0L)
      return(classPred)
   }
   classPred <- ifelse(inBox, 0L, 1L)
   return(classPred)
   
   
   # To ensure the estimate for the negative class is unbiased,
   # use the training sample proportion to randomly assign
#    if(biasAdj){
#       
#    }
   return(classPred)
}
