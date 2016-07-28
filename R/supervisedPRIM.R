supervisedPRIM <- function(x, y, peel.alpha = 0.05, paste.alpha = 0.01,
                           mass.min = 0.05, threshold.type = 1){
   
   result <- prim::prim.box(x, y, peel.alpha = peel.alpha, paste.alpha = paste.alpha,
                  mass.min = mass.min, threshold.type = threshold.type)
   class(result) <- c("supervisedPRIM", "prim")
   return(result)
}

predict.supervisedPRIM <- function(x, newdata, classProb = FALSE){
   # Determine if the threshold is upper or lower
   positive <- x$ind[1]
   
   # Determine label for observations outside of the boxes
   other <- x$num.class
   
   # Fit the prim box on the new data
   class(x) <- "prim"
   
   # Calculuate class probabilities using the same methodology of CART
   if(classProb){
      return(NULL);
   }
   primPred <- predict(x, newdata = newdata)
   # Determine the boxes to use
   classPred <- ifelse(primPred != other, positive, 1 - positive)
   return(classPred)
}
