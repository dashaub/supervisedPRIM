# ensemblePRIM <- function(x, y, newdata, ...) {
#   # Fit a supervisedPRIM model with a positive and a negative threshold
#   primPos <- supervisedPRIM(x = x,
#                             y = y,
#                             threshold.type = 1L,
#                             ...)
#   primNeg <- supervisedPRIM(x = x,
#                             y = y,
#                             threshold.type = -1L,
#                             ...)
#   # Obtain the class and probability predictions on the new dataset
#   primPosClass <- predict(primPos, newdata = newdata)
#   primPosProb <-
#     predict(primPos, newdata = newdata, classProb = TRUE)
#   primNegClass <- predict(primNeg, newdata = newdata)
#   primNegProb <-
#     predict(primNeg, newdata = newdata, classProb = TRUE)
#   
#   # Start building the final classifier
#   # If the two class predictions from the positive and negative threshold agree,
#   # use this classifier. If they disagree, use the higher confidence probability
#   # from the positive or negative threshold.
#   agreeClassPred <- which(primPosClass == primNegClass)
#   disagreeClassPred <- -agreeClassPred
#   finalClass <- rep(0, nrow(newdata))
#   finalClass[agreeClassPred] <- primPosClass[agreeClassPred]
#   
#   # Now break the disagreements
#   posConf <- abs(primPosProb - 0.5)
#   negConf <- abs(1 - primNegProb - 0.5)
#   usePos <- ifelse(posConf >= negConf, TRUE, FALSE)
#   tiePred <- rep(0, length(usePos))
#   tiePred[usePos] <- primPosClass[-agreeClassPred][usePos]
#   tiePred[!usePos] <- primNegClass[-agreeClassPred][!usePos]
#   
#   # Finally combine these with the agree predictions
#   finalClass[-agreeClassPred] <- tiePred
# }
