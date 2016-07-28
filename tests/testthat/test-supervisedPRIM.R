if(require(testthat)){
   context("Building supervised PRIM models")
   # Predict if the species is "setosa" or not on the iris dataset
   # Prepare a train and test set
   data(iris)
   irisData <- iris
   set.seed(552)
   trainIndex <- sample(1:nrow(irisData), size = 120, replace = FALSE)
   irisData$Species <- ifelse(irisData$Species == "setosa", 1L, 0L)
   # Add some fake data so we don't get perfect separation
   irisData$fake1 <- rnorm(nrow(irisData))
   irisData$fake2 <- runif(nrow(irisData))
   irisData$fake3 <- rnorm(nrow(irisData))
   irisData$fake4 <- runif(nrow(irisData))
   training <- irisData[trainIndex, ]
   testing <- irisData[-trainIndex, ]
   ytrain <- training$Species
   training$species <- NULL
   ytest <- testing$Species
   testing$species <- NULL
   
   # Perform the tests
   test_that("Testing the supervisedPRIM() and predict.supervisedPRIM()", {
      # Fit the positive and negative thresholds
      expect_error(mp <- supervisedPRIM(x = training, y = ytrain, threshold.type = 1), NA)
      expect_error(mn <- supervisedPRIM(x = training, y = ytrain, threshold.type = -1), NA)
      # Make class predictions
      expect_error(cpp <- predict(mp, newdata = testing), NA)
      expect_error(cpn <- predict(mp, newdata = testing), NA)
      # Verify the integrity of the class predictions
      expect_true(length(cpp) == length(ytest))
      expect_true(length(cpn) == length(ytest))
      expect_true(class(cpp) == "integer")
      expect_true(class(cpn) == "integer")
      expect_true(all(cpp %in% c(0L, 1L)))
      expect_true(all(cpn %in% c(0L, 1L)))
      
      # Make probability predictions
      expect_error(pp <- predict(mp, newdata = testing, classProb = TRUE), NA)
      expect_error(pn <- predict(mp, newdata = testing, classProb = TRUE), NA)
      # Verify the integrity of the probability predictions
      expect_true(length(pp) == length(ytest))
      expect_true(length(pn) == length(ytest))
      expect_true(class(pp) == "numeric")
      expect_true(class(pn) == "numeric")
      expect_true(min(pp) >= 0)
      expect_true(max(pp) <= 1)
      expect_true(min(pn) >= 0)
      expect_true(max(pn) <= 1)
   })
}
