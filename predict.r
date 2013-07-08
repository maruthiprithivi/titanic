## Train a random forest with early stopping.
## Write out its prediction on a test set.

source("fillNA.r")

classifier = function(X, bestT = 2000, best.d = 4)
{
    seed = .Random.seed[3] # between -2^31 and 2^31
    set.seed(seed)
    RF0 = randomForest(survived ~ ., X, ntree = bestT, mtry = best.d)
    i.min = which.min(RF0$err.rate[, "OOB"])
    set.seed(seed)
    RFm = randomForest(survived ~ ., X, ntree = i.min, mtry = best.d)

    # if (which.min(RFm$err.rate[, "OOB"]) != i.min) # sanity check
        # print("Minimum index has changed!")

    # plot(RF0$err.rate[, "OOB"])
    # lines(RFm$err.rate[, "OOB"], col = "red")

    # return(RFm)
}

# fill test missing values by assuming all test examples belong to one class
# find prob(survived = 0) and prob(survived = 1) for each class assumption
# sum probabilities over the class assumptions to give a weight for
# survived = 0 and for survived = 1
# prediction is given by the value of survived with the max weight
predictRF = function(train, test, ...)
{
    X = rfImpute(survived ~ ., train)
    RF = classifier(X, ...)
    p0 = predict(RF, fillByAssumption(X, test, 0), "prob")
    p1 = predict(RF, fillByAssumption(X, test, 1), "prob")

    as.integer(levels(X$survived)[max.col(p0 + p1)])
}

p = predictRF(train, test)
write.csv(data.frame(PassengerId = 1:nrow(test), Survived = p),
          "prediction.csv", row.names = F)

# create a matrix of predictions to look at error prediction
# x is the training set, y is the test set and n is the number of predictions
preMat <- function(x, y, n)
{
    pM = matrix(nrow = n, ncol = nrow(y))
    for (i in 1 : n)
    {
        xRf = classifier(x)
        p = predict(xRf, y)
        pM[i, ] = as.integer(levels(p))[p]
    }

    return(pM)

}
