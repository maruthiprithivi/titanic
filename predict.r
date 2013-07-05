## Train a random forest with early stopping.
## Write out its prediction on a test set.

classifier = function(bestT = 2000, best.d = 4)
{
    X = rfImpute(survived ~ ., train) # fill missing values
    seed = .Random.seed[3]
    set.seed(seed)
    RF0 = randomForest(survived ~ ., X, ntree = bestT, mtry = best.d)
    i.min = which.min(RF0$err.rate[, "OOB"])
    set.seed(seed)
    RFm = randomForest(survived ~ ., X, ntree = i.min, mtry = best.d)

    if (which.min(RFm$err.rate[, "OOB"]) != i.min) # sanity check
        print("Minimum index has changed!")

    plot(RF0$err.rate[, "OOB"])
    lines(RFm$err.rate[, "OOB"], col = "red")

    return(RFm)
}

# source("titanic.r")
# p = predict(classifier(), na.roughfix(test))
# write(as.vector(p), "prediction.csv", 1)