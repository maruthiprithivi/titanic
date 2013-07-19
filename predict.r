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

# generate predictions to look at their statistics
predMatrix <- function(train, test, nPred)
{
    pM = matrix(nrow = nPred, ncol = nrow(test))

    for (i in 1 : nPred) pM[i, ] = predictRF(train, test)

    return(pM)
}

# compute row (margin = 1) or column (margin = 2) mode of a matrix x of integers
# to use for characters, remove as.integer
statsMode <- function(x, margin)
{
    apply(x, margin,
        function(y)
        {
            tab = table(y)
            as.integer(names(tab)[which.max(tab)])
        })

}

# compute mean of the difference of each prediction with the prediction mode
# x is a matrix of predictions
avgDifFromMode <- function(x)
{
    ref = statsMode(x, 2)
    avgDif = apply(x, 1, function(y) {mean(ref != y)})
    
    return(avgDif)

}

# study the uncertainty as a function of the accuracy
uncertaintyVsAccuracy = function(pred)
{
    x = vector() # means of accuracy
    y = vector() # std dev of accuracy

    for (i in 1:10)
    {
        n = ncol(pred)
        v = rep(0, n)
        v[sample(n, round(i / 10 * n))] = 1
        acc = apply(pred, 1, function(r) { mean(r == v) })
        x[i] = mean(acc)
        y[i] = sd(acc)
    }

    plot(x, y)
}

# randomly sample increasing sub-matrices of predictions from a matrix x of predictions 
# call the function avgDifFromMode on each of these sub-matrics
# store the average differences in a list
# progress is the increase size
avgDifSample <- function(x, progress)
{
    avgDifSamList = vector("list")
    rowMat = nrow(x)
    for (i in 1 : (rowMat / progress))
        avgDifSamList[[i]] = avgDifFromMode(x[sample(1 : rowMat, i * progress, replace = F), ])

    return(avgDifSamList)

}

# compute 3 vectors to gauge uncertainty of the list x created by the function avgDifSample
# the 1st vector is the means of the average differences
# the 2nd vector is the sd's of the average differences
# the 3rd vector is a number which is the sd of the means of the average differences
uncertainty <- function(x)
{
    valid = vector("list")
    valid[[1]] = sapply(x, mean)
    valid[[2]] = sapply(x, sd)
    valid[[3]] = sd(valid[[1]])

    return(valid)

}

# toy Monte Carlo study on the uncertainty in prediction
toyMC = function(input, validSize, nExpt)
{
    TV = trainAndValidSets(input, validSize)
    acc = vector()

    for (i in 1:nExpt)
    {
        X1 = rfImpute(survived ~ ., TV[["train"]])
        X2 = rfImpute(survived ~ ., TV[["valid"]])
        p = predict(classifier(X1), X2)
        acc[i] = mean(p == X2$survived)
    }

    return(acc)
}

# p = predictRF(train, test)
# write.csv(data.frame(PassengerId = 1:nrow(test), Survived = p),
          # "prediction.csv", row.names = F)
