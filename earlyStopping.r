## Script to study early stopping by running random forest many times.
## Plot histograms for minimum OOB error, index corresponding to minimum,
## and validation curve for d (number of variables to try at each split).

stopEarly = function(X, d, tree = 2000, iter = 100)
{
    err = matrix(nrow = iter, ncol = 2, dimnames = list(NULL, c("index", "value")))

    for (i in 1:iter)
    {
        RF = randomForest(survived ~ ., X, ntree = tree, mtry = d)

        err[i, "index"] = which.min(RF$err.rate[, "OOB"])
        err[i, "value"] = min(RF$err.rate[, "OOB"])
    }

    return(err)
}

plotError = function(err, name)
{
    nVar = length(err)
    nSqr = sqrt(nVar)
    nCol = ceiling(nSqr)
    nRow = round(nSqr)

    windows(nCol * 300, nRow *300)
    par(mfrow = c(nRow, nCol))

    for (d in 1:nVar)
    {
        x = err[[d]][, name]
        hist(x, main = paste("d =", d), xlab = name)
        abline(v = median(x), col = "red")
        abline(v = mean(x), col = "blue")
    }

    savePlot(paste("figures/min error", name), "png")
}

validate = function(train, errFile = "")
{
    if ("" == errFile)
    {
        set.seed(0)
        X = rfImpute(survived ~ ., train) # fill missing values
        err = lapply(2:ncol(X) - 1, function(d) { stopEarly(X, d) })
        save(err, file = "data/OOBerrors.RData")
    }

    load("data/OOBerrors.RData")
    plotError(err, "index")
    plotError(err, "value")
    windows()
    boxplot(lapply(1:length(err), function(d) { err[[d]][, "value"] }), xlab = "d")
    savePlot("figures/validation curve", "png")
}