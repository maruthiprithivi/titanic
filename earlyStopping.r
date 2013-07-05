## Script to study early stopping by running random forest many times.
## Plot histograms for minimum OOB error, index corresponding to minimum,
## and validation curve for d (number of variables to try at each split).

stopEarly = function(X, d, n = 100)
{
    err = matrix(nrow = n, ncol = 2, dimnames = list(NULL, c("index", "value")))

    for (i in 1:n)
    {
        RF = randomForest(survived ~ ., X, ntree = 2000, mtry = d)

        err[i, "index"] = which.min(RF$err.rate[, "OOB"])
        err[i, "value"] = min(RF$err.rate[, "OOB"])
    }

    return(err)
}

plotError = function(err, name)
{
    windows()
    par(mfrow = c(3, 3))

    for (d in 1:length(err))
    {
        x = err[[d]][, name]
        hist(x, main = paste("d =", d), xlab = name)
        abline(v = median(x), col = "red")
        abline(v = mean(x), col = "blue")
    }

    savePlot(paste("figures/min error", name), "png")
}

early stopping
err = lapply(2:ncol(X) - 1, function(d) { stopEarly(X, d) })
save(err, file = "OOBerrors.RData")
# load("OOBerrors.RData")
plotError(err, "index")
plotError(err, "value")
windows()
boxplot(lapply(1:length(err), function(d) { err[[d]][, "value"] }), xlab = "d")
savePlot("figures/validation curve", "png")