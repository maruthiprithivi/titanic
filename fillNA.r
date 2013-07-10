## Methods to fill missing values (NA) in data sets.

# fill test missing values by regression
fillByRegression = function(X, test, column)
{
	regRF = randomForest(X[, column] ~ ., X, na.action = na.omit)
	na.i = which(is.na(test[column]))
	rows = test[na.i, ]
	rows[column] = NULL
	test[na.i, column] = predict(regRF, rows)
}

# fill test missing values by assuming one class and
# combining with training set before using rfImpute
fillByAssumption = function(X, test, value)
{
    test$survived = factor(value, c(0, 1))
    test = rfImpute(survived ~ ., rbind(test, X))[1:nrow(test), ]
    test$survived = NULL

    return(test)
}

# replace missing age values by half of (mean + median)
ageNA <- function(x)
{
	a = "age"
	t = "title"
	ageMissing = is.na(x[a])
	for (y in c("Master", "Miss", "Mr", "Mrs"))
		x[ageMissing & y == x[t], ][a] = 0.5 * (colMeans(x[y == x[t], ][a], na.rm = T) + median(x[y == x[t],a], na.rm = T))
	if (sum(ageMissing & "Dr" == x[t]) > 0)
		x[ageMissing & "Dr" == x[t], ][a] = 0.5 * (colMeans(x["Dr" == x[t], ][a], na.rm = T) + median(x["Dr" == x[t],a], na.rm = T))

	return(x)

}

# fill missing test values by rfImpute
# X0 = na.roughfix(test)
# y0 = predict(forest, X0)

# while (T)
# {
	# X1 = rfImpute(test, y0, iter = 1)
	# y1 = predict(forest, X1)

	# if (sum(y1 != y0) == 0) break

	# y0 = y1
# }
