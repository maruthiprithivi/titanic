## Methods to fill missing values (NA) in data sets.

# fill test missing values by regression
fill = function(X, test, column)
{
	regRF = randomForest(X[, column] ~ ., X, na.action = na.omit)
	na.i = which(is.na(test[column]))
	rows = test[na.i, ]
	rows[column] = NULL
	test[na.i, column] = predict(regRF, rows)
}

# replace missing age values
ageNA <- function(x)
{
  for (y in c("Master", "Miss", "Mr", "Mrs"))
    x[is.na(x["age"]) & y == x["title"], ]["age"] = 0.5 * (colMeans(x[y == x["title"], ]["age"], na.rm = T) + median(x[y == x["title"],"age"], na.rm = T))
  if (sum(is.na(x["age"]) & "Dr" == x["title"]) > 0)
    x[is.na(x["age"]) & "Dr" == x["title"], ]["age"] = 0.5 * (colMeans(x["Dr" == x["title"], ]["age"], na.rm = T) + median(x["Dr" == x["title"],"age"], na.rm = T))

  return(x)

}

# fill missing test values
# Xage = Xfill
# Xage$age = NULL
# ageRF = randomForest(Xage, Xfill$age)

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
