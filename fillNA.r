## Methods to fill missing values (NA) in data sets.

# fill missing age values using title only
fillAge = function(input)
{
    input$age = rfImpute(survived ~ age + title, input)$age

    return(input)
}

# fill missing fare values using pclass only
fillFare = function(input)
{
    input$fare = rfImpute(survived ~ fare + pclass, input)$fare

    return(input)
}

fillAll = function(input)
{
    input = fillAge(input) # fill age first to avoid using all variables
    input = rfImpute(survived ~ ., input) # fill remaining NA's
}

# fill test missing values by assuming one class and
# combining with training set before using rfImpute
fillByAssumption = function(X, test, value)
{
    test$survived = factor(value, c(0, 1))
    test = fillAll(rbind(test, X))[1:nrow(test), ]
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
