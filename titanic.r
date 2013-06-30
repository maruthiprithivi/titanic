library("randomForest")
source("change.r")

formatData = function(fileName)
{
	input = change(sub(".csv", "", fileName))

    # drop unnecessary columns
    input$lastName = NULL
    input$title = NULL
    input$firstName = NULL
    input$ticket1 = NULL
    input$ticket2 = NULL
    input$cabinLetter = NULL
    input$cabinNumber = NULL

	return(input)
}

trainRF = function(X, d, n = 200)
{
    # Xfill = rfImpute(X, y, ntree = n, mtry = d)
    # Xfill$y = NULL
    RF = randomForest(X$survived ~ ., X, ntree = n, mtry = d, na.action = na.omit)
    CM = RF$confusion[, 1:2]
    err = 1 - sum(diag(CM)) / sum(CM)
}

# fill test missing values by regression
fill = function(X, test, column)
{
	regRF = randomForest(X[, column] ~ ., X, na.action = na.omit)
	na.i = which(is.na(test[column]))
	rows = test[na.i, ]
	rows[column] = NULL
	test[na.i, column] = predict(regRF, rows)
}

train = formatData("train.csv")
test = formatData("test.csv")

X = rfImpute(survived ~ ., train) # fill missing values

# missRF = randomForest(train$survived ~ ., train, ntree = 100, mtry = 3, na.action = na.omit)
# fillRF = randomForest(X, train$survived, ntree = 100, mtry = 3)

# convergence curve
# X1 = Xfill[1:594, ]
# y1 = y[1:594]
# X2 = Xfill[595:891, ]
# y2 = y[595:891]
# forest = randomForest(X1, y1, X2, y2, ntree = 8000)
# plot($err.rate[, 'OOB'])

# validation curve
# plot(sapply(1:ncol(X), function(d) { trainRF(train, d) }), type = 'l')

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

# p1 = predict(missRF, X0)
# p2 = predict(fillRF, X0)

# write(as.vector(p1), "prediction1.csv", 1)
# write(as.vector(p2), "prediction2.csv", 1)