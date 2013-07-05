library("randomForest")
source("processData.r")

formatData = function(fileName)
{
	input = refineData(rawData(sub(".csv", "", fileName)))

    # drop unnecessary columns
    input$lastName = NULL
    # input$title = NULL
    input$ticket1 = NULL
    # input$ticket2 = NULL
    # input$cabinLetter = NULL
    input$cabinNumber = NULL
    input$cabin = NULL

	return(input)
}

train = formatData("train.csv")
test = formatData("test.csv")

X = rfImpute(survived ~ ., train) # fill missing values
RF = randomForest(survived ~ ., X, ntree = 8000)
plot(RF$err.rate[, "OOB"])
savePlot("figures/OOB error vs Index", "png")

# convergence curve
# X1 = Xfill[1:594, ]
# y1 = y[1:594]
# X2 = Xfill[595:891, ]
# y2 = y[595:891]
# forest = randomForest(X1, y1, X2, y2, ntree = 8000)
# plot($err.rate[, 'OOB'])

# missRF = randomForest(survived ~ ., train, ntree = 100, mtry = 3, na.action = na.omit)
# fillRF = randomForest(survived ~ ., X, ntree = 100, mtry = 3)

# p1 = predict(missRF, X0)
# p2 = predict(fillRF, X0)

# write(as.vector(p1), "prediction1.csv", 1)
# write(as.vector(p2), "prediction2.csv", 1)
