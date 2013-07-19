library("randomForest")
source("processData.r")

formatData = function(fileName)
{
    input = refineData(rawData(sub(".csv", "", fileName)))

    # drop unnecessary columns
    input$lastName = NULL
    # input$title = NULL
    input$ticketHeader = NULL
    # input$ticketNumber = NULL
    # input$cabinLetter = NULL
    input$cabinNumber = NULL
    input$cabin = NULL

    return(input)
}

# divide a data set to training and validation of given size
trainAndValidSets = function(X, validSize = nrow(X) / 3)
{
    shuffle = sample.int(nrow(X))[1:validSize]

    list(train = X[-shuffle, ], valid = X[shuffle, ])
}

# convergence curve
convergence = function(X)
{
    TV = trainAndValidSets(X, nrow(X) / 3)
    X1 = TV[["train"]][, 2:ncol(X)]
    X2 = TV[["valid"]][, 2:ncol(X)]
    y1 = TV[["train"]][, 1]
    y2 = TV[["valid"]][, 1]
    RF = randomForest(X1, y1, X2, y2, ntree = 8000)
    plot(RF$err.rate[, "OOB"], col = "red")
    lines(RF$test[["err.rate"]][, "Test"])
}

train = formatData("train.csv")
test = formatData("test.csv")

# X = rfImpute(survived ~ ., train) # fill missing values
# RF = randomForest(survived ~ ., X, ntree = 8000)
# plot(RF$err.rate[, "OOB"])
# savePlot("figures/OOB error vs Index", "png")
