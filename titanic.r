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

# convergence curve
convergence = function(X)
{
    n = nrow(X)
    validSize = n / 3
    shuffle = sample.int(n)[1:validSize]
    X1 = X[-shuffle, 2:ncol(X)]
    X2 = X[ shuffle, 2:ncol(X)]
    y1 = X[-shuffle, 1]
    y2 = X[ shuffle, 1]
    RF = randomForest(X1, y1, X2, y2, ntree = 8000)
    plot(RF$err.rate[, 'OOB'], col = "red")
    lines(RF$test[["err.rate"]][, "Test"])
}

train = formatData("train.csv")
test = formatData("test.csv")

# X = rfImpute(survived ~ ., train) # fill missing values
# RF = randomForest(survived ~ ., X, ntree = 8000)
# plot(RF$err.rate[, "OOB"])
# savePlot("figures/OOB error vs Index", "png")
