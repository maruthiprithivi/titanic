library("randomForest")
source("processData.r")

formatData = function(fileName)
{
    input = refineData(rawData(sub(".csv", "", fileName)))

    # drop unnecessary columns
    cols = c("lastName", "ticketHeader", "cabinLetter", "cabinNumber", "cabin")
    input = input[!(names(input) %in% cols)]

    return(input)
}

recoData = function(fileName)
{
    input = read.csv(fileName)
    input[names(input) %in% "survived"] = factor(input$survived)
    input$child = factor(input$child)
    input$parent = factor(input$parent)
    input$pclass = factor(input$pclass, ordered = T)
    input$cabinLetter = ordered(input$cabinLetter)

    # drop unnecessary columns
    cols = c("ticketHeader", "cabinNumber", "cabinLetter", "child", "parent")
    input = input[!(names(input) %in% cols)]

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

# train = formatData("train.csv")
# test = formatData("test.csv")
train = recoData("data/recoTrain.csv")
test = recoData("data/recoTest.csv")

# X = rfImpute(survived ~ ., train) # fill missing values
# RF = randomForest(survived ~ ., X, ntree = 8000)
# plot(RF$err.rate[, "OOB"])
# savePlot("figures/OOB error vs Index", "png")
