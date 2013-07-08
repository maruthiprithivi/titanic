## Functions to read and process data.

rawData = function(fileName)
{
    if ((fileName %in% c("train", "test")) == 0)
        stop("invalid file name")

    path = file.path(getwd(), paste(fileName, ".csv", sep = ""))
    x = read.csv(path, stringsAsFactors = F)

    # split name into 3 columns and add them to the data frame as lastName, title and firstName
    nameSplit = sapply(x["name"], function(y) {strsplit(y, "[,.] | [ ]")})
    x["lastName"] = sapply(nameSplit, function(y) {y[1]})
    x["title"] = sapply(nameSplit, function(y) {y[2]})
    x["firstName"] = sapply(nameSplit, function(y) {y[3]})

    # split ticket into 2 columns and add them to the data frame as ticket1 (alpha-numeric) and ticket2
    ticketSplit = sapply(x["ticket"],
        function(y) {strsplit(gsub("[./]", "", y), " ")})
    x["ticket1"] = sapply(ticketSplit,
        function(y)
        {
            if (length(y) > 2) paste(y[1], y[2], sep = "") # STON/O 2.
            else if (substr(y[1], 1, 1) %in% LETTERS) y[1]
            else ""
        })
    x["ticket2"] = sapply(ticketSplit,
        function(y)
        {
            if (length(y) > 2) as.integer(y[3]) # STON/O 2. 3101294
            else if (length(y) > 1) as.integer(y[2])
            else if (substr(y[1], 1, 1) %in% LETTERS) NA # LINE
            else as.integer(y[1])
        })

    # split cabin into 2 columns and add them to the data frame as cabinLetter and cabinNumber
    cabinSplit = sapply(x["cabin"], function(y) {strsplit(y, " ")})
    x["cabinLetter"] = sapply(cabinSplit,
        function(y)
        {
            if (length(y) > 1) substring(y[2], 1, 1) # C23 C25 C27 or F G73
            else if (length(y) == 1) substring(y[1], 1, 1)
            else ""
        })
    x["cabinNumber"] = sapply(cabinSplit,
        function(y)
        {
            if (length(y) > 1) # C23 C25 C27 or F G73
                as.integer(substring(y[2], 2, 4))
            else if (length(y) == 1) # C85 or C123 or G6
                as.integer(substring(y[1], 2, 4))
            else NA # D
        })

    # replace all "" entries for every columns with NA
    for (i in 1:ncol(x)) x[x[, i] %in% "", i] = NA

    # factor response column "survived" in training data set
    x[names(x) %in% "survived"] = factor(x$survived)

    return(x)
}

refineData = function(x)
{
    # combine some titles together
    x$title["Mlle" == x$title] = "Miss"
    x$title["Ms"   == x$title] = "Miss"
    x$title["Mme"  == x$title] = "Mrs"

    ranks = c("Capt", "Col", "Major")
    x$title[x$title %in% ranks] = "Offr"

    royal = c("Don", "Dona", "Jonkheer", "Lady", "Sir", "the Countess")
    x$title[x$title %in% royal] = "Royal"

    # change unrealistic cabin letter to NA
    x$cabinLetter["T" == x$cabinLetter] = NA

    # turn the columns below into factors
    for (y in c("pclass", "sex", "embarked", "title", "ticket1", "cabinLetter"))
        x[y] = factor(x[ , y])

    # add order for these factors
    for (y in c("pclass", "cabinLetter"))
        x[y] = ordered(x[ , y])

    # remove unnecessary columns
    x = x[!(names(x) %in% c("name", "ticket", "firstName"))]

    return(x)
}

# for (y in c("Master", "Miss", "Mr", "Mrs"))
#  x[is.na(x["age"]) & y == x["title"], ]["age"] = 0.5 * (mean(x[y == x["title"], ]["age"], na.rm = T) + median(x[y == x["title"], ]["age"], na.rm = T))
# levels(test$title) = c(levels(test$title), "Capt", "Don", "Jonkheer", "Lady", "Major", "Mme", "Sir", "the Countess")
# c(10, 50, 292, 308, 547, 701, 782, 831, 856) Mrs
# c(310, 557, 600) Lady and Sir ticket1
# c(258, 505, 760) Countess
# c(53, 597, 646, 682, 721, 849) Rev Harper
# y = vector("list")
# for (i in 1 : 10) y[[i]] = randomForest(survived ~ ., trainImpute, ntree = 200, mtry = 3)
