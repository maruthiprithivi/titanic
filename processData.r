## Functions to read and process data.

rawData = function(fileName)
{
    if ((fileName %in% c("train", "test")) == 0)
        stop("invalid file name")

    path = file.path(getwd(), paste(fileName, ".csv", sep = ""))
    x = read.csv(path, stringsAsFactors = F)

    # split name into lastName, title and firstName which are added to data frame as columns
    nameSplit = sapply(x["name"], function(y) {strsplit(y, "[,.] | [ ]")})
    x["lastName"] = sapply(nameSplit, function(y) {y[1]})
    x["title"] = sapply(nameSplit, function(y) {y[2]})
    x["firstName"] = sapply(nameSplit, function(y) {y[3]})

    # split ticket into ticketHeader (alpha-numeric) and ticketNumber which are added to data frame as columns
    ticketSplit = sapply(x["ticket"], function(y) {strsplit(gsub("[./]", "", y), " ")})
    x["ticketHeader"] = sapply(ticketSplit,
        function(y)
        {
            if (length(y) > 2) paste(y[1], y[2], sep = "") # STON/O 2.
            else if (substr(y[1], 1, 1) %in% LETTERS) y[1]
            else ""
        })
    x["ticketNumber"] = sapply(ticketSplit,
        function(y)
        {
            if (length(y) > 2) as.integer(y[3]) # STON/O 2. 3101294
            else if (length(y) > 1) as.integer(y[2])
            else if (substr(y[1], 1, 1) %in% LETTERS) NA # LINE
            else as.integer(y[1])
        })

    # split cabin into cabinLetter and cabinNumber which are added to data frame as columns
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
    # combine certain titles
    x$title["Mlle" == x$title] = "Miss"
    x$title["Ms"   == x$title] = "Miss"
    x$title["Mme"  == x$title] = "Mrs"

    ranks = c("Capt", "Col", "Major")
    x$title[x$title %in% ranks] = "Offr"

    royal = c("Don", "Dona", "Jonkheer", "Lady", "Sir", "the Countess")
    x$title[x$title %in% royal] = "Royal"

    # change unrealistic cabin letter to NA
    x$cabinLetter["T" == x$cabinLetter] = NA

    # combine certain ticketHeaders
    y = "ticketHeader"
    z = x[y][[1]]
    x[z %in% c("A2", "A4", "A5", "AQ3", "AQ4", "AS"), ][y] = "A"
    x[z %in% c("C", "CA", "CASOTON"), ][y] = "C"
    x[z %in% c("Fa", "FC", "FCC"), ][y] = "F"
    x[z %in% c("PC", "PP", "PPP"), ][y] = "P"
    x[z %in% c("SOTONO2", "SOTONOQ", "STONO2", "STONOQ"), ][y] = "SOTON"
    x[which("SCParis" == x[y]), ][y] = "SCPARIS"
    x[z %in% c("SC", "SCA3", "SCA4", "SCAH", "SCAHBasle", "SCOW", "SOC", "SOP", "SOPP", "SP", "SWPP"), ][y] = "S"
    x[z %in% c("WC", "WEP"), ][y] = "W"
    x[z %in% c("LINE", "LP"), ][y] = NA

    # turn certain columns into factors
    for (y in c("pclass", "sex", "embarked", "title", "ticketHeader", "cabinLetter"))
        x[y] = factor(x[ , y])

    # add order for these factors
    for (y in c("pclass", "cabinLetter"))
        x[y] = ordered(x[ , y])

    # remove unnecessary columns
    x = x[!(names(x) %in% c("name", "ticket", "firstName"))]

    return(x)
}

# split a factor of characters which cannot be ordered into columns of binaries
binarize <- function(x, column)
{
    y = x[column][[1]]
    for (lev in levels(y))
    {
        x[paste(column, lev, sep = "")] = factor(as.integer(sapply(y,
        function(z)
        {
            if (z %in% lev) z = 1
            else if (is.na(z)) z = NA
            else 0
        })))
    }

    x[column] = NULL

    return(x)

}

# reconstruct family relationships
recoFamily = function(train, test)
{
    test$survived = factor(NA, c(0, 1))
    combined = rbind(train, test)
    combined$famSize = combined$sibsp + combined$parch + 1
    combined$famName = paste(combined$lastName, combined$ticketNumber)
    combined$invalid = factor(0, c(0, 1))
    families = split(combined, combined$famName)

    for (f in families)
    {
        nMember = nrow(f)
        indices = rownames(f) # original row numbers in combined

        if (any(nMember != f$famSize)) # inconsistent family size
        {
            combined[indices, "invalid"] = 1
        }
        else # family size agrees with number of members
        {
            if (any(f$parch > 0)) # parents and children
            {
                mother = f[f$title == "Mrs", ]
                childs = f[f$title %in% c("Master", "Miss"), ]
                hasMom = nrow(mother) > 0
                nChild = nrow(childs)
                iChild = which(
                    if (hasMom)
                    {
                        mother$parch - 1 == f$sibsp
                    }
                    else if (nChild > 0)
                    {
                        childs$sibsp[1] == f$sibsp
                    }
                    else # only father and sons
                    {
                        1 == f$parch # 1 parent only (father)
                    })

                # if no. of parents = no. of children = 1 or 2,
                # then length(iChild) = nMember
                if (length(iChild) < nMember) # normal case
                {
                    combined[indices[ iChild], "child" ] = 1
                    combined[indices[ iChild], "parent"] = 0
                    combined[indices[-iChild], "child" ] = 0
                    combined[indices[-iChild], "parent"] = 1
                }
                else # special case for length(iChild) = nMember
                {
                    if (hasMom) 
                    {
                        combined[rownames(mother), "child" ] = 0
                        combined[rownames(mother), "parent"] = 1
                    }

                    if (nChild > 0)
                    {
                        combined[rownames(childs), "child" ] = 1
                        combined[rownames(childs), "parent"] = 0
                    }

                    if (2 == nChild ||           # 2 parents + 2 children
                        (!hasMom && nChild > 0)) # 1 father + 1 child
                    {
                        father = rownames(f[f$title == "Mr", ])

                        combined[father, "child" ] = 0
                        combined[father, "parent"] = 1
                    }

                    if (hasMom && 0 == nChild) # 1 mother + 1 son
                    {
                        son = rownames(f[f$title == "Mr", ])

                        combined[son, "child" ] = 1
                        combined[son, "parent"] = 0
                    }
                }
            }
            else # brothers/sisters or single
            {
                combined[indices, "child" ] = 0
                combined[indices, "parent"] = 0
            }

            # add info of other members' survival
            x = f$survived == 1
            nAlive = sum( x, na.rm = T)
            nDrown = sum(!x, na.rm = T)
            combined[indices, "othersAlive"] = nAlive - (!is.na(x) &  x)
            combined[indices, "othersDrown"] = nDrown - (!is.na(x) & !x)
        }
    }

    combined$child   = as.factor(combined$child)
    combined$parent  = as.factor(combined$parent)
    combined$famSize = NULL
    combined$famName = NULL

    write.csv(combined, "data/combined.csv", na = "")

    return(combined)
}

# c(10, 50, 292, 308, 547, 701, 782, 831, 856) Mrs
# c(310, 557, 600) Lady and Sir ticket1
# c(258, 505, 760) Countess
# c(53, 597, 646, 682, 721, 849) Rev Harper
# y = vector("list")
# for (i in 1 : 10) y[[i]] = randomForest(survived ~ ., trainImpute, ntree = 200, mtry = 3)
