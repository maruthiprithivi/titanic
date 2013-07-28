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
    x["otherName"] = sapply(nameSplit, function(y) {y[3]})

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
    x = x[!(names(x) %in% c("name", "ticket", "otherName"))]

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
#            else if (is.na(z)) z = 2
            else 0
        })))
    }

    x[column] = NULL

    return(x)

}

# reconstruct a family relationship
recoFamily = function(f)
{
    nMember = nrow(f)

    if (any(nMember != f$famSize)) # inconsistent family size
    {
        f$invalid = TRUE
    }
    else # family size agrees with number of members
    {
        if (any(f$parch > 0)) # parents and children
        {
            mother = which(f$title == "Mrs")
            youngs = which(f$title %in% c("Master", "Miss"))
            hasMom = length(mother) > 0
            nYoung = length(youngs) # exclude children who are Mr
            childs = which(         # indices of all children
                if (hasMom)
                {
                    f[mother, "parch"] - 1 == f$sibsp
                }
                else if (nYoung > 0)
                {
                    f[youngs[1], "sibsp"] == f$sibsp
                }
                else # only father and sons
                {
                    1 == f$parch # 1 parent only (father)
                })
            parent = -childs #indices of parents

            # if no. of parents = no. of children = 1 or 2,
            # then length(childs) = number of members in family
            # need to correct for this case
            if (length(childs) == nMember)
            {
                father = NULL
                oldSon = NULL

                if (2 == nYoung ||           # 2 parents + 2 children
                    (!hasMom && nYoung > 0)) # 1 father + 1 child
                {
                    father = which(f$title %in% c("Mr", "Rev"))
                }
                else if (hasMom && 0 == nYoung) # 1 mother + 1 son
                {
                    oldSon = which(f$title == "Mr")
                }
                else if (hasMom + nYoung == 0) # 1 father + 1 son, both Mr
                {
                    if (!any(is.na(f$age)))
                    {
                        oldSon = which.min(f$age)
                        father = -oldSon
                    }
                    else # there is missing age values
                        print(paste("Dad/son ambiguity in family", f$lastName))
                }

                childs = c(youngs, oldSon)
                parent = c(mother, father)
            }

            f[childs, "child" ] = 1
            f[childs, "parent"] = 0
            f[parent, "child" ] = 0
            f[parent, "parent"] = 1
        }
        else # brothers/sisters or single
        {
            f$child  = 0
            f$parent = 0
        }

        # add info of other members' survival
        x = f$survived == 1
        nAlive = sum( x, na.rm = T)
        nDrown = sum(!x, na.rm = T)
        f$othersAlive = nAlive - (!is.na(x) &  x)
        f$othersDrown = nDrown - (!is.na(x) & !x)
    }

    return(f)
}

# reconstruct family relationships in the combined data set of train + test
recoFamilies = function(train, test)
{
    test$survived = factor(NA, c(0, 1))
    combined = rbind(train, test)
    combined$famSize = combined$sibsp + combined$parch + 1
    combined$invalid = FALSE
    combined$child   = factor(NA, c(0, 1))
    combined$parent  = factor(NA, c(0, 1))
    combined$othersAlive = NA
    combined$othersDrown = NA

    # first find families by surname
    for (f in split(combined, combined$lastName))
    {
        combined[rownames(f), ] = recoFamily(f)
    }

    invalids = combined[combined$invalid, ]
    invalids$invalid = FALSE # reset invalid flag

    # find remaining families by ticket number
    for (f in split(invalids, invalids$ticketNumber))
    {
        combined[rownames(f), ] = recoFamily(f)
    }

    combined$famSize = NULL

    write.csv(combined, "data/combined.csv", na = "")

    return(combined)
}

# c(10, 50, 292, 308, 547, 701, 782, 831, 856) Mrs
# c(310, 557, 600) Lady and Sir ticket1
# c(258, 505, 760) Countess
# c(53, 597, 646, 682, 721, 849) Rev Harper
# y = vector("list")
# for (i in 1 : 10) y[[i]] = randomForest(survived ~ ., trainImpute, ntree = 200, mtry = 3)
