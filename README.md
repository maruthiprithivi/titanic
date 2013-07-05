https://www.kaggle.com/c/titanic-gettingStarted

The task is to predict whether a given passenger survived the sinking of Titanic. Data consist of the attributes: passenger class, gender, name, age, number of siblings or spouse on board, number of parents or children on board, ticket number, the fare, location of the passenger's cabin on the ship, and the place of embarkation. Solutions are evaluated by comparing the percentage of correct answers on a test dataset.

Random forest was used as the classifier. Names, ticket numbers and cabin locations were excluded from training, so 7 features were used. I used 100 trees and 3 features for each decision split as these values gave the lowest out-of-bag error when I scanned through the range of possible values (up to 10000 trees).

The exercise was done in R which had a library for random forest. I trained the classifier by calling

  forest = randomForest(train$survived ~ ., train, ntree = 100, mtry = 3, na.action = na.omit)

where train was a dataset with 7 features as mentioned above. Missing values were omitted in the training.

One problem I had was the presence of missing values in the test dataset and they needed to be filled before calling the predict function. This was done by calling na.roughfix which filled the missing values using the medians of their attributes.

I obtained a test accuracy of 73.7% for my first submission to Kaggle. The score could be improved (probably by 2%) if I did not omit the missing values in training the classifier.

I improved by calling

  X = rfImpute(train$survived ~ ., train)[, 2:8]

to fill the missing values before training the classifier. All other parameters remained the same.

My score increased to 76.1%

July 3 - Obtained a score of 0.78947 by testing a random forest with 200 trees and 3 variables randomly chosen at each node, i.e. _d_ is 3. Used 9 predictors (pclass, gender, age, sibsp, parch, fare, embarked, title and ticket2). Computed the mean and median of the available age values of the passengers with the title "Master". Took the average of this mean and median to replace the missing age values of the other passengers with the title "Master". Repeated this for each title "Miss", "Mr" and "Mrs". The missing values of the other variables (fare, embarked and ticket2) were completed by rfImpute.
