# we are using the algorithm to determine the number of patients with different categories of cancer ie B and M(these are the ).
#Getting the data
wdbc <- read.table(file.choose(), sep=',')

# excluding the first variable (id), since its not required
wdbc <- wdbc[,-1]


# data normalizing function
data_norm <- function(x){((x-min(x))/(max(x)-min(x)))}

#applying a normalizing function to the data
wdbc_norm <- as.data.frame(lapply(wdbc[,-1], data_norm))
summary(wdbc_norm)
# specifying the train data from the normalized data, but we consider the number of rows
wdbc_train <- wdbc_norm[1:450,]
# specifying the test data from the normalized data, also we consider the number of rows
wdbc_test <- wdbc_norm[451:569,]
# we import alibrary
library(class)
#we need to predict the results
pred <- knn(wdbc_train, wdbc_test,wdbc[1:450,1],k=21)
# lets predict the number of people who have either the cancer of type B or type M
#These will appear in atabular format.
table(pred,wdbc[451:569,1])