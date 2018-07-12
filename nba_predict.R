




##prediction using knn
##predicting the position the player
## Open csv
nba1  <- read.csv("Seasons_Stats.csv")
nba1[is.na(nba1)] <- 0
 nba1 = nba1[,-1]
 nba1 = nba1[,-1]
 nba1 = nba1[,-1]
 nba2=nba1[,-3]
#nba3=nba2[nba2$Age %in% 44,]

 #head(nba2)
 # data normalizing function
data_norm <- function(x){((x-min(x))/(max(x)-min(x)))}
 #applying a normalizing function to the data
 nba2_norm <- as.data.frame(lapply(nba2[,-1], data_norm))
 nba2_norm[is.na(nba2_norm)] <- 0
 
 
 #nba1_train <- nba2_norm[1:20000,]
 nba1_train <- nba2_norm[1:20000,]
 # specifying the test data from the normalized data, also we consider the number of rows
 nba1_test <- nba2_norm[nba2_norm$Age %in% (20/40),]
 
 # we import alibrary
 
 library(class)
 #we need to predict the results
 pred <- knn(nba1_train, nba1_test,nba2[1:20000,1],k=201)
 
 table(pred)
 

 ##predicting the performance of a player
 
 nba4  <- read.csv("Seasons_Stats.csv")
 nba4$Category[nba4$PTS >0 & nba4$PTS <=363] = "Worst"
 nba4$Category[nba4$PTS > 363 & nba4$PTS <=777] = "Medium"
 nba4$Category[nba4$PTS >777 & nba4$PTS <=4029] = "Best"
 
 head(nba4)
 nba4[is.na(nba4)] <- 0
 nba4 = nba4[,-1]
 nba4 = nba4[,-1]
 nba4 = nba4[,-1]
 nba4 = nba4[,-1]
 nba4 = nba4[,-1]
 nba4 = nba4[,-1]
 refcols <- c("Category")
 nba4 <- nba4[, c(refcols, setdiff(names(nba4), refcols))] 
 # data normalizing function
 data_norm <- function(x){((x-min(x))/(max(x)-min(x)))}
 #applying a normalizing function to the data
 nba4_norm <- as.data.frame(lapply(nba4[,-1], data_norm))
 nba1_train <- nba4_norm[1:20000,]
 nba1_test <- nba4_norm[nba4_norm$G %in% 0.5,]
 
 library(class)
 #we need to predict the results
 pred <- knn(nba1_train, nba1_test,nba4[1:20000,1],k=201)
 table(pred)
 
 
 
