library(Rcpp)
library(imager)
library(tictoc)
library(ggplot2)
library(jpeg)
library(magick)
library(e1071)


# set wd to reduced images ------------------------------------------------

# Josh wd desktop and laptop: 

reduced_dir <- "C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project/Images/60 Images"
reduced_dir <- "C:/Users/thema/Dropbox/Topics in Math Stats 5931/Final Project/Images/60 Images"

# Henri wd: 

reduced_dir <- 
  
  # Jennifer wd:
  
  reduced_dir <- "C:/Users/jans7/OneDrive - Marquette University/Fall 2022/MSSC 5931 - Topics in Math or Stats/Project/NewFace_60"

# setting working directory -----------------------------------------------

setwd(reduced_dir)

# checking for empty folders ----------------------------------------------

if (length(list.files(reduced_dir)) == 0){
  stop("Empty Folder")
}

folders <- dir(path = reduced_dir, pattern = NULL, all.files = FALSE,
               full.names = FALSE, recursive = FALSE,
               ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

if (length(folders) < length(folders)){
  stop("Missing atleast 1 folder")
}

# converting picture to vector function for coding checks  ----------------

# (not utilized in the for loop to create matrix of images)
n = 250

# Putting data into matrix -----------------------------------------

# CREATING MATRIX OF IMAGES (nxn)

X <- c()

tic("runtime")
for (i in 1:length(folders)) {
  setwd(paste0(reduced_dir, "/",folders[i]))
  photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  for (j in 1:60) {
    pic <- grayscale(load.image(photos[j]))
    pic <- resize(pic, n, n)
    pic <- matrix(pic,nrow=n)[-c(c(1:floor(0.25*n)),c(floor(0.75*n+1):n)),-c(c(1:floor(0.25*n)),c(floor(0.75*n+1):n))]
    vector <- matrix(pic, ncol=1)[,1]
    X <- cbind(X, vector)
  }
}
toc()

# plotting mean face for fun (he don't look great)

# full PCA of image matrix -- need to do pca of transpose --------

# do NOT center and scale, this causes the faces to become darker than we want

tic("pca runtime")
faces_pca <- prcomp(t(X), center = FALSE, scale. = FALSE)
toc()

# checking for total explained variance limit
# paramater p: our goal for explained variance

importance <- as.data.frame(summary(faces_pca)$importance)

p = 0.99

for (i in 1:ncol(importance)) {
  if(importance[3,i] < p){
  } else {
    r <- i
    print(r)
    break
  }
}

# scree plot for PCA (first 10 components)

var_explained = faces_pca$sdev^2 / sum(faces_pca$sdev^2)
qplot(c(1:10), var_explained[1:10]) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,10,1)) +
  ylim(0,1)

# plotting Eigenfaces and reconstructing images ---------------------------

# faces_pca$x are the scores of the images

setwd("C:/Users/jdseidma/Dropbox/Topics in Math Stats 5931/Final Project")
setwd("../..")

# first eigenface and plot

EigenFaces <- faces_pca$rotation[,1:r]

ef_1 <- matrix(EigenFaces[,1], ncol = 1)
ef_1_mat <- matrix(ef_1, nrow = n)

plot(as.cimg(ef_1_mat))

# reconstruct: whatever # image it is, do plot as.cimg(matrix(restr[#,], ncol = n))

# restr <- faces_pca$x[,1:r]%*%t(EigenFaces)
# 
# restr <- scale(restr, center = -1*faces_pca$center, scale = 1/faces_pca$scale)

# SVM --------------------------------------------------------------

y<-c()

for (i in 1:length(folders)) {
  setwd(paste0(reduced_dir, "/",folders[i]))
  photos <- dir(path = paste0(reduced_dir, "/",folders[i]), 
                pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  y <- c(y, rep(i, each=60))
}

# no PCA

mydata <- as.data.frame(cbind(y, faces_pca$x))
mydata$y <- as.factor(mydata$y)

#for loop splitting ~3.61 s

tic("for loop PCA splitting")
train <- c()
val <- c()
test <- c()
for (i in 1:length(mydata[,1])) {
  if (i%%60 <= 48 & i%%60 != 0) {
    train <- rbind(train, mydata[i,])
  }
  else
  {
    if (i%%60 <= 54 & i%%60 != 0) {
      val <- rbind(val,mydata[i,])
    }
    else
    {
      if (i%%60 <= 60 || i%%60 == 0) {
        test <- rbind(test, mydata[i,])
      }
    }
  }
}
toc()

# index splitting~0.02 s

tic("index PCA splitting")
data_vec <- 1:length(mydata[,1])
train_vec <- data_vec[data_vec%%60 <= 48 & data_vec%%60 != 0]
val_vec <- data_vec[data_vec%%60 <= 54 & data_vec%%60 > 48]
test_vec <- data_vec[data_vec%%60 <= 60 & data_vec%%60 > 54 | data_vec%%60 == 0]

train <- mydata[train_vec,]
val <- mydata[val_vec,]
test <- mydata[test_vec,]
toc()

# choosing the best kernel parameter for our final model

accuracy <- c()
max <- 0
gammas <- c(0.1,0.01,0.001,0.0001,0.00001,0.000001)

for (i in 1:length(gammas)) {
  classifier <- svm(y ~ ., data = train, gamma = gammas[i], kernel = "radial")
  prediction <- predict(classifier, newdata = val)
  m <- sum(prediction == val$y)/length(prediction)
  accuracy <- c(accuracy, m)
  if (m == max(accuracy)){
    max <- m
  }
  if (i == length(gammas)){
    print(paste0("Best gamma is ", gammas[i]))
  }
}

# accuracy of validation set

classifier <- svm(y ~ ., data = train, gamma = 0.000001, kernel = "radial")
prediction <- predict(classifier, newdata = val)
sum(prediction == val$y)/length(prediction)

# accuracy of test set

prediction2 <- predict(classifier, newdata = test)
sum(prediction2 == test$y)/length(prediction2)

# putting y and the predictors together and making y a factor for SVM
# with PCA

mydata <- as.data.frame(cbind(y, faces_pca$x[,1:r]))
mydata$y <- as.factor(mydata$y)

# for loop splitting ~0.93 s

tic("for loop PCA splitting")
train <- c()
val <- c()
test <- c()
for (i in 1:length(mydata[,1])) {
  if (i%%60 <= 48 & i%%60 != 0) {
    train <- rbind(train, mydata[i,])
  }
  else
  {
    if (i%%60 <= 54 & i%%60 != 0) {
      val <- rbind(val,mydata[i,])
    }
    else
    {
      if (i%%60 <= 60 || i%%60 == 0) {
        test <- rbind(test, mydata[i,])
      }
    }
  }
  
}
toc()

# index splitting ~0.01 s

tic("index PCA splitting")
data_vec <- 1:length(mydata[,1])
train_vec <- data_vec[data_vec%%60 <= 48 & data_vec%%60 != 0]
val_vec <- data_vec[data_vec%%60 <= 54 & data_vec%%60 > 48]
test_vec <- data_vec[data_vec%%60 <= 60 & data_vec%%60 > 54 | data_vec%%60 == 0]

train <- mydata[train_vec,]
val <- mydata[val_vec,]
test <- mydata[test_vec,]
toc()

# choosing the best kernel parameter for our final model

accuracy <- c()
max <- 0
gammas <- c(0.1,0.01,0.001,0.0001,0.00001,0.000001)

for (i in 1:length(gammas)) {
  classifier <- svm(y ~ ., data = train, gamma = gammas[i], kernel = "radial")
  prediction <- predict(classifier, newdata = val)
  m <- sum(prediction == val$y)/length(prediction)
  accuracy <- c(accuracy, m)
  if (m == max(accuracy)){
    max <- m
  }
  if (i == length(gammas)){
    print(paste0("Best gamma is ", gammas[i]))
  }
}

# accuracy of validation set

classifier <- svm(y ~ ., data = train, gamma = 0.000001, kernel = "radial")
prediction <- predict(classifier, newdata = val)
sum(prediction == val$y)/length(prediction)

# accuracy of test set

prediction2 <- predict(classifier, newdata = test)
sum(prediction2 == test$y)/length(prediction2)

(var <- table(as.numeric(prediction2), as.matrix(test[,1])))
(var <- table(as.numeric(prediction), as.matrix(val[,1])))

for (i in 1:length(var[1,])) {
  print(var[i,i]/sum(var[,i]))
}

# pictures of our people --------------------------------------------------

pplot <- function(i){
  matrix <- matrix(X[,60*(i-1)+1], nrow = n/2)
  plot(as.cimg(matrix), axes = F)
}

setwd("C:/Users/jdseidma/Desktop/Projects/Topics/MathematicalFoundationOfDataScience")

for (i in 1:8) {
  jpeg(paste0(folders[i],"ppt",".jpeg"), quality = 100)
  pplot(i)
  dev.off()
}

