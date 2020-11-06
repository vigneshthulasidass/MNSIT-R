library(caret)
library(rpart)
library(rpart.plot)
library(plyr)

#loading the HWR data
image.data = read.csv("/MNIST-data/mnist_train.csv",
                      header = F)

#Viewing the HWR data
View(image.data)

#Structure of the HWR datset
str(image.data)

# Changing V1 (label) as a factor
image.data$V1 = as.factor(image.data$V1)

dim(image.data)

# Summary of the label column

summary(image.data$V1)

# plot function to verify the label column data

rotate <- function(x) {
  return(t(apply(x, 2, rev)))
}

plot_matrix <- function(vec) {
  q <- matrix(vec, 28, 28, byrow = TRUE)
  nq <- apply(q, 2, as.numeric)
  image(rotate(nq), col = gray((0:255)/255))
}

plot_matrix(image.data[6, 2:785])


########### Preparing the regression trees for each pixel ######################################

i=0
cart_fit = list()
pred_list = list()

# Running the regression loop for the 784 pixel features

for(i in colnames(image.data[, -1])){
  # preparing the formula to fit the model
  formula = as.formula(paste(i,"V1", sep = "~"))
  #storing the rpart output to a list
  cart_fit[[i]] = rpart(formula, data = image.data, method = "anova")

}

summary(cart_fit$V100) # summarizing the fit

# Plotting the regression trees

plot = rpart.plot(cart_fit$V100)

#preparing the list for prediction
pred_list = list()

# Running the prediction on my model for the values of our digit label
i=0
for (i in (image.data$V1)){

  pred_list[[i]]= predict(cart_fit)

}

# Viewing the predicted values in a list
View(pred_list)

# VIewing the summary of the prediction list
summary(pred_list)

# Changing the list into dataframe to apply it plot_matrix function
pred_df = ldply(pred_list,data.frame)

# Viewing the prediction data frame
View(pred_df)
head(pred_df)

# plotting the output digit form the prediction dataframe
plot_matrix(pred_df["16",2:785])
