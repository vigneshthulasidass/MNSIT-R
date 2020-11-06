library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

#loading the HWR data
image.data = read.csv("/MNIST-data/mnist_train.csv", header = F)

#Viewing the HWR data
View(image.data)

#summary(image.data$V778)
#summary(model_j778$y)

#Structure of the HWR datset
str(image.data)

image.data$V1 = as.factor(image.data$V1)

image.data = data.frame(image.data)

dim(image.data)

# Summary of the label column

summary(image.data$V1)

image.data.label = image.data[ , 1]

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


i = 0
#rm(model)
model_j = 0
#rm(fmla)

#looing with column names in the dataset
for (i in colnames(image.data[, -1])){  #colnames(image.data)


  #model_j = rpart(image.data[, i] ~ image.data$V1, data=image.data, method = "anova" )
  #hwr_model=assign(paste0("model_j",i),model_j,.GlobalEnv)

  #model_j = rpart(i~V1, data = image.data, method = "anova")

  #formula <- as.formula(paste(i,"V1", sep = "~"))
  #model_j = rpart(formula, data = image.data, method = "anova")

  formula = as.formula(paste(i,"V1", sep = "~"))
  modl.name = paste("model", i, sep="_")
  model_list = assign(modl.name, rpart(formula, data = image.data, method = "anova"))

}

class(model_V100)

i=0
#predicts=list()
for (i in colnames(image.data[, -1]) ){

  pred_mod.name = paste("predict", i, sep = "_")
  formula <- as.formula(paste(i,"V1", sep = "~"))
  prediction_list =assign(pred_mod.name, predict(rpart(formula, data = image.data, method = "anova"), image.data))
}

summary(prediction_list)

View(predict_V100)
View(predict_V10)

#looping the model with respect to i values and creating new model in each iteration.
i=0
for( i in 2:785 ){
  mod.name <- paste("model", i, sep="_")
  model_list=assign(mod.name, rpart(image.data[, i] ~ image.data$V1, data = image.data, method="anova")) #, control=rpart.control(maxdepth=i)
}

summary(model_110)


i = 0
for (i in 2:785){

  pixel[i]=predict(model, image.data$V1) #, newdata=data.frame(V1 = 5))
  #pixel[i]=prediction(model_j[i], image.data$V1)
}


#for loop with a list

i=0
model = list()
for (i in 2:785){

  model[i] = rpart( image.data[, i] ~ image.data$V1, image.data, method = "anova" )
}

str(model[100])

i = 0
for (i in 2:785){

  pixel[i]=predict(model[i], newdata=data.frame(V1 = 5), type = "vector") #newdata=data.frame(V1 = 5)
  #pixel[i]=prediction(model_j[i], image.data$V1)
}


df_model <- data.frame(matrix(unlist(model), nrow=length(model), byrow=T), stringsAsFactors=FALSE)

mat_model = matrix(unlist(model), ncol = length(model), byrow = T)


str(df_model)

i = 0
for (i in 2:784){

  pixel[i]=predict(mat_model,data=image.data[i])

}

pred = predict(model_778,type = "vector")
summary(pred)

df= as.data.frame(image.data[,-1])

fitx=rpart(V2 ~ df$V3, data = image.data, method = "anova")
#control = rpart.control (minsplit = 30, cp = 0.001)))
