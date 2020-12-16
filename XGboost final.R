library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(DiagrammeR)

# Data

df = TitanicSurvival
str(df)

y = as.numeric(df$survived )- 1
X = df %>% select(-survived)

X$sex = as.numeric(X$sex)
X$passengerClass = as.numeric(X$passengerClass)


# Transform factor into Dummy variable
library(fastDummies)

X = dummy_cols(X,
               remove_first_dummy = TRUE)


# setting the parameters

params = list(set.seed = 1502,
              eval_metric = "auc",
              objective = "binary:logistic")

# Running xgboost

model = xgboost(data = as.matrix(X),
                label = y,
                params = params,
                nrounds = 20,
                verbose = 1)

# shap value

xgb.plot.shap(data = as.matrix(X),
              model = model,
              top_n = 6)

################################################################################


df = TitanicSurvival
#data$rank = as.factor(data$rank)
df$survived = as.numeric(df$survived) -1 
survived = as.factor(df$survived)
head(df)
set.seed(1234)
ind = sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train = df[ind == 1,]
test = df[ind == 2,]
head(train)
head(test)

# Create matrix - One-Hot Encoding for Factor variables

trainm = sparse.model.matrix(survived~.-1, data = train)
train_label = train[,"survived"]
length(train_label)
dim(trainm)
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label = train_label)


################################################################################


# Partition data
set.seed(1234)

n = nrow(df)
train.index = sample(n,floor(0.75*n))
train.data = df[train.index,]
test.data = df[-train.index,]

train.label = label[train.index]
test.label = label[-train.index]
length(train.label)


# Create matrix - One-Hot Encoding for Factor variables
trainm = sparse.model.matrix(survived~.-1, data = train.data)
#train_label = train.data[,"survived"]
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label = train.label)


head(trainm)
dim(trainm)
dim(train.data)
dim(test.data)
length(train_label)
dim(df)


testm = sparse.model.matrix(survived~.-1, data = test.data)
test_label = test.data[,"survived"]
test_matrix = xgb.DMatrix(data = as.matrix(testm))#, label = test_label)

head(df)
#Parameters

nc = length(unique(train_label))
xgb_params = list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = nc)

watchlist = list(train = train_matrix, test = test_matrix)

#eXtrem Gradient Boosting Model

bst_model_1 = xgb.train(params = xgb_params,
                      data = train_matrix,
                      nrounds = 100,
                      watchlist = watchlist,
                      eta = 0.05,
                      max.depth = 8,
                      gamma = 0,
                      subsample = 1,
                      colsample_bytree = 1,
                      missing = NA,
                      set.seed = 333)

e = data.frame(bst_model_1$evaluation_log)
plot(e$iter,e$train_mlogloss, col = 'blue')
lines(e$iter,e$test_mlogloss, col = 'red')

# Feature importance

imp = xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data

p = predict(bst_model, newdata = test_matrix)
head(p)
pred = matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(.,"last")-1)
head(pred)
table(Prediction = pred$max_prob, Actual = pred$label)





#########################################################################################################

# Convert the Species factor to an integer class starting at 0
# This is picky, but it's a requirement for XGBoost
species = iris$Species
label = as.integer(iris$Species)-1
iris$Species = NULL

n = nrow(iris)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(iris[train.index,])
train.label = label[train.index]
test.data = as.matrix(iris[-train.index,])
test.label = label[-train.index]

# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

# Define the parameters for multinomial classification
num_class = length(levels(species))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)
# Review the final model and results
xgb.fit


xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(species)
xgb.pred

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(species)[test.label+1]

# Calculate the final accuracy
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%",100*result)))


#################################################################################
library(caTools)
library(caret)

Sonar = read.csv(file = '/Users/benjamin.guigon/Desktop/PSB/Maths - R/R/Xgboost/sonar_csv.csv')

DataFrame = Sonar
dim(DataFrame)
head(DataFrame,3)


ind = createDataPartition(DataFrame$Class, p = 2/3, list = FALSE)

trainDF = DataFrame[ind,]
testDF = DataFrame[-ind,]

ControlParametres = trainControl(method = 'cv',
                                  number = 5,
                                  classProbs = TRUE)

parametersGrid = expand.grid(eta = 0.1,
                             colsample_bytree = c(0.5,0.7),
                             max_depth = c(3,6),
                             nrounds = 100,
                             gamma = 1,
                             min_child_weight = 2,
                             subsample = c(0,1,2))
parametersGrid 


modelxgboost = train(Class~.,
                     data = trainDF,
                     method ="xgbTree",
                     trControl = ControlParametres,
                     tuneGrid = parametersGrid)

modelxgboost

prediction = predict(modelxgboost,testDF)

t = table(predictions = prediction, actual = testDF$Class)
t




