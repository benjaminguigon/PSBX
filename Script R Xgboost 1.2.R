# Package

library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(DiagrammeR)

# Data
data = read.csv(file.choose(), header = T)
str(data)
data$rank = as.factor(data$rank)

# Partition data
set.seed(1234)
ind = sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train = data[ind == 1,]
test = data[ind == 2,]
test
# Create matrix - One-Hot Encoding for Factor variables
trainm = sparse.model.matrix(admit~.-1, data = train)
train_label = train[,"admit"]
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm = sparse.model.matrix(admit~.-1, data = test)
test_label = test[,"admit"]
test_matrix = xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Parameters

nc = length(unique(train_label))
xgb_params = list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = nc)

watchlist = list(train = train_matrix, test = test_matrix)

#eXtrem Gradient Boosting Model

bst_model = xgb.train(params = xgb_params,
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
 
e = data.frame(bst_model$evaluation_log)
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

################################################################################

#2eme exemple pour xgboost

#install.packages("modeldata")
library(modeldata)
data("stackoverflow")

# isolate X and Y
library("dplyr")
y = as.numeric(stackoverflow$Remote) - 1
X = stackoverflow %>% select(-Remote)
str(X)

# Transform factor into dummy variable
#install.packages("fastDummies")
library(fastDummies)
X = dummy_cols(X,
               remove_first_dummy = TRUE)
X = X %>% select(-Country)

# Setting the parameters
params = list(set.seed = 1997,
              eval_metric = "auc",
              objective = "binary:logistic")

# Running xgboost
#install.packages("xgboost")
library(xgboost)

model = xgboost(data = as.matrix(X),
                label = y,
                params = params,
                nrounds = 20,
                verbose = 1)

# Shap value
xgb.plot.shap(data = as.matrix(X),
              model = model,
              top_n = 5)

################################################################################

#ionosphere_csv

data("agaricus.train", package = 'xgboost')
data("agaricus.test", package = 'xgboost')

train_aga = agaricus.train
test_aga = agaricus.test
#head(train_aga)
#View(train_aga)
str(train_aga)

# Dimention du data set train
dim(train_aga$data)

# Dimention du data set test
dim(test_aga$data)

#Comme on le voit ci-dessous, les datasont stockés dans a dgCMatrixqui est 
#une matrice creuse et le labelvecteur est un numericvecteur ( {0,1}):

class(train_aga$data)
class(train_aga$label)

#Nous formerons le modèle d'arbre de décision en utilisant les paramètres suivants:

# -> objective = "binary:logistic": nous allons former un modèle de classification binaire;
# -> max.depth = 2: les arbres ne seront pas profonds, car notre cas est très simple;
# -> nthread = 2: le nombre de threads cpu que nous allons utiliser;
# -> nrounds = 2: il y aura deux passages sur les données, le second améliorera le 
# -> modèle en réduisant encore la différence entre la vérité terrain et la prédiction.


bstSparse = xgboost(data = train_aga$data, 
                    label = train_aga$label,
                    max.depth = 2, 
                    eta = 1, 
                    nthread = 2, 
                    nrounds = 2, 
                    objective = "binary:logistic") 

# XGBoost offre un moyen de les regrouper dans un fichierxgb.DMatrix . 
# Vous pouvez même y ajouter d'autres métadonnées. Cela sera utile pour les 
# fonctionnalités les plus avancées que nous découvrirons plus tard.

dtrain_aga = xgb.DMatrix(data = train_aga$data, label = train_aga$label)
bstDMatrix = xgboost(data = dtrain_aga, 
                     max.depth = 2, 
                     eta = 1,
                     nthread = 2,
                     nrounds = 2, 
                     objective = "binary:logistic")
   
### Option verbeuse 
# XGBoost dispose de plusieurs fonctionnalités pour vous aider à visualiser la 
# progression de l'apprentissage en interne. Le but est de vous aider à définir 
# les meilleurs paramètres, ce qui est la clé de la qualité de votre modèle.

# L'un des moyens les plus simples de voir la progression de l'entraînement 
# consiste à définir l' verboseoption 
    
# verbose = 0, no message

bst_aga = xgboost(data = dtrain_aga, 
                  max.depth = 2, 
                  eta = 1,
                  nthread = 2,
                  nrounds = 2, 
                  objective = "binary:logistic",
                  verbose = 0)

# verbose = 1, print evolution metric

bst_aga = xgboost(data = dtrain_aga, 
                  max.depth = 2, 
                  eta = 1,
                  nthread = 2,
                  nrounds = 2, 
                  objective = "binary:logistic",
                  verbose = 1)

# verbose = 2, also print information about tree

bst_aga = xgboost(data = dtrain_aga, 
                  max.depth = 2, 
                  eta = 1,
                  nthread = 2,
                  nrounds = 2, 
                  objective = "binary:logistic",
                  verbose = 2)


# Prédiction de base avec XGBoost

pred =  predict(bst_aga, test_aga$data)

# size of the prediction vector

print(length(pred))
print(head(pred))



# Transformer la régression en une classification binaire

prediction = as.numeric(pred > 0.5)
head(prediction)

### Mesurer les performances du modèle 

# Pour mesurer les performances du modèle, nous allons calculer 
# une métrique simple, l' erreur moyenne .


err = mean(as.numeric(pred > 0.5) != test_aga$label)
paste('test-erro =', err)

# Notez que l'algorithme n'a pas vu les testdonnées lors de la 
# construction du modèle.

# La chose la plus importante à retenir est que pour effectuer une classification, 
# il vous suffit de faire une régression vers le label , puis d'appliquer un seuil .


### Fonctionnalités avancées 

# Préparation du jeu de données 

dtrain_aga = xgb.DMatrix(data = train_aga$data, label = train_aga$label)
dtest_aga = xgb.DMatrix(data = test_aga$data, label = test_aga$label)

# Mesurer la progression de l'apprentissage avec xgb.train

# Les fonctions xgboost(simples) et xgb.train(avancées) forment des modèles.
# L'une des particularités de xgb.trainest la capacité de suivre la progression de 
# l'apprentissage après chaque tour. En raison de la façon dont le boosting fonctionne, 
# il y a un moment où avoir trop de tours conduit à un surajustement.

# Une façon de mesurer les progrès dans l'apprentissage d'un modèle est de fournir 
# à XGBoost un deuxième ensemble de données déjà classé. Il peut donc apprendre sur 
# le premier ensemble de données et tester son modèle sur le second. Certaines 
# métriques sont mesurées après chaque tour pendant l'apprentissage.

# En quelque sorte, c'est similaire à ce que nous avons fait ci-dessus avec 
# l'erreur moyenne. La principale différence est que ci-dessus c'était après 
# la construction du modèle, et maintenant c'est pendant la construction que nous 
# mesurons les erreurs.


watchlist = list(train = dtrain_aga, test = dtest_aga)

bst <- xgb.train(data=dtrain_aga,
                 max.depth=2, 
                 eta=1, 
                 nthread = 2, 
                 nrounds=4, 
                 watchlist=watchlist, 
                 objective = "binary:logistic")

# Pour une meilleure compréhension de la progression de l'apprentissage, 
# vous voudrez peut-être avoir une métrique spécifique ou même utiliser plusieurs
# métriques d'évaluation.


bst <- xgb.train(data=dtrain_aga, 
                 max.depth=2, 
                 eta=1, 
                 nthread = 2, 
                 nrounds=2, 
                 watchlist=watchlist, 
                 eval.metric = "error", 
                 eval.metric = "logloss", 
                 objective = "binary:logistic")

# eval.metricnous permet de surveiller deux nouvelles mesures pour 
# chaque tour, loglosset error.


### Boosting linéaire

# Jusqu'à présent, tous les apprentissages que nous avons réalisés étaient basés 
# sur la dynamisation des arbres. XGBoost implémente un deuxième algorithme, basé 
# sur le boosting linéaire. 

bst <- xgb.train(data=dtrain_aga,
                 booster = "gblinear",
                 nthread = 2,
                 nrounds=2,
                 watchlist=watchlist,
                 eval.metric = "error", 
                 eval.metric = "logloss", 
                 objective = "binary:logistic")

# Dans ce cas précis, l'augmentation linéaire obtient des mesures de performances 
# légèrement meilleures qu'un algorithme basé sur un arbre de décision.


### Voir l'importance / l'influence des fonctionnalités à partir du modèle appris


importance_matrix <- xgb.importance(model = bst)

print(importance_matrix)

xgb.plot.importance(importance_matrix = importance_matrix)



### Afficher les arbres à partir d'un modèle 
bst <- xgboost(data=dtrain_aga,
                 booster = "gblinear",
                 nthread = 2,
                 nrounds=2,
                 watchlist=watchlist,
                 eval.metric = "error", 
                 eval.metric = "logloss", 
                 objective = "binary:logistic")

bst_aga = xgboost(data = dtrain_aga, 
                  max.depth = 2, 
                  eta = 1,
                  nthread = 2,
                  nrounds = 2, 
                  objective = "binary:logistic",
                  verbose = 2)


xgb.dump(bst, with_stats = TRUE)

xgb.plot.tree(feature_names = agaricus.train$data@Dimnames[[2]],model = bst_aga)

################################################################################
# Le jeu de données choisi comprend 9568 lignes de données collectées sur une centrale 
# électrique au gaz sur une période de 6 ans (2006 à 2011), en fonctionnement à pleine puissance.
library(tidyverse)
library(xgboost)
library(caret)
library(readxl)
library(corrplot)
library(GGally)
# Import & création de noms de colonnes plus parlants :
df <- read_excel("/Users/benjamin.guigon/Desktop/PSB/Maths - R/R/Xgboost/Exemple Vin/Folds5x2_pp.xlsx")
colnames(df) <- c("Temp", "VEchap", "PressAtm", "Humid", "ProdElec")
df

# Prédicteurs (X) :
#  Temp = Température ambiante (en °C)
#  VEchap = Vide d’échappement (en cmHg)
#  PressAtm = Pression atmosphérique ambiante (en millibars)
#  Humid = Humidité relative (en %)
# Variable réponse à prédire (y) :
#  ProdElec = Production électrique (en MWatt)

corrplot(cor(df), method = "ellipse")
ggpairs(df)
# On observer que la production électique est corrélée négativement avec la température
ggplot(data = df, aes(x = Temp, y = ProdElec)) + geom_point() + geom_smooth(method = "lm")
# On observer que la production électique est corrélée négativement avec la pression athmospherique 
ggplot(data = df, aes(x = PressAtm, y = ProdElec)) + geom_point() + geom_smooth(method = "lm")

# Commençons par diviser le jeu de données en un échantillon de d’entraînement appelé 
# training et un second de test appelé testing:

set.seed(1337)  # Pour la 'reproductibilité'
inTrain <- createDataPartition(y = df$ProdElec, p = 0.85, list = FALSE)  
# 85% des données dans le train, et le rest dans le test 
training <- df[inTrain, ]
testing <- df[-inTrain, ]

# Ne pas oublier les valeurs qualitatives qu'il faut encoder

#X_train = xgb.DMatrix(as.matrix(training)[,-5])
X_train = as.matrix(training)[,-5]
y_train = training$ProdElec
#X_test = xgb.DMatrix(as.matrix(testing)[,-5])
X_test = as.matrix(testing)[,-5]
y_test = testing$ProdElec


trainm = sparse.model.matrix(admit~.-1, data = train)
train_label = train[,"admit"]
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm = sparse.model.matrix(admit~.-1, data = test)
test_label = test[,"admit"]
test_matrix = xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Parameters

nc = length(unique(train_label))
xgb_params = list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = nc)

watchlist = list(train = train_matrix, test = test_matrix)




# On va donc commencer par définir un objet trainControl, qui permet de contrôler
# la manière dont se fait l’entraînement du modèle, assuré par la fonction train().

# Ici, nous choisissons une validation croisée (method = ‘cv’) à 5 folds (number = 5). 
# On choisit également d’autoriser la parallélisation des calculs (allowParallel = TRUE),
# de réduire la verbosité (verboseIter = FALSE).

# trainControl = Control the computational nuances of the train function
xgb_trcontrol = trainControl(method = "boot",
                             number = 5,
                             allowParallel = TRUE, 
                             verboseIter = FALSE, 
                             returnData = FALSE)

# On définit ensuite une grille de paramètres du modèle XGBoost appelée xgbGrid.
# expand.grid = Create a tibble from all combinations of inputs
xgbGrid <- expand.grid(nrounds = c(100,200),  
                       max_depth = c(3, 5, 10, 15, 20),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## valeurs par défaut : 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1)
head(xgbGrid)
# caret testera chaque combinaisons de ces paramètres dans un modèle distinct. 
# Je me risque ici à faire une description simplifiée de quelques paramètres essentiels:
  
# -> nrounds: nombre d’itérations de boosting à effectuer. Plus il est grand, 
# et plus c’est lent

# -> max_depth: profondeur d’arbre maximale. Risque d’over-fit si trop grand, 
# et d’under-fit si trop petit

# -> colsample_bytree: pourcentage des colonnes pris pour construire un arbre 
# (un arbre est construit avec un sous-ensemble des données: lignes et colonnes)

# -> eta: ou learning rate, ce paramètre contrôle la vitesse à laquelle on 
# convergence lors de la descente du gradient fonctionnelle (par défaut = 0.3)

# -> gamma: diminution minimale de la valeur de la loss (fonction objectif) pour 
# prendre la décision de partitionner une feuille


set.seed(0)
xgb_model = train(X_train, y_train)

xgb_model = train(X_train, 
                  y_train, 
                  trControl = xgb_trcontrol, 
                  tuneGrid = xgbGrid, 
                  method = "xgbTree")



