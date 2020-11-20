library(datasets)
library(data.table) 
library(vcd)
library(xgboost)
library(models)
library(Matrix)


data(Arthritis)
df <- data.table(Arthritis, keep.rownames = FALSE)

head(df)

str(df)

head(df[,AgeDiscret := as.factor(round(Age/10,0))]) # Mettre les ages à la dixaine prêt

head(df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))]) # Classé les gens
# en 2 catégories d'age

df[,ID:=NULL] # Mettre l'ID à 0

levels(df[,Treatment]) # Connaitre les valeurs possibles de la colonne Treatment

# Encodage à chaud
# Le but est de transformer chaque valeur de chaque caractéristique catégorielle 
# en une caractéristique binaire .{0, 1}


sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)

output_vector = df[,Improved] == "Marked"


bst <- xgboost(data = sparse_matrix, 
               label = output_vector, 
               max.depth = 4,
               eta = 1, 
               nthread = 2, 
               nrounds = 10,
               objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], 
                             model = bst)
head(importance)

importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]],
                                model = bst, 
                                data = sparse_matrix, 
                                label = output_vector)

head(importanceRaw)
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean)

importanceRaw


xgb.plot.importance(importance_matrix = importanceRaw)
