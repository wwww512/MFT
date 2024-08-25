#Machine Learning Model Refining

#Data Preparation
Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2))
colnames(Data_ML)[1] <- "Label"
Data_ML$Label <- as.factor(Data_ML$Label)

#Package Loading
library(mlr3)
library(mlr3verse)
library(mlr3viz)
library(mlr3learners)
library(mlr3tuning)
library(mlr3extralearners)
library(ggplot2)

#Task Creation
Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
task_PD$set_col_roles("rn", roles = "name")
print(task_PD)
print(task_PD$col_roles)

# #XG
# Importance_XG <- matrix(0,50,100)
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG_Best <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                          max_depth = 1, subsample = 0.5,
#                          colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                          lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG_Best$train(task_PD, row_ids = 1:485)
#   Importance_XG[,i] <- as.vector(rownames(as.data.frame(learner_XG_Best$importance())))[1:50]
# }
# rm(i)
# write.csv(Importance_XG, "Machine_Learning_Model_Refining/Importance_XG.csv")
# #Delete the first row and the first col

#Feature Number Determination
Importance_XG <- read.csv("Machine_Learning_Model_Refining/Importance_XG.csv", header = F)
table(as.vector(as.matrix(Importance_XG)))
Importance_XG <- data.frame(Importance_XG)
Protein_XG_Stat <- as.data.frame(table(as.vector(as.matrix(Importance_XG))))
length(which(Protein_XG_Stat$Freq >= 30))
Protein_XG <- as.character(Protein_XG_Stat$Var1)[which(Protein_XG_Stat$Freq >= 30)]
rm(Protein_XG_Stat)
rm(task_PD, Data_ML)
rm(Importance_XG)

# write.csv(Protein_XG, "Machine_Learning_Model_Refining/56ProteinsAfterImportanceFiltering.csv")
#Delete the first row and the first col

rm(Ratio_Data_R2)
rm(Ratio_Data_New_impseqrob_combat)
