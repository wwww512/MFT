#Machine Learning Tuning and Benchmark

#Tuning

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

# #KNN
# 
# #Learner
# set.seed(1)
# learner_KNN <- lrn("classif.kknn")
# learner_KNN$predict_type <- "prob"
# learner_KNN
# learner_KNN$param_set
# 
# #SearchSpace
# search_space_KNN <- ps(
#   k = p_int(lower = 31, upper = 90),
#   distance = p_int(lower = 1, upper = 3)
# )
# search_space_KNN
# 
# #Evaluation
# cv_inner_KNN <- rsmp("cv", folds = 5)
# measure <- msr("classif.auc")
# 
# #Terminator
# evals_KNN <- trm("evals", n_evals = 100)
# 
# #Tuning
# instance_KNN <- TuningInstanceSingleCrit$new(
#   task = task_PD,
#   learner = learner_KNN,
#   resampling = cv_inner_KNN,
#   measure = measure,
#   search_space = search_space_KNN,
#   terminator = evals_KNN
# )
# tuner_KNN <- tnr("random_search")
# tuner_KNN$optimize(instance_KNN)
# instance_KNN$result_learner_param_vals
# #k = 78, distance = 1
# 
# #SVM with radial kernel
# 
# #Learner
# set.seed(1)
# learner_SVM <- lrn("classif.svm", type = "C-classification")
# learner_SVM$predict_type <- "prob"
# learner_SVM
# learner_SVM$param_set
# 
# #SearchSpace
# search_space_SVM <- ps(
#   cost = p_dbl(lower = 2, upper = 5),
#   gamma = p_dbl(lower = 0.001, upper = 0.004),
#   kernel = p_fct(list("radial"))
# )
# search_space_SVM
# 
# #Evaluation
# cv_inner_SVM <- rsmp("cv", folds = 5)
# measure <- msr("classif.auc")
# 
# #Terminator
# evals_SVM <- trm("evals", n_evals = 100)
# 
# #Tuning
# instance_SVM <- TuningInstanceSingleCrit$new(
#   task = task_PD,
#   learner = learner_SVM,
#   resampling = cv_inner_SVM,
#   measure = measure,
#   search_space = search_space_SVM,
#   terminator = evals_SVM
# )
# tuner_SVM <- tnr("random_search")
# tuner_SVM$optimize(instance_SVM)
# instance_SVM$result_learner_param_vals
# #cost = 4.750579, gamma = 0.001728199, kernel = radial
# 
# #RF
# 
# #Learner
# set.seed(1)
# learner_RF <- lrn("classif.randomForest")
# learner_RF$predict_type <- "prob"
# learner_RF
# learner_RF$param_set
# 
# #SearchSpace
# search_space_RF <- ps(
#   ntree = p_int(lower = 500, upper = 1000),
#   maxnodes = p_int(lower = 10, upper = 40)
# )
# 
# #Evaluation
# cv_inner_RF <- rsmp("cv", folds = 5)
# measure <- msr("classif.auc")
# 
# #Terminator
# evals_RF <- trm("evals", n_evals = 100)
# 
# #Tuning
# instance_RF <- TuningInstanceSingleCrit$new(
#   task = task_PD,
#   learner = learner_RF,
#   resampling = cv_inner_RF,
#   measure = measure,
#   search_space = search_space_RF,
#   terminator = evals_RF
# )
# tuner_RF <- tnr("random_search")
# tuner_RF$optimize(instance_RF)
# instance_RF$result_learner_param_vals
# #ntree = 947, maxnodes = 35
# 
# #XG
# 
# #Learner
# set.seed(2)
# learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                  max_depth = 1, subsample = 0.5,
#                  colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                  lambda = 1, alpha = 0)
# learner_XG$predict_type <- "prob"
# learner_XG
# learner_XG$param_set
# 
# #SearchSpace
# search_space_XG <- ps(
#   nrounds = p_int(lower = 30, upper = 180)
# )
# search_space_XG
# 
# #Evaluation
# cv_inner_XG <- rsmp("cv", folds = 5)
# measure <- msr("classif.auc")
# 
# #Terminator
# evals_XG <- trm("evals", n_evals = 100)
# 
# #Tuning
# instance_XG <- TuningInstanceSingleCrit$new(
#   task = task_PD,
#   learner = learner_XG,
#   resampling = cv_inner_XG,
#   measure = measure,
#   search_space = search_space_XG,
#   terminator = evals_XG
# )
# tuner_XG <- tnr("random_search")
# tuner_XG$optimize(instance_XG)
# instance_XG$result_learner_param_vals
# #nrounds = 159

#Benchmark

#Ranking

#Pre-Feature-Selection
filter_anova <- flt("anova")
filter_anova$calculate(task_PD)
filter_anova$scores
FS_anova <- as.data.frame(filter_anova$scores)

filter_kt <- flt("kruskal_test")
filter_kt$calculate(task_PD)
filter_kt$scores
FS_kt <- as.data.frame(filter_kt$scores)

library(FSelectorRcpp)
filter_ig <- flt("information_gain")
filter_ig$calculate(task_PD)
filter_ig$scores
FS_ig <- as.data.frame(filter_ig$scores)

Rank_anova <- data.frame(Rank1 = 1:126)
rownames(Rank_anova) <- rownames(FS_anova)
Rank_kt <- data.frame(Rank2 = 1:126)
rownames(Rank_kt) <- rownames(FS_kt)
Rank_ig <- data.frame(Rank3 = 1:126)
rownames(Rank_ig) <- rownames(FS_ig)
Rank_kt <- Rank_kt[rownames(Rank_anova),]
Rank_ig <- Rank_ig[rownames(Rank_anova),]

Rank <- data.frame(Rank1 = Rank_anova$Rank1, Rank2 = Rank_kt, Rank3 = Rank_ig)
rownames(Rank) <- rownames(Rank_anova)
Rank$MR <- apply(Rank, 1, mean)
library(dplyr)
Rank <- arrange(Rank, MR)
Protein_Rank <- rownames(Rank)
rm(FS_anova, FS_ig, FS_kt, filter_anova, filter_ig, filter_kt)
rm(Rank_ig, Rank_kt, Rank_anova, Rank)
rm(Data_ML, task_PD)

# #Benchmark15
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:15]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix15 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix15[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix15[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Logistic
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix15[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix15[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix15[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix15[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix15, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix15.csv")
# #Delete the first row and the first col
# 
# #Benchmark20
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:20]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix20 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix20[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix20[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Logistic
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix20[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix20[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix20[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix20[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix20, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix20.csv")
# #Delete the first row and the first col
# 
# #Benchmark25
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:25]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix25 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix25[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix25[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Logistic
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix25[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix25[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix25[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix25[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix25, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix25.csv")
# #Delete the first row and the first col
# 
# #Benchmark30
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:30]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix30 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix30[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix30[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Logistic
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix30[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix30[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix30[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix30[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix30, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix30.csv")
# #Delete the first row and the first col
# 
# #Benchmark35
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:35]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix35 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix35[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix35[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Logistic
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix35[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix35[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix35[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix35[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix35, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix35.csv")
# #Delete the first row and the first col
# 
# #Benchmark40
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:40]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix40 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix40[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix40[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Logistic
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix40[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix40[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix40[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix40[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix40, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix40.csv")
# #Delete the first row and the first col
# 
# #Benchmark45
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:45]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix45 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix45[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix45[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Logistic
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix45[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix45[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix45[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix45[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix45, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix45.csv")
# #Delete the first row and the first col
# 
# #Benchmark50
# 
# #Data Preparation
# Data_ML <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_Rank[1:50]]))
# colnames(Data_ML)[1] <- "Label"
# Data_ML$Label <- as.factor(Data_ML$Label)
# 
# #Task Creation
# Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
# task_PD <- as_task_classif(Data_ML, target = "Label", positive = "C")
# task_PD$set_col_roles("rn", roles = "name")
# print(task_PD)
# print(task_PD$col_roles)
# 
# ComparisonMatrix50 <- matrix(0,100,6)
# 
# #KNN
# for (i in 1:100) {
#   set.seed(i)
#   learner_KNN <- lrn("classif.kknn", k = 78, distance = 1)
#   learner_KNN$predict_type <- "prob"
#   learner_KNN
#   learner_KNN$param_set
#   resampling_KNN <- rsmp("cv", folds = 5)
#   rr_KNN <- resample(task_PD, learner_KNN, resampling_KNN)
#   ComparisonMatrix50[i,1] <- rr_KNN$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #Naive Bayes
# for (i in 1:100) {
#   set.seed(i)
#   learner_NB <- lrn("classif.naive_bayes")
#   learner_NB$predict_type <- "prob"
#   learner_NB
#   learner_NB$param_set
#   resampling_NB <- rsmp("cv", folds = 5)
#   rr_NB <- resample(task_PD, learner_NB, resampling_NB)
#   ComparisonMatrix50[i,2] <- rr_NB$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #LL
# for (i in 1:100) {
#   set.seed(i)
#   learner_LL <- lrn("classif.glmnet", lambda = 0, alpha = 1)
#   learner_LL$predict_type <- "prob"
#   learner_LL
#   learner_LL$param_set
#   resampling_LL <- rsmp("cv", folds = 5)
#   rr_LL <- resample(task_PD, learner_LL, resampling_LL)
#   ComparisonMatrix50[i,3] <- rr_LL$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #SVM
# for (i in 1:100) {
#   set.seed(i)
#   learner_SVM <- lrn("classif.svm", type = "C-classification", cost = 4.750579, gamma = 0.001728199, kernel = "radial")
#   learner_SVM$predict_type <- "prob"
#   learner_SVM
#   learner_SVM$param_set
#   resampling_SVM <- rsmp("cv", folds = 5)
#   rr_SVM <- resample(task_PD, learner_SVM, resampling_SVM)
#   ComparisonMatrix50[i,4] <- rr_SVM$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #RF
# for (i in 1:100) {
#   set.seed(i)
#   learner_RF <- lrn("classif.randomForest", ntree = 947, maxnodes = 35)
#   learner_RF$predict_type <- "prob"
#   learner_RF
#   learner_RF$param_set
#   resampling_RF <- rsmp("cv", folds = 5)
#   rr_RF <- resample(task_PD, learner_RF, resampling_RF)
#   ComparisonMatrix50[i,5] <- rr_RF$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# #XG
# for (i in 1:100) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 1, colsample_bynode = 1, colsample_bytree = 1, eta = 0.1,
#                     lambda = 1, alpha = 0, nrounds = 159)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   resampling_XG <- rsmp("cv", folds = 5)
#   rr_XG <- resample(task_PD, learner_XG, resampling_XG)
#   ComparisonMatrix50[i,6] <- rr_XG$aggregate(msr("classif.auc"))
# }
# rm(i)
# 
# write.csv(ComparisonMatrix50, "Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix50.csv")
# #Delete the first row and the first col

ComparisonMatrix15 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix15.csv", header = FALSE)
ComparisonMatrix20 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix20.csv", header = FALSE)
ComparisonMatrix25 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix25.csv", header = FALSE)
ComparisonMatrix30 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix30.csv", header = FALSE)
ComparisonMatrix35 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix35.csv", header = FALSE)
ComparisonMatrix40 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix40.csv", header = FALSE)
ComparisonMatrix45 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix45.csv", header = FALSE)
ComparisonMatrix50 <- read.csv("Machine_Learning_Tuning_and_Benchmark/ComparisonMatrix50.csv", header = FALSE)

library(ggsci)
ComparisonMatrix15_DF <- data.frame(ComparisonMatrix15 = as.vector(as.matrix(ComparisonMatrix15)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box15 <- ggplot(data = ComparisonMatrix15_DF, aes(x = Classifier, y = ComparisonMatrix15, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("15 Proteins")
plot_box15

apply(ComparisonMatrix15, 2, mean)

rm(ComparisonMatrix15)
rm(ComparisonMatrix15_DF)

library(ggsci)
ComparisonMatrix20_DF <- data.frame(ComparisonMatrix20 = as.vector(as.matrix(ComparisonMatrix20)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box20 <- ggplot(data = ComparisonMatrix20_DF, aes(x = Classifier, y = ComparisonMatrix20, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("20 Proteins")
plot_box20

apply(ComparisonMatrix20, 2, mean)

rm(ComparisonMatrix20)
rm(ComparisonMatrix20_DF)

library(ggsci)
ComparisonMatrix25_DF <- data.frame(ComparisonMatrix25 = as.vector(as.matrix(ComparisonMatrix25)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box25 <- ggplot(data = ComparisonMatrix25_DF, aes(x = Classifier, y = ComparisonMatrix25, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("25 Proteins")
plot_box25

apply(ComparisonMatrix25, 2, mean)

rm(ComparisonMatrix25)
rm(ComparisonMatrix25_DF)

library(ggsci)
ComparisonMatrix30_DF <- data.frame(ComparisonMatrix30 = as.vector(as.matrix(ComparisonMatrix30)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box30 <- ggplot(data = ComparisonMatrix30_DF, aes(x = Classifier, y = ComparisonMatrix30, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("30 Proteins")
plot_box30

apply(ComparisonMatrix30, 2, mean)

rm(ComparisonMatrix30)
rm(ComparisonMatrix30_DF)

library(ggsci)
ComparisonMatrix35_DF <- data.frame(ComparisonMatrix35 = as.vector(as.matrix(ComparisonMatrix35)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box35 <- ggplot(data = ComparisonMatrix35_DF, aes(x = Classifier, y = ComparisonMatrix35, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("35 Proteins")
plot_box35

apply(ComparisonMatrix35, 2, mean)

rm(ComparisonMatrix35)
rm(ComparisonMatrix35_DF)

library(ggsci)
ComparisonMatrix40_DF <- data.frame(ComparisonMatrix40 = as.vector(as.matrix(ComparisonMatrix40)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box40 <- ggplot(data = ComparisonMatrix40_DF, aes(x = Classifier, y = ComparisonMatrix40, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("40 Proteins")
plot_box40

apply(ComparisonMatrix40, 2, mean)

rm(ComparisonMatrix40)
rm(ComparisonMatrix40_DF)

library(ggsci)
ComparisonMatrix45_DF <- data.frame(ComparisonMatrix45 = as.vector(as.matrix(ComparisonMatrix45)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box45 <- ggplot(data = ComparisonMatrix45_DF, aes(x = Classifier, y = ComparisonMatrix45, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("45 Proteins")
plot_box45

apply(ComparisonMatrix45, 2, mean)

rm(ComparisonMatrix45)
rm(ComparisonMatrix45_DF)

library(ggsci)
ComparisonMatrix50_DF <- data.frame(ComparisonMatrix50 = as.vector(as.matrix(ComparisonMatrix50)), Classifier = rep(c("KNN","NB","L","SVM","RF","XG"), each = 100))
plot_box50 <- ggplot(data = ComparisonMatrix50_DF, aes(x = Classifier, y = ComparisonMatrix50, fill = Classifier)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(size = 0.7) +
  theme_classic() +
  scale_color_nejm() +
  scale_fill_nejm() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
  scale_x_discrete(limits = c("KNN","NB","L","SVM","RF","XG")) +
  labs(y = "CV-AUC (100 times)", x = "Classifier") +
  theme(legend.position = "None") +
  ggtitle("50 Proteins")
plot_box50

apply(ComparisonMatrix50, 2, mean)

rm(ComparisonMatrix50)
rm(ComparisonMatrix50_DF)

library(patchwork)
plot_box15 + plot_box20 + plot_box25 + plot_box30 + plot_box35 + plot_box40 + plot_box45 + plot_box50 + plot_layout(nrow = 2, ncol = 4)
rm(plot_box15, plot_box20, plot_box25, plot_box30, plot_box35, plot_box40, plot_box45, plot_box50)

rm(Protein_Rank)
