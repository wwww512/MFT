#PRM


#Training and Test 1

#Data Loading
library(readr)
Matrix <- as.data.frame(read_csv("PRM/MFT_PRM_ProtMatrix.csv"))
rownames(Matrix) <- Matrix$...1
Matrix <- Matrix[,-1]

Label <- as.data.frame(read_csv("PRM/MFT_PRM_Subtype_Label.csv"))
rownames(Label) <- Label$MatrixReName

Matrix <- Matrix[,rownames(Label)]

#Split
Index1 <- which(Label$Type == "FT-UMP")
Index2 <- which(Label$Oncocytic == "Y")
Index3 <- which(Label$DistantMeta == "Y")
Index4 <- which(Label$Recurrence == "Y")
Index <- sort(union(Index1, union(Index2, union(Index3, Index4))))

Label_Main <- Label[-Index,]
Label_Other <- Label[Index,]

Matrix_Main <- Matrix[,rownames(Label_Main)]
Matrix_Other <- Matrix[,rownames(Label_Other)]

rm(Matrix)
rm(Label)
rm(Index1)
rm(Index2)
rm(Index3)
rm(Index4)
rm(Index)

#Missing Value Imputation
library(imputeLCMD)
set.seed(1)
Matrix_Main <- impute.MinDet(Matrix_Main)
set.seed(1)
Matrix_Other <- impute.MinDet(Matrix_Other)

# write.csv(Matrix_Other[,union(which(Label_Other$Oncocytic == "Y"), which(Label_Other$Type == "FT-UMP"))], "PRM/PRM1_Matrix_FT-UMP_Oncocytic.csv")

#Training-Test Split
set.seed(196)
Rep <- which(!is.na(Label_Main$Rep))
TrainingIndex <- sample(1:ncol(Matrix_Main), 0.65 * ncol(Matrix_Main))
TrainingIndex <- union(TrainingIndex, Rep)
TestIndex <- setdiff(1:ncol(Matrix_Main), TrainingIndex)
rm(Rep)

# write.csv(Label_Main[TrainingIndex,], "PRM/MFT_PRM_Label_Train.csv", row.names = FALSE)
# write.csv(Label_Main[TestIndex,], "PRM/MFT_PRM_Label_Test.csv", row.names = FALSE)
# write.csv(Matrix_Main[,TrainingIndex], "PRM/MFT_PRM_ProtMatrix_Train.csv")
# write.csv(Matrix_Main[,TestIndex], "PRM/MFT_PRM_ProtMatrix_Test.csv")

#Differential analysis for training set (t-test)
Matrix_Main <- 2^Matrix_Main
p_value <- vector()
for (i in 1:nrow(Matrix_Main)){
  p_value[i] <- t.test(Matrix_Main[i, intersect(TrainingIndex, which(Label_Main$Type == "FA"))], Matrix_Main[i, intersect(TrainingIndex, which(Label_Main$Type == "FTC"))])$p.value
}
rm(i)
p_value <- p.adjust(p_value, method = "BH")

FC <- vector()
for (i in 1:nrow(Matrix_Main)){
  FC[i] <- mean(t(Matrix_Main[i, intersect(TrainingIndex, which(Label_Main$Type == "FTC"))])) / mean(t(Matrix_Main[i, intersect(TrainingIndex, which(Label_Main$Type == "FA"))]))
}
rm(i)

# write.csv(cbind(rownames(Matrix_Main), p_value, FC), "PRM/MFT_PRM_DEP.csv", row.names = FALSE)

Index1 <- which(p_value < 0.05)
Index2 <- union(which(FC > 1.4), which(FC < 1/1.4))
Index <- intersect(Index1, Index2)

p_value <- data.frame(Protein = rownames(Matrix_Main)[Index], p_value = p_value[Index])
Matrix_Main_All <- as.data.frame(t(log2(Matrix_Main)))
Matrix_Main <- as.data.frame(t(log2(Matrix_Main[Index,])))
Matrix_Other <- as.data.frame(t(Matrix_Other[Index,]))
rm(FC)
rm(Index1)
rm(Index2)
rm(Index)

#Boxplot
library(ggplot2)
library(patchwork)
A <- read.csv("PRM/Gene_Name.csv")
rownames(p_value) <- p_value$Protein
p_value <- p_value[A$Feature,]
Boxplot <- list()
set.seed(1)
for (i in 1:nrow(A)) {
  data <- data.frame(Abundance = Matrix_Main[TrainingIndex, A$Feature[i]], Type = Label_Main$Type[TrainingIndex])
  Boxplot[[i]] <- ggplot(data = data, aes(x = Type, y = Abundance, color = Type)) +
    geom_boxplot(size = 1, outlier.size = 1) +
    geom_jitter(size = 1) +
    theme_classic() +
    scale_color_manual(values = c("#0072B5FF","#BC3C29FF")) +
    scale_x_discrete(limits = c("FA","FTC")) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
    theme(legend.text = element_text(size = 15), legend.title = element_text(size = 15)) +
    theme(plot.title = element_text(size = 15, face = "bold")) +
    labs(y = "Relative abundance", x = paste(A$Feature[i], A$Gene[i], sep = "_")) +
    ggtitle(paste("Adjusted p-value:", signif(p_value$p_value[i], 3), sep = ""))
}
rm(i)
rm(A)
rm(data)
rm(p_value)

Boxplot[[1]] + Boxplot[[2]] + Boxplot[[3]] + Boxplot[[4]] + Boxplot[[5]] + Boxplot[[6]] + Boxplot[[7]] + Boxplot[[8]] +
Boxplot[[9]] + Boxplot[[10]] + Boxplot[[11]] + Boxplot[[12]] + Boxplot[[13]] + Boxplot[[14]] + Boxplot[[15]] + Boxplot[[16]] +
Boxplot[[17]] + Boxplot[[18]] + Boxplot[[19]] + Boxplot[[20]] + Boxplot[[21]] + Boxplot[[22]] + Boxplot[[23]] + Boxplot[[24]] + plot_layout(ncol = 4)
rm(Boxplot)

#XGBoost Tuning

#Data Preparation
set.seed(5)
Data_ML_XG <- as.data.frame(cbind(Label_Main$Type[TrainingIndex], as.data.frame(Matrix_Main[TrainingIndex,])))
colnames(Data_ML_XG)[1] <- "Label"
Data_ML_XG$Label <- as.factor(Data_ML_XG$Label)

#Package Loading
library(mlr3)
library(mlr3verse)
library(mlr3viz)
library(mlr3learners)
library(mlr3tuning)
library(mlr3extralearners)

#Task Creation
Data_ML_XG <- as.data.table(Data_ML_XG, keep.rownames = TRUE)
task_PD_XG <- as_task_classif(Data_ML_XG, target = "Label", positive = "FTC")
task_PD_XG$set_col_roles("rn", roles = "name")
print(task_PD_XG)
print(task_PD_XG$col_roles)

#Learner
length(which(Label_Main[TrainingIndex,]$Type == "FA")) / length(which(Label_Main[TrainingIndex,]$Type == "FTC"))

learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
                   max_depth = 4, subsample = 0.5,
                   scale_pos_weight = 1.321656,
                   colsample_bylevel = 0.9, colsample_bynode = 0.9, colsample_bytree = 0.9, eta = 0.025)
learner_XG$predict_type <- "prob"
learner_XG
learner_XG$param_set

#Search Space
search_space_XG <- ps(
  nrounds = p_int(lower = 70, upper = 130),
  lambda = p_dbl(lower = 0, upper = 0.5),
  alpha = p_dbl(lower = 0, upper = 0.5)
)
search_space_XG

#Evaluation
cv_inner_XG <- rsmp("cv", folds = 5)
measure <- msr("classif.auc")

#Terminator
evals_XG <- trm("evals", n_evals = 20)

#Tuning
instance_XG <- TuningInstanceSingleCrit$new(
  task = task_PD_XG,
  learner = learner_XG,
  resampling = cv_inner_XG,
  measure = measure,
  search_space = search_space_XG,
  terminator = evals_XG
)
tuner_XG <- tnr("random_search")
tuner_XG$optimize(instance_XG)
instance_XG$result_learner_param_vals
#nrounds = 116, lambda = 0.1107656, alpha = 0.4306024

rm(Data_ML_XG)
rm(learner_XG)
rm(search_space_XG)
rm(cv_inner_XG)
rm(measure)
rm(evals_XG)
rm(tuner_XG)
rm(task_PD_XG)

#XGBoost Training & Test 1

#Data Preparation
Data_ML <- as.data.frame(cbind(Label_Main$Type, Matrix_Main))
colnames(Data_ML)[1] <- "Label"
Data_ML$Label <- as.factor(Data_ML$Label)

#Task Creation
Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
task_PD <- as_task_classif(Data_ML, target = "Label", positive = "FTC")
task_PD$set_col_roles("rn", roles = "name")
print(task_PD)
print(task_PD$col_roles)

#XGBoost Learner
learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
                  max_depth = 4, subsample = 0.5,
                  scale_pos_weight = 1.321656,
                  colsample_bylevel = 0.8, colsample_bynode = 0.8, colsample_bytree = 0.8, eta = 0.025,
                  lambda = instance_XG$result_learner_param_vals$lambda, alpha = instance_XG$result_learner_param_vals$alpha, nrounds = instance_XG$result_learner_param_vals$nrounds)
learner_XG$predict_type <- "prob"
set.seed(456)
learner_XG$train(task_PD, row_ids = TrainingIndex)

#Performance on Training Set
prediction_XG1 <- learner_XG$predict(task_PD, row_ids = TrainingIndex)

library(fastR2)

#Prevalence
length(which(Label_Main[TrainingIndex,]$Type == "FTC"))/729
length(which(Label_Main[TrainingIndex,]$Type == "FTC"))

prediction_XG1$confusion

prediction_XG1$score(msr("classif.acc"))
(267 + 379) / 729
267 + 379
wilson.ci(x = 646, n = 729, conf.level = 0.95)

prediction_XG1$score(msr("classif.auc"))
library(pROC)
roc(Label_Main$Type[TrainingIndex], prediction_XG1$prob[,1], ci = TRUE)$ci

prediction_XG1$score(msr("classif.sensitivity"))
267 / (267 + 47)
(267 + 47)
wilson.ci(x = 267, n = 314, conf.level = 0.95)

prediction_XG1$score(msr("classif.specificity"))
379 / (379 + 36)
(379 + 36)
wilson.ci(x = 379, n = 415, conf.level = 0.95)

prediction_XG1$score(msr("classif.ppv"))
267 / (267 + 36)
(267 + 36)
wilson.ci(x = 267, n = 303, conf.level = 0.95)

prediction_XG1$score(msr("classif.npv"))
379 / (379 + 47)
(379 + 47)
wilson.ci(x = 379, n = 426, conf.level = 0.95)

#ROC

#Train
library(pROC)
library(ggplot2)
roc <- roc(Label_Main$Type[TrainingIndex], prediction_XG1$prob[,1], ci = TRUE)
roc$ci
ggroc(roc, colour = "#0072B5FF", size = 4, legacy.axes = TRUE) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  ggtitle("Training ROC") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  annotate("text", x = 0.8, y = 0.1, label = "AUC = 0.958", size = 10, fontface = "bold")
rm(roc)

#Confusion Matrix

#Train
prediction_XG1$confusion
tab1 <- table(Label_Main[TrainingIndex,]$Type, prediction_XG1$response)
tab1 <- as.data.frame(tab1)
ggplot(tab1, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 10) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_blank()) +
  theme(axis.text.x = element_text(size = 15, color = "black"), axis.text.y = element_text(size = 15, color = "black")) +
  scale_fill_gradient(low = "#C6DBEFFF", high = "#3182BDFF") +
  labs(x = "Truth", y = "Prediction", title = "Training confusion matrix", fill = "Count") +
  theme(axis.title.x = element_text(size = 23), axis.title.y = element_text(size = 23)) +
  scale_x_discrete(limits = c("FTC","FA"), expand = c(0,0)) +
  scale_y_discrete(limits = c("FA","FTC"), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, margin = margin(20, 0, 20, 0), face = "bold"),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 20),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 20)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))
rm(tab1)

#Performance on Test Set
prediction_XG2 <- learner_XG$predict(task_PD, row_ids = TestIndex)

#Prevalence
length(which(Label_Main[TestIndex,]$Type == "FTC"))/325
length(which(Label_Main[TestIndex,]$Type == "FTC"))

prediction_XG2$confusion

prediction_XG2$score(msr("classif.acc"))
(90 + 165) / 325
90 + 165
wilson.ci(x = 255, n = 325, conf.level = 0.95)

prediction_XG2$score(msr("classif.auc"))

prediction_XG2$score(msr("classif.sensitivity"))
90 / (90 + 34)
(90 + 34)
wilson.ci(x = 90, n = 124, conf.level = 0.95)

prediction_XG2$score(msr("classif.specificity"))
165 / (165 + 36)
(165 + 36)
wilson.ci(x = 165, n = 201, conf.level = 0.95)

prediction_XG2$score(msr("classif.ppv"))
90 / (90 + 36)
90 + 36
wilson.ci(x = 90, n = 126, conf.level = 0.95)

prediction_XG2$score(msr("classif.npv"))
165 / (165 + 34)
165 + 34
wilson.ci(x = 165, n = 199, conf.level = 0.95)

#ROC

#Test
library(pROC)
library(ggplot2)
roc <- roc(Label_Main$Type[TestIndex], prediction_XG2$prob[,1], ci = TRUE)
roc$ci
ggroc(roc, colour = "#0072B5FF", size = 4, legacy.axes = TRUE) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  ggtitle("Test ROC") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  annotate("text", x = 0.8, y = 0.1, label = "AUC = 0.871", size = 10, fontface = "bold")
rm(roc)

#Confusion Matrix

#Test
prediction_XG2$confusion
tab1 <- table(Label_Main[TestIndex,]$Type, prediction_XG2$response)
tab1 <- as.data.frame(tab1)
ggplot(tab1, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 10) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_blank()) +
  theme(axis.text.x = element_text(size = 15, color = "black"), axis.text.y = element_text(size = 15, color = "black")) +
  scale_fill_gradient(low = "#C6DBEFFF", high = "#3182BDFF") +
  labs(x = "Truth", y = "Prediction", title = "Test confusion matrix", fill = "Count") +
  theme(axis.title.x = element_text(size = 23), axis.title.y = element_text(size = 23)) +
  scale_x_discrete(limits = c("FTC","FA"), expand = c(0,0)) +
  scale_y_discrete(limits = c("FA","FTC"), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, margin = margin(20, 0, 20, 0), face = "bold"),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 20),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 20)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))
rm(tab1)

#Protein Importance
Importance <- as.data.frame(learner_XG$importance())
colnames(Importance) <- "Importance"
Importance_DF <- data.frame(Features = rownames(Importance), Importance = Importance$Importance)
A <- read.csv("PRM/Gene_Name.csv")
Importance_DF$Features <- paste(A$Feature, A$Gene, sep = "_")
library(ggcharts)
bar_chart(Importance_DF, Features, Importance, top_n = 40) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "Feature", y = "Importance")
rm(Importance)
rm(Importance_DF)
rm(A)

rm(Data_ML, prediction_XG2, prediction_XG1, task_PD, learner_XG)


#XGBoost Test 2

#Test 2 Data Loading
Test2Label <- read.csv("D:/Guomics/MFT/PRM/MFT_PRM_2024_combined_pept2prot_quant_log2matrix_206sample_Label20240708.csv")
rownames(Test2Label) <- Test2Label$MatrixReName

Test2Matrix <- read.csv("D:/Guomics/MFT/PRM/MFT_PRM_2024_combined_pept2prot_quant_log2matrix_206sample_20240708.csv")
rownames(Test2Matrix) <- Test2Matrix$X
Test2Matrix <- Test2Matrix[, -1]

Test2Matrix <- as.data.frame(t(Test2Matrix[,rownames(Test2Label)]))
Test2Matrix2 <- Test2Matrix[which(Test2Label$Type == "FT-UMP"),]
Test2Label2 <- Test2Label[which(Test2Label$Type == "FT-UMP"),]
Test2Matrix <- Test2Matrix[which(Test2Label$Type %in% c("FA", "FTC")),]
Test2Label <- Test2Label[which(Test2Label$Type %in% c("FA", "FTC")),]

#Missing Value Imputation
library(imputeLCMD)
set.seed(1)
Test2Matrix <- impute.MinDet(t(Test2Matrix))
Test2Matrix <- as.data.frame(t(Test2Matrix))
set.seed(1)
Test2Matrix2 <- impute.MinDet(t(Test2Matrix2))
Test2Matrix2 <- as.data.frame(t(Test2Matrix2))

#Batch Effects Correction
Matrix_Main2 <- as.data.frame(rbind(Matrix_Main_All[TrainingIndex,], Test2Matrix[,colnames(Matrix_Main_All)]))
library(sva)
Matrix_Main2 <- ComBat(dat = t(Matrix_Main2), batch = c(rep(1, 729), rep(2, 90), rep(3, 70)), mean.only = T, ref.batch = 1)

# write.csv(Test2Label[which(Test2Label$Dataset == "Retro"),], "PRM/Test2Label_Retro.csv", row.names = F)
# write.csv(Matrix_Main2[,729 + which(Test2Label$Dataset == "Retro")], "PRM/Test2Matrix_Retro.csv")
# write.csv(Test2Label[which(Test2Label$Dataset == "pro_fna"),], "PRM/Test2Label_Pros.csv", row.names = F)
# write.csv(Matrix_Main2[,729 + which(Test2Label$Dataset == "pro_fna")], "PRM/Test2Matrix_Pros.csv")

Matrix_Main2 <- as.data.frame(t(Matrix_Main2[colnames(Matrix_Main),]))

#Data Preparation
Data_ML <- as.data.frame(cbind(c(Label_Main[TrainingIndex,]$Type, Test2Label$Type), Matrix_Main2))
colnames(Data_ML)[1] <- "Label"
Data_ML$Label <- as.factor(Data_ML$Label)

#Task Creation
Data_ML <- as.data.table(Data_ML, keep.rownames = TRUE)
task_PD <- as_task_classif(Data_ML, target = "Label", positive = "FTC")
task_PD$set_col_roles("rn", roles = "name")
print(task_PD)
print(task_PD$col_roles)

#XGBoost Learner
learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
                  max_depth = 4, subsample = 0.5,
                  scale_pos_weight = 1.321656,
                  colsample_bylevel = 0.8, colsample_bynode = 0.8, colsample_bytree = 0.8, eta = 0.025,
                  lambda = instance_XG$result_learner_param_vals$lambda, alpha = instance_XG$result_learner_param_vals$alpha, nrounds = instance_XG$result_learner_param_vals$nrounds)
learner_XG$predict_type <- "prob"
set.seed(456)
learner_XG$train(task_PD, row_ids = 1:729)
prediction_XG1 <- learner_XG$predict(task_PD, row_ids = 1:729)
prediction_XG1$score(msr("classif.auc"))
prediction_XG1$score(msr("classif.acc"))
prediction_XG1$score(msr("classif.sensitivity"))
prediction_XG1$score(msr("classif.specificity"))
prediction_XG1$score(msr("classif.ppv"))
prediction_XG1$score(msr("classif.npv"))

#Performance on Test Set 2
Label_Type <- Data_ML$Label

#Retro
prediction_XG3 <- learner_XG$predict(task_PD, row_ids = 729 + which(Test2Label$Dataset == "Retro"))

#Prevalence
prediction_XG3$confusion
(42 + 11)/(42 + 29 + 8 + 11)

prediction_XG3$score(msr("classif.acc"))
(42 + 29) / (42 + 29 + 8 + 11)
(42 + 29)
(42 + 29 + 8 + 11)
wilson.ci(x = 71, n = 90, conf.level = 0.95)

prediction_XG3$score(msr("classif.auc"))

prediction_XG3$score(msr("classif.sensitivity"))
42 / (42 + 11)
(42 + 11)
wilson.ci(x = 42, n = 53, conf.level = 0.95)

prediction_XG3$score(msr("classif.specificity"))
29 / (29 + 8)
(29 + 8)
wilson.ci(x = 29, n = 37, conf.level = 0.95)

prediction_XG3$score(msr("classif.ppv"))
42 / (42 + 8)
(42 + 8)
wilson.ci(x = 42, n = 50, conf.level = 0.95)

prediction_XG3$score(msr("classif.npv"))
29 / (29 + 11)
(29 + 11)
wilson.ci(x = 29, n = 40, conf.level = 0.95)

#ROC

#Test
library(pROC)
library(ggplot2)
roc <- roc(Label_Type[729 + which(Test2Label$Dataset == "Retro")], prediction_XG3$prob[,1], ci = TRUE)
roc$ci
ggroc(roc, colour = "#0072B5FF", size = 4, legacy.axes = TRUE) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  ggtitle("PRM Retro ROC") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  annotate("text", x = 0.8, y = 0.1, label = "AUC = 0.853", size = 10, fontface = "bold")
rm(roc)

#Confusion Matrix

#Test
prediction_XG3$confusion
tab1 <- table(Label_Type[729 + which(Test2Label$Dataset == "Retro")], prediction_XG3$response)
tab1 <- as.data.frame(tab1)
ggplot(tab1, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 10) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_blank()) +
  theme(axis.text.x = element_text(size = 15, color = "black"), axis.text.y = element_text(size = 15, color = "black")) +
  scale_fill_gradient(low = "#C6DBEFFF", high = "#3182BDFF") +
  labs(x = "Truth", y = "Prediction", title = "Retro confusion matrix", fill = "Count") +
  theme(axis.title.x = element_text(size = 23), axis.title.y = element_text(size = 23)) +
  scale_x_discrete(limits = c("FTC", "FA"), expand = c(0,0)) +
  scale_y_discrete(limits = c("FA", "FTC"), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, margin = margin(20, 0, 20, 0), face = "bold"),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 20),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 20)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))
rm(tab1)

#Pros
prediction_XG4 <- learner_XG$predict(task_PD, row_ids = 729 + which(Test2Label$Dataset == "pro_fna"))

#Prevalence
prediction_XG4$confusion
(9 + 2)/(9 + 2 + 15 + 44)

prediction_XG4$score(msr("classif.acc"))
(9 + 44) / (9 + 44 + 2 + 15)
(9 + 44)
(9 + 44 + 2 + 15)
wilson.ci(x = 53, n = 70, conf.level = 0.95)

prediction_XG4$score(msr("classif.auc"))

prediction_XG4$score(msr("classif.sensitivity"))
9 / (9 + 2)
(9 + 2)
wilson.ci(x = 9, n = 11, conf.level = 0.95)

prediction_XG4$score(msr("classif.specificity"))
44 / (44 + 15)
(44 + 15)
wilson.ci(x = 44, n = 59, conf.level = 0.95)

prediction_XG4$score(msr("classif.ppv"))
9 / (9 + 15)
(9 + 15)
wilson.ci(x = 9, n = 24, conf.level = 0.95)

prediction_XG4$score(msr("classif.npv"))
44 / (44 + 2)
(44 + 2)
wilson.ci(x = 44, n = 46, conf.level = 0.95)

#ROC

#Test
library(pROC)
library(ggplot2)
roc <- roc(Label_Type[729 + which(Test2Label$Dataset == "pro_fna")], prediction_XG4$prob[,1], ci = TRUE)
roc$ci
ggroc(roc, colour = "#0072B5FF", size = 4, legacy.axes = TRUE) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  ggtitle("PRM Pros ROC") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  annotate("text", x = 0.8, y = 0.1, label = "AUC = 0.781", size = 10, fontface = "bold")
rm(roc)

#Confusion Matrix

#Test
prediction_XG4$confusion
tab1 <- table(Label_Type[729 + which(Test2Label$Dataset == "pro_fna")], prediction_XG4$response)
tab1 <- as.data.frame(tab1)
ggplot(tab1, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 10) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_blank()) +
  theme(axis.text.x = element_text(size = 15, color = "black"), axis.text.y = element_text(size = 15, color = "black")) +
  scale_fill_gradient(low = "#C6DBEFFF", high = "#3182BDFF") +
  labs(x = "Truth", y = "Prediction", title = "Pros confusion matrix", fill = "Count") +
  theme(axis.title.x = element_text(size = 23), axis.title.y = element_text(size = 23)) +
  scale_x_discrete(limits = c("FTC", "FA"), expand = c(0,0)) +
  scale_y_discrete(limits = c("FA", "FTC"), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, margin = margin(20, 0, 20, 0), face = "bold"),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 20),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 20)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))
rm(tab1)

rm(Data_ML, prediction_XG3, prediction_XG4, prediction_XG1, task_PD, learner_XG)
rm(Label_Type)


#Remove All
rm(list = ls())
gc()
