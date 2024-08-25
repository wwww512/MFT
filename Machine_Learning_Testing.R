#Machine Learning Testing

#Package Loading
library(mlr3)
library(mlr3verse)
library(mlr3viz)
library(mlr3learners)
library(mlr3tuning)
library(mlr3extralearners)
library(ggplot2)

# #XG Tuning
# 
# #Data Preparation
# set.seed(15)
# Data_ML_XG <- as.data.frame(cbind(as.factor(Label$Group), Ratio_Matrix_R2[,Protein_XG]))
# colnames(Data_ML_XG)[1] <- "Label"
# Data_ML_XG$Label <- as.factor(Data_ML_XG$Label)
# 
# #Task Creation
# Data_ML_XG <- as.data.table(Data_ML_XG, keep.rownames = TRUE)
# task_PD_XG <- as_task_classif(Data_ML_XG, target = "Label", positive = "C")
# task_PD_XG$set_col_roles("rn", roles = "name")
# print(task_PD_XG)
# print(task_PD_XG$col_roles)
# 
# #Learner
# learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                   max_depth = 1, subsample = 0.5,
#                   colsample_bylevel = 0.9, colsample_bynode = 0.9, colsample_bytree = 0.9, eta = 0.025)
# learner_XG$predict_type <- "prob"
# learner_XG
# learner_XG$param_set
# 
# #SearchSpace
# search_space_XG <- ps(
#   nrounds = p_int(lower = 80, upper = 120),
#   lambda = p_dbl(lower = 0, upper = 0.5),
#   alpha = p_dbl(lower = 0, upper = 0.5)
# )
# search_space_XG
# 
# #Evaluation
# cv_inner_XG <- rsmp("cv", folds = 5)
# measure <- msr("classif.auc")
# 
# #Terminator
# evals_XG <- trm("evals", n_evals = 20)
# 
# #Tuning
# instance_XG <- TuningInstanceSingleCrit$new(
#   task = task_PD_XG,
#   learner = learner_XG,
#   resampling = cv_inner_XG,
#   measure = measure,
#   search_space = search_space_XG,
#   terminator = evals_XG
# )
# tuner_XG <- tnr("random_search")
# tuner_XG$optimize(instance_XG)
# instance_XG$result_learner_param_vals
# #Best lambda = 0.4588493, alpha = 0.354727, nrounds = 106

# #CV
# ValidationAUC <- 1:1000
# ValidationACC <- 1:1000
# ValidationSEN <- 1:1000
# ValidationSPE <- 1:1000
# ValidationPPV <- 1:1000
# ValidationNPV <- 1:1000
# for (i in 1:1000) {
#   set.seed(i)
#   learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
#                     max_depth = 1, subsample = 0.5,
#                     colsample_bylevel = 0.9, colsample_bynode = 0.9, colsample_bytree = 0.9, eta = 0.025,
#                     lambda = 0.4588493, alpha = 0.354727, nrounds = 106)
#   learner_XG$predict_type <- "prob"
#   learner_XG
#   learner_XG$param_set
#   rr <- resample(task_PD_XG, learner_XG, rsmp("cv", folds = 5))
#   ValidationAUC[i] <- rr$aggregate(msr("classif.auc"))
#   ValidationACC[i] <- rr$aggregate(msr("classif.acc"))
#   ValidationSEN[i] <- rr$aggregate(msr("classif.sensitivity"))
#   ValidationSPE[i] <- rr$aggregate(msr("classif.specificity"))
#   ValidationPPV[i] <- rr$aggregate(msr("classif.ppv"))
#   ValidationNPV[i] <- rr$aggregate(msr("classif.npv"))
# }
# rm(i)
# ValidationMeasures <- data.frame(ValidationAUC, ValidationACC, ValidationSEN, ValidationSPE, ValidationPPV, ValidationNPV)
# 
# write.csv(ValidationMeasures, "Machine_Learning_Testing/ValidationMeasures.csv")
# #Delete the first row and first col

ValidationMeasures <- read.csv("Machine_Learning_Testing/ValidationMeasures.csv", header = F)
for (i in 1:6) {
  print(mean(ValidationMeasures[,i]) - 1.96 * sd(ValidationMeasures[,i]))
  print(mean(ValidationMeasures[,i]) + 1.96 * sd(ValidationMeasures[,i]))
}
rm(i)
rm(ValidationMeasures)

set.seed(592)
Data_ML_XG <- as.data.frame(cbind(Label$Group, Ratio_Matrix_R2[,Protein_XG]))
colnames(Data_ML_XG)[1] <- "Label"
Data_ML_XG$Label <- as.factor(Data_ML_XG$Label)
Data_ML_XG <- as.data.table(Data_ML_XG, keep.rownames = TRUE)
task_PD_XG <- as_task_classif(Data_ML_XG, target = "Label", positive = "C")
task_PD_XG$set_col_roles("rn", roles = "name")
print(task_PD_XG)
print(task_PD_XG$col_roles)
learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
                 max_depth = 1, subsample = 0.5,
                 colsample_bylevel = 0.9, colsample_bynode = 0.9, colsample_bytree = 0.9, eta = 0.025,
                 lambda = 0.4588493, alpha = 0.354727, nrounds = 106)
learner_XG$predict_type <- "prob"
learner_XG
learner_XG$param_set
rr <- resample(task_PD_XG, learner_XG, rsmp("cv", folds = 5))
rr$aggregate(msr("classif.auc"))
#0.9045685
rr$aggregate(msr("classif.acc"))
#0.828866
rr$aggregate(msr("classif.sensitivity"))
#0.7668665
rr$aggregate(msr("classif.specificity"))
#0.8897869
rr$aggregate(msr("classif.ppv"))
#0.8551923
rr$aggregate(msr("classif.npv"))
#0.8097304
V <- rr$predictions()

library(pROC)
rr$score(msr("classif.auc"))

validation_prob <- c(V[[1]]$prob[,2])
validation_rowids <- c(V[[1]]$row_ids)
roc_v1 <- roc(Label$Group[validation_rowids], validation_prob, direction = ">")
roc_v1$auc

validation_prob <- c(V[[2]]$prob[,2])
validation_rowids <- c(V[[2]]$row_ids)
roc_v2 <- roc(Label$Group[validation_rowids], validation_prob, direction = ">")
roc_v2$auc

validation_prob <- c(V[[3]]$prob[,2])
validation_rowids <- c(V[[3]]$row_ids)
roc_v3 <- roc(Label$Group[validation_rowids], validation_prob, direction = ">")
roc_v3$auc

validation_prob <- c(V[[4]]$prob[,2])
validation_rowids <- c(V[[4]]$row_ids)
roc_v4 <- roc(Label$Group[validation_rowids], validation_prob, direction = ">")
roc_v4$auc

validation_prob <- c(V[[5]]$prob[,2])
validation_rowids <- c(V[[5]]$row_ids)
roc_v5 <- roc(Label$Group[validation_rowids], validation_prob, direction = ">")
roc_v5$auc

f <- function(x, revthresholds, revsensitivity, revspecificity)
{
  se <- vector()
  sp <- vector()
  for (i in 1:length(x)) {
    for (j in 1:(length(revthresholds) - 1)) {
      if((x[i] >= revthresholds[j]) & (x[i] < revthresholds[j + 1])){
        se[i] <- revsensitivity[j]
        sp[i] <- revspecificity[j]
      }
    }
  }
  return(list(se, 1 - sp))
}
#Input: 1 your cutoffs
#2 thresholds 3 sensitivities 4 specificities
#234 generated by roc function and need to be reversed
#Output: a list containing sensitivities and 1-specificities corresponding to your cutoffs

V1 <- f(seq(0, 1, length.out = 100000), rev(roc_v1$thresholds), rev(roc_v1$sensitivities), rev(roc_v1$specificities))
V2 <- f(seq(0, 1, length.out = 100000), rev(roc_v2$thresholds), rev(roc_v2$sensitivities), rev(roc_v2$specificities))
V3 <- f(seq(0, 1, length.out = 100000), rev(roc_v3$thresholds), rev(roc_v3$sensitivities), rev(roc_v3$specificities))
V4 <- f(seq(0, 1, length.out = 100000), rev(roc_v4$thresholds), rev(roc_v4$sensitivities), rev(roc_v4$specificities))
V5 <- f(seq(0, 1, length.out = 100000), rev(roc_v5$thresholds), rev(roc_v5$sensitivities), rev(roc_v5$specificities))

oneminussp <- data.frame(V1[[2]], V2[[2]], V3[[2]], V4[[2]], V5[[2]])
se <- data.frame(V1[[1]], V2[[1]], V3[[1]], V4[[1]], V5[[1]])
oneminussp <- as.numeric(apply(oneminussp, 1, mean))
se <- as.numeric(apply(se, 1, mean))

ggplot(mapping = aes(x = oneminussp, y = se)) +
  geom_line(colour = "#BC3C29FF", size = 4) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  ggtitle("CV ROC") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate("text", x = 0.8, y = 0.1, label = "AUC = 0.905", size = 10, fontface = "bold")

rm(learner_XG)
rm(rr)
rm(V)
rm(validation_prob)
rm(validation_rowids)
rm(roc_v1)
rm(roc_v2)
rm(roc_v3)
rm(roc_v4)
rm(roc_v5)
rm(Data_ML_XG)
rm(task_PD_XG)
rm(V1, V2, V3, V4, V5, f, se, oneminussp)

#Test

#Data Preparation
Label_All <- as.data.frame(rbind(Label, Label_New))
rm(Label_New)
Ratio_Matrix_R2_All <- as.data.frame(rbind(Ratio_Matrix_R2, Ratio_Matrix_New_impseqrob_combat))
rm(Ratio_Matrix_New_impseqrob_combat)
SampleName_All <- c(SampleName, SampleName_New)
rm(SampleName_New)

Data_ML_Test <- as.data.frame(cbind(Label_All$Group, Ratio_Matrix_R2_All[,Protein_XG]))
colnames(Data_ML_Test)[1] <- "Label"
Data_ML_Test$Label <- as.factor(Data_ML_Test$Label)
Data_ML_Test <- as.data.table(Data_ML_Test, keep.rownames = TRUE)
task_PD_test <- as_task_classif(Data_ML_Test, target = "Label", positive = "C")
task_PD_test$set_col_roles("rn", roles = "name")
print(task_PD_test)
print(task_PD_test$col_roles)

#Test
learner_XG <- lrn("classif.xgboost", objective = "binary:logistic", nthread = 16, verbose = 0,
                      max_depth = 1, subsample = 0.5,
                      colsample_bylevel = 0.9, colsample_bynode = 0.9, colsample_bytree = 0.9, eta = 0.025,
                      lambda = 0.4588493, alpha = 0.354727, nrounds = 106)
learner_XG$predict_type <- "prob"
set.seed(7187)
learner_XG$train(task_PD_test, row_ids = 1:485)

#Performance on Training Set
prediction_XG1 <- learner_XG$predict(task_PD_test, row_ids = 1:485)
prediction_XG1$set_threshold(0.5)

train_prob <- prediction_XG1$prob[,1]
Label_Train <- Label_All[c(1:485),]
(C_Index <- which(train_prob >= 0.5))
Label_Train$Group[C_Index]
C_Index2 <- data.frame(CIndex = C_Index, Label = Label_Train$Group[C_Index])
WrongIndex1 <- C_Index2[which(C_Index2$Label == "A"),1]
(A_Index <- which(train_prob < 0.5))
Label_Train$Group[A_Index]
A_Index2 <- data.frame(AIndex = A_Index, Label = Label_Train$Group[A_Index])
WrongIndex2 <- A_Index2[which(A_Index2$Label == "C"),1]
WrongIndex <- sort(c(WrongIndex1,WrongIndex2))
SampleName_Train <- SampleName_All[1:485]
SampleName_Train[WrongIndex]
# write.csv(SampleName_Train[WrongIndex], "Machine_Learning_Testing/SampleIndex_Train_Wrong.csv")
#Delete the first row and first col

Data <- data.frame(Probability = train_prob,Index = c(1:485), Label = Label_Train$Group)
ggplot(Data, aes(x = Probability, y = Index, fill = Label, color = Label)) +
  geom_point(size = 4) +
  theme_classic() +
  scale_color_manual(values = c("#0072B5FF", "#BC3C29FF")) +
  theme(axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "Predicted probability", y = "Sample index") +
  ggtitle("Training set") +
  theme(plot.title = element_text(size = 22.5, face = "bold")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", size = 1)

ggplot(Data, aes(x = Probability, fill = Label, color = Label)) +
  geom_histogram(bins = 20, alpha = 0.9, position = "identity", size = 1.5) +
  scale_color_manual(values = c("#0072B5FF", "#BC3C29FF")) +
  scale_fill_manual(values = c("#0072B5FF", "#BC3C29FF")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "Predicted probability", y = "Frequency") +
  ggtitle("Training set") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  geom_vline(xintercept = 0.5,linetype ="dashed", color = "black", size = 1)

prediction_XG1$confusion

library(fastR2)
length(which(Label$Group == "C"))/485
length(which(Label$Group == "C"))

tab1 <- table(Label$Group, prediction_XG1$response)
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
  scale_x_discrete(limits = c("C", "A"), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "C"), expand = c(0, 0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, margin = margin(20, 0, 20, 0), face = "bold"),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 20),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 20)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))
rm(tab1)

prediction_XG1$score(msr("classif.acc"))
180 + 242
wilson.ci(x = 422, n = 485, conf.level = 0.95)

prediction_XG1$score(msr("classif.auc"))

prediction_XG1$score(msr("classif.sensitivity"))
180 / (180 + 44)
(180 + 44)
wilson.ci(x = 180, n = 224, conf.level = 0.95)

prediction_XG1$score(msr("classif.specificity"))
242 / (242 + 19)
(242 + 19)
wilson.ci(x = 242, n = 261, conf.level = 0.95)

prediction_XG1$score(msr("classif.ppv"))
180 / (180 + 19)
(180 + 19)
wilson.ci(x = 180, n = 199, conf.level = 0.95)

prediction_XG1$score(msr("classif.npv"))
242 / (242 + 44)
(242 + 44)
wilson.ci(x = 242, n = 286, conf.level = 0.95)

library(pROC)
roc1 <- roc(Label$Group, train_prob, ci = TRUE)
roc1$ci
ggroc(roc1, colour = "#BC3C29FF", size = 4, legacy.axes = TRUE) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  ggtitle("Training ROC") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  annotate("text", x = 0.8, y = 0.1, label = "AUC = 0.953", size = 10, fontface = "bold")
rm(roc1)
rm(train_prob)
rm(prediction_XG1)
rm(Data)
rm(WrongIndex)
rm(WrongIndex1)
rm(WrongIndex2)
rm(A_Index)
rm(C_Index)
rm(A_Index2)
rm(C_Index2)
rm(SampleName_Train)
rm(Label_Train)

#Performance on Test Set
prediction_XG2 <- learner_XG$predict(task_PD_test,row_ids = 486:620)
prediction_XG2$prob
prediction_XG2$set_threshold(0.5)
test_prob <- prediction_XG2$prob[,1]
Label_New <- Label_All[c(486:620),]
(C_Index <- which(test_prob > 0.5))
Label_New$Group[C_Index]
C_Index2 <- data.frame(CIndex = C_Index, Label = Label_New$Group[C_Index])
WrongIndex1 <- C_Index2[which(C_Index2$Label == "A"),1]
(A_Index <- which(test_prob < 0.5))
Label_New$Group[A_Index]
A_Index2 <- data.frame(AIndex = A_Index, Label = Label_New$Group[A_Index])
WrongIndex2 <- A_Index2[which(A_Index2$Label == "C"),1]
WrongIndex <- sort(c(WrongIndex1, WrongIndex2))
SampleName_New <- SampleName_All[486:620]
SampleName_New[WrongIndex]
# write.csv(SampleName_New[WrongIndex], "Machine_Learning_Testing/SampleIndex_Test_Wrong.csv")
#Delete the first row and first col

Data <- data.frame(Probability = test_prob, Index = c(1:135), Label = Label_New$Group)
ggplot(Data, aes(x = Probability, y = Index, fill = Label, color = Label)) +
  geom_point(size = 4) +
  theme_classic() +
  scale_color_manual(values = c("#0072B5FF", "#BC3C29FF")) +
  theme(axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "Predicted probability", y = "Sample index") +
  ggtitle("Test set") +
  theme(plot.title = element_text(size = 22.5, face = "bold")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", size = 1)

ggplot(Data, aes(x = Probability, fill = Label,color = Label)) +
  geom_histogram(bins = 20, alpha = 0.9, position = "identity", size = 1.5) +
  scale_color_manual(values = c("#0072B5FF", "#BC3C29FF")) +
  scale_fill_manual(values = c("#0072B5FF", "#BC3C29FF")) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "Predicted probability", y = "Frequency") +
  ggtitle("Test set") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", size = 1)

rm(Data)
rm(WrongIndex)
rm(WrongIndex1)
rm(WrongIndex2)
rm(A_Index)
rm(C_Index)
rm(A_Index2)
rm(C_Index2)
rm(SampleName_New)

prediction_XG2$confusion

tab2 <- table(Label_New$Group, prediction_XG2$response)
tab2 <- as.data.frame(tab2)
ggplot(tab2, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 10) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_blank()) +
  theme(axis.text.x = element_text(size = 15, color = "black"), axis.text.y = element_text(size = 15, color = "black")) +
  scale_fill_gradient(low = "#C6DBEFFF", high = "#3182BDFF") +
  labs(x = "Truth", y = "Prediction", title = "Test confusion matrix", fill = "Count") +
  theme(axis.title.x = element_text(size = 23), axis.title.y = element_text(size = 23)) +
  scale_x_discrete(limits = c("C", "A"), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "C"), expand = c(0, 0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5, margin = margin(20, 0, 20, 0), face = "bold"),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 20),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 20)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))
rm(tab2)

#Prevalence
length(which(Label_New$Group == "C"))/135
length(which(Label_New$Group == "C"))

prediction_XG2$score(msr("classif.acc"))
52 + 59
wilson.ci(x = 111, n = 135, conf.level = 0.95)

prediction_XG2$score(msr("classif.auc"))

prediction_XG2$score(msr("classif.sensitivity"))
52 / (52 + 13)
(52 + 13)
wilson.ci(x = 52, n = 65, conf.level = 0.95)

prediction_XG2$score(msr("classif.specificity"))
59 / (59 + 11)
(59 + 11)
wilson.ci(x = 59, n = 70, conf.level = 0.95)

prediction_XG2$score(msr("classif.ppv"))#Precision
52 / (52 + 11)
52 + 11
wilson.ci(x = 52, n = 63, conf.level = 0.95)

prediction_XG2$score(msr("classif.npv"))
59 / (59 + 13)
59 + 13
wilson.ci(x = 59, n = 72, conf.level = 0.95)

roc2 <- roc(Label_New$Group, test_prob, ci = TRUE)
roc2$ci
ggroc(roc2, colour = "#BC3C29FF", size = 4, legacy.axes = TRUE) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme(plot.title = element_text(size = 25, face = "bold")) +
  ggtitle("Test ROC") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  annotate("text", x = 0.8, y = 0.1, label = "AUC = 0.899", size = 10, fontface = "bold")

rm(roc2)
rm(test_prob)
rm(prediction_XG2)
rm(Label_New)
