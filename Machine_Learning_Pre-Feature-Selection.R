#Machine Learning Pre-Feature-Selection

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

#Pre-Feature-Selection
filter_anova <- flt("anova")
filter_anova$calculate(task_PD)
filter_anova$scores
FS_anova <- as.data.frame(filter_anova$scores)
FSR_anova <- rownames(FS_anova)[which(FS_anova <= -log10(0.001))]

filter_kt <- flt("kruskal_test")
filter_kt$calculate(task_PD)
filter_kt$scores
FS_kt <- as.data.frame(filter_kt$scores)
FSR_kt <- rownames(FS_kt)[which(FS_kt <= -log10(0.001))]

library(FSelectorRcpp)
filter_ig <- flt("information_gain")
filter_ig$calculate(task_PD)
filter_ig$scores
FS_ig <- as.data.frame(filter_ig$scores)
FSR_ig <- rownames(FS_ig)[which(FS_ig == 0)]

#Remove Features
FSR <- union(union(FSR_anova, FSR_kt), FSR_ig)
MissingRate_Protein_R1 <- as.data.frame(MissingRate_Protein_R1)
rownames(MissingRate_Protein_R1) <- Protein_Index_R2_retain
Protein_Index_R2_retain <- setdiff(Protein_Index_R2_retain, FSR)

rm(task_PD, Data_ML, FSR_kt, FSR_ig, FSR_anova, FSR, filter_ig, filter_anova, filter_kt, FS_ig, FS_anova, FS_kt)

Ratio_Data_New_impseqrob_combat <- Ratio_Data_New_impseqrob_combat[Protein_Index_R2_retain,]
Ratio_Data_R2 <- Ratio_Data_R2[Protein_Index_R2_retain,]
Ratio_Matrix_New_impseqrob_combat <- Ratio_Matrix_New_impseqrob_combat[,Protein_Index_R2_retain]
Ratio_Matrix_R2 <- Ratio_Matrix_R2[,Protein_Index_R2_retain]
MissingRate_Protein_R1 <- MissingRate_Protein_R1[Protein_Index_R2_retain,]

# write.csv(Protein_Index_R2_retain, "Machine_Learning_Pre-Feature-Selection/126PredictiveProteins.csv")
#Delete the first row and the first col
