#Model Interpretation and Validation

#Protein Importance
Importance <- as.data.frame(learner_XG$importance())
colnames(Importance) <- "Importance"
Protein_Gene <- read.csv("Data_Acquisition/Protein_Gene.csv", header = T)
rownames(Protein_Gene) <- Protein_Gene$Index
Protein_Gene <- Protein_Gene[rownames(Importance),]
Names <- paste0(Protein_Gene[,1], "_")
Names <- paste0(Names, Protein_Gene[,2])
Importance_DF <- data.frame(Proteins = Names, Importance = Importance)
library(ggcharts)
bar_chart(Importance_DF, Proteins, Importance, top_n = 25) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 14, color = "black")) +
  labs(x = "Protein_Gene", y = "Importance")
rm(Importance)
rm(Names)
rm(Importance_DF)
rm(Protein_Gene)

#Remove All
rm(list = ls())
gc()
