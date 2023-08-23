library(topicmodels)
library(quanteda)
library(tidyr)
library(dplyr)
library(tidytext)
library(NLP)
library(tm)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(wordcloud)
library(lda)
library(stringr)
library(text2vec)
library(Matrix)
library(patchwork)
data<- read.csv("Label-titles-abstracts-tokenized-filtered.csv")
data$words <- gsub("\\\\", "", data$words)
data$words <- gsub("\"", "", data$words)
data$words <- gsub("\"", "", data$words)
data$words <- str_replace_all(data$words, "(\\\\|\")", "")
data$FundingBody <- gsub("Innovate-UK", 3, data$FundingBody)
data$FundingBody <- gsub("BBSRC", 5, data$FundingBody)
data$FundingBody <- gsub("NERC", 1, data$FundingBody)
data$FundingBody <- gsub("STFC", 4, data$FundingBody)
data$FundingBody <- gsub("EPSRC", 2, data$FundingBody)

train_indices<-readRDS("train_indices.rds")
train_set <- data[train_indices, ]  # 训练集的文档-词频矩阵
train_corpus <- corpus(train_set$words)
train_tokens<- tokens(train_corpus)
train_dtm <- dfm(train_tokens)

validation_indices <- setdiff(1:nrow(data), train_indices)  # 剩下的样本作为验证集
validation_set <- data[validation_indices, ]  # 验证集的文档-词频矩阵
validation_corpus <- corpus(validation_set$words)
validation_tokens<- tokens(validation_corpus)
validation_dtm <- dfm(validation_tokens)

train_sparse <- as(train_dtm, "sparseMatrix")
tcm<-crossprod(sign(train_sparse))


lda_logratio <- c()
lda_pmi<-c()
lda_difference<- c()
lda_cosim<- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("lda_out", k, ".rds")
  
  lda_model <- readRDS(filename)
  Terms<-terms(lda_model, 20)
  Coherence<-coherence(Terms, tcm, metrics = c("mean_logratio", "mean_pmi", "mean_npmi",
                                                 "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                      smooth = 1e-12, n_doc_tcm = k)
  logratio<-sum(Coherence[,1], na.rm = TRUE)
  pmi<-sum(Coherence[,2], na.rm = TRUE)
  difference<-sum(Coherence[,4], na.rm = TRUE)
  cosim<-sum(Coherence[,5], na.rm = TRUE)
  lda_logratio <- c(lda_logratio, logratio)
  lda_pmi<-c(lda_pmi,pmi)
  lda_difference<- c(lda_difference,difference)
  lda_cosim<- c(lda_cosim,cosim)
  
}



slda_logratio<-c()
slda_pmi<-c()
slda_difference<- c()
slda_cosim<- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("slda_mod", k, ".rds")
  slda_model <- readRDS(filename)
  
  Sterms<-slda_model$topics %>% top.topic.words(20, by.score = TRUE)
  Scoherence<-coherence(Sterms, tcm, metrics = c("mean_logratio", "mean_pmi", "mean_npmi",
                                               "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                       smooth = 1e-12, n_doc_tcm = k)
  slogratio<-sum(Scoherence[,1], na.rm = TRUE)
  spmi<-sum(Scoherence[,2], na.rm = TRUE)
  sdifference<-sum(Scoherence[,4], na.rm = TRUE)
  scosim<-sum(Scoherence[,5], na.rm = TRUE)
  slda_logratio <- c(slda_logratio, slogratio)
  slda_pmi<-c(slda_pmi,spmi)
  slda_difference<- c(slda_difference,sdifference)
  slda_cosim<- c(slda_cosim,scosim)
}

Durslda_logratio<-c()
Durslda_pmi<-c()
Durslda_difference<- c()
Durslda_cosim<- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("Durslda_mod", k, ".rds")
  slda_model <- readRDS(filename)
  
  Sterms<-slda_model$topics %>% top.topic.words(20, by.score = TRUE)
  Scoherence<-coherence(Sterms, tcm, metrics = c("mean_logratio", "mean_pmi", "mean_npmi",
                                                 "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                        smooth = 1e-12, n_doc_tcm = k)
  slogratio<-sum(Scoherence[,1], na.rm = TRUE)
  spmi<-sum(Scoherence[,2], na.rm = TRUE)
  sdifference<-sum(Scoherence[,4], na.rm = TRUE)
  scosim<-sum(Scoherence[,5], na.rm = TRUE)
  Durslda_logratio <- c(Durslda_logratio, slogratio)
  Durslda_pmi<-c(Durslda_pmi,spmi)
  Durslda_difference<- c(Durslda_difference,sdifference)
  Durslda_cosim<- c(Durslda_cosim,scosim)
  
  
}

FPslda_logratio<-c()
FPslda_pmi<-c()
FPslda_difference<- c()
FPslda_cosim<- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("FPslda_mod", k, ".rds")
  slda_model <- readRDS(filename)
  
  Sterms<-slda_model$topics %>% top.topic.words(20, by.score = TRUE)
  Scoherence<-coherence(Sterms, tcm, metrics = c("mean_logratio", "mean_pmi", "mean_npmi",
                                                 "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                        smooth = 1e-12, n_doc_tcm = k)
  slogratio<-sum(Scoherence[,1], na.rm = TRUE)
  spmi<-sum(Scoherence[,2], na.rm = TRUE)
  sdifference<-sum(Scoherence[,4], na.rm = TRUE)
  scosim<-sum(Scoherence[,5], na.rm = TRUE)
  FPslda_logratio <- c(FPslda_logratio, slogratio)
  FPslda_pmi<-c(FPslda_pmi,spmi)
  FPslda_difference<- c(FPslda_difference, sdifference)
  FPslda_cosim<- c(FPslda_cosim,scosim)
}

FAslda_logratio<-c()
FAslda_pmi<-c()
FAslda_difference<- c()
FAslda_cosim<- c()
for (k in seq(50, 500, 25)) {
  filename <- paste0("FAslda_mod", k, ".rds")
  slda_model <- readRDS(filename)
  
  Sterms<-slda_model$topics %>% top.topic.words(20, by.score = TRUE)
  Scoherence<-coherence(Sterms, tcm, metrics = c("mean_logratio", "mean_pmi", "mean_npmi",
                                                 "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                        smooth = 1e-12, n_doc_tcm = k)
  slogratio<-sum(Scoherence[,1], na.rm = TRUE)
  spmi<-sum(Scoherence[,2], na.rm = TRUE)
  sdifference<-sum(Scoherence[,4], na.rm = TRUE)
  scosim<-sum(Scoherence[,5], na.rm = TRUE)
  FAslda_logratio <- c(FAslda_logratio, slogratio)
  FAslda_pmi<-c(FAslda_pmi,spmi)
  FAslda_difference<- c(FAslda_difference, sdifference)
  FAslda_cosim<- c(FAslda_cosim,scosim)
}

logratioDF <- data.frame(k = seq(50, 500, 25),
                         lda_logratio, slda_logratio,Durslda_logratio,
                         FPslda_logratio,FAslda_logratio)
pmiDF <- data.frame(k = seq(50, 500, 25),
                         lda_pmi, slda_pmi,Durslda_pmi,
                         FPslda_pmi,FAslda_pmi)
differenceDF <- data.frame(k = seq(50, 500, 25),
                    lda_difference, slda_difference,Durslda_difference,
                    FPslda_difference,FAslda_difference)
cosimDF <- data.frame(k = seq(50, 500, 25),
                           lda_cosim, slda_cosim,Durslda_cosim,
                           FPslda_cosim,FAslda_cosim)
write.csv(logratioDF,"logratio.csv")
write.csv(pmiDF,"pmi.csv")
write.csv(differenceDF,"difference.csv")
write.csv(cosimDF,"cosim.csv")



logratioPlot <- ggplot(data = logratioDF, aes(x = k)) +
  geom_line(aes(y = lda_logratio, color = "lda"), linetype = "solid") +
  geom_point(aes(y = lda_logratio, color = "lda"), shape = 16, size = 3) +
  geom_line(aes(y = slda_logratio, color = "slda"), linetype = "solid") +
  geom_point(aes(y = slda_logratio, color = "slda"), shape = 16, size = 3) +
  geom_line(aes(y = Durslda_logratio, color = "Durslda"), linetype = "solid") +
  geom_point(aes(y = Durslda_logratio, color = "Durslda"), shape = 16, size = 3) +
  geom_line(aes(y = FPslda_logratio, color = "FPslda"), linetype = "solid") +
  geom_point(aes(y = FPslda_logratio, color = "FPslda"), shape = 16, size = 3) +
  geom_line(aes(y = FAslda_logratio, color = "FAslda"), linetype = "solid") +
  geom_point(aes(y = FAslda_logratio, color = "FAslda"), shape = 16, size = 3) +
  labs(x = "Number of Topics", y = "Logratio") +
  theme_minimal() +
  scale_color_manual(values = c("lda" = "black", "slda" = "#757fa0", "Durslda" = "#DDacaa", "FPslda" = "#F2a869", "FAslda" = "#b783bd")) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )


pmiPlot<-ggplot(data=pmiDF, aes(x=k,y=lda_pmi))+
  geom_line(aes(y = lda_pmi), linetype = "solid")+
  geom_point(aes(y = lda_pmi), shape = 16, size = 3)+
  geom_line(aes(x = k, y = slda_pmi),color = "#757fa0", linetype = "solid") +
  geom_point(aes(x = k, y = slda_pmi),shape = 16, size = 3,color = "#757fa0")+
  geom_line( aes(x = k, y = Durslda_pmi), color = "#DDacaa", linetype = "solid") +  
  geom_point( aes(x = k, y = Durslda_pmi), shape = 16, size = 3, color = "#DDacaa")+
  geom_line(aes(x = k, y = FPslda_pmi), color = "#F2a869", linetype = "solid") +  # Dursdf 数据的线条
  geom_point( aes(x = k, y = FPslda_pmi), shape = 16, size = 3, color = "#F2a869")+
  geom_line( aes(x = k, y = FAslda_pmi), color = "#b783bd", linetype = "solid") +  # Dursdf 数据的线条
  geom_point(aes(x = k, y = FAslda_pmi), shape = 16, size = 3, color = "#b783bd")+
  labs(x = "Number of Topics", y = "PMI") +
  theme_minimal() +
  scale_color_manual(values = c("lda" = "black", "slda" = "#757fa0", "Durslda" = "#DDacaa", "FPslda" = "#F2a869", "FAslda" = "#b783bd")) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )

differencePlot<-ggplot(data=differenceDF, aes(x=k,y=lda_difference))+
  geom_line(aes(y = lda_difference), linetype = "solid")+
  geom_point(aes(y = lda_difference), shape = 16, size = 3)+
  geom_line(aes(x = k, y = slda_difference),color = "#757fa0", linetype = "solid") +
  geom_point(aes(x = k, y = slda_difference),shape = 16, size = 3,color = "#757fa0")+
  geom_line( aes(x = k, y = Durslda_difference), color = "#DDacaa", linetype = "solid") +  
  geom_point( aes(x = k, y = Durslda_difference), shape = 16, size = 3, color = "#DDacaa")+
  geom_line(aes(x = k, y = FPslda_difference), color = "#F2a869", linetype = "solid") +  # Dursdf 数据的线条
  geom_point( aes(x = k, y = FPslda_difference), shape = 16, size = 3, color = "#F2a869")+
  geom_line( aes(x = k, y = FAslda_difference), color = "#b783bd", linetype = "solid") +  # Dursdf 数据的线条
  geom_point(aes(x = k, y = FAslda_difference), shape = 16, size = 3, color = "#b783bd")+
  labs(x = "Number of Topics", y = "Difference") +
  theme_minimal() +
  scale_color_manual(values = c("lda" = "black", "slda" = "#757fa0", "Durslda" = "#DDacaa", "FPslda" = "#F2a869", "FAslda" = "#b783bd")) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )

cosimPlot<-ggplot(data=cosimDF, aes(x=k,y=lda_cosim))+
  geom_line(aes(y = lda_cosim), linetype = "solid")+
  geom_point(aes(y = lda_cosim), shape = 16, size = 3)+
  geom_line(aes(x = k, y = slda_cosim),color = "#757fa0", linetype = "solid") +
  geom_point(aes(x = k, y = slda_cosim),shape = 16, size = 3,color = "#757fa0")+
  geom_line( aes(x = k, y = Durslda_cosim), color = "#DDacaa", linetype = "solid") +  
  geom_point( aes(x = k, y = Durslda_cosim), shape = 16, size = 3, color = "#DDacaa")+
  geom_line(aes(x = k, y = FPslda_cosim), color = "#F2a869", linetype = "solid") +  # Dursdf 数据的线条
  geom_point( aes(x = k, y = FPslda_cosim), shape = 16, size = 3, color = "#F2a869")+
  geom_line( aes(x = k, y = FAslda_cosim), color = "#b783bd", linetype = "solid") +  # Dursdf 数据的线条
  geom_point(aes(x = k, y = FAslda_cosim), shape = 16, size = 3, color = "#b783bd")+
  labs(x = "Number of Topics", y = "Cosim") +
  theme_minimal() +
  scale_color_manual(values = c("lda" = "black", "slda" = "#757fa0", "Durslda" = "#DDacaa", "FPslda" = "#F2a869", "FAslda" ="#b783bd")) +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  )


logratioPlot<-logratioPlot+labs(x=NULL)
pmiPlot<-pmiPlot+labs(x=NULL)
combined_Coher <- (logratioPlot + pmiPlot) / (differencePlot + cosimPlot) +plot_layout(guides = "collect")+ 
  plot_annotation(title = "Coherence Metrics in Different Topic Models")&
  theme(plot.title= element_text(face = "bold",size=16))


ggsave("Coherence.pdf", combined_Coher)

